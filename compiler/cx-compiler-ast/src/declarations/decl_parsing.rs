use cx_data_ast::{
    assert_token_matches,
    parse::parser::VisibilityMode,
    peek_next_kind,
    preparse::naive_types::{CXNaiveTypeKind, PredeclarationType},
    try_next,
};
use cx_data_lexer::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_util::{log_error, CXResult};

use crate::{
    declarations::{
        data_parsing::parse_template_prototype,
        function_parsing::{parse_destructor_prototype, try_function_parse},
        type_parsing::{parse_enum, parse_initializer, parse_struct, parse_union},
        TypeDeclaration,
    },
    preparse::PreparseData,
};

pub(crate) fn preparse_decl_stmt(data: &mut PreparseData) -> CXResult<()> {
    let Some(next_token) = data.tokens.peek() else {
        return Some(());
    };

    match &next_token.kind {
        keyword!(Import) => {
            let import = parse_import(&mut data.tokens)?;
            data.contents.imports.push(import);
        }

        keyword!(Struct, Enum, Union) => parse_plain_typedef(data)?,
        keyword!(Typedef) => parse_typedef(data)?,
        
        operator!(Tilda) => {
            parse_destructor_prototype(&mut data.tokens)?.add_to(data);
            data.tokens.goto_statement_end()?;
            
        },

        specifier!(Public) => {
            data.tokens.next();
            data.visibility_mode = VisibilityMode::Public;
            try_next!(data.tokens, punctuator!(Colon));
        }

        specifier!(Private) => {
            data.tokens.next();
            data.visibility_mode = VisibilityMode::Private;
            try_next!(data.tokens, punctuator!(Colon));
        }

        punctuator!(Semicolon) => {
            data.tokens.next();
        }

        _ => preparse_global_expr(data)?,
    }

    Some(())
}

pub(crate) fn parse_import(tokens: &mut TokenIter) -> CXResult<String> {
    assert_token_matches!(tokens, keyword!(Import));

    let mut import_path = String::new();

    loop {
        let Some(tok) = tokens.next() else {
            log_preparse_error!(tokens, "Reached end of token stream when parsing import!");
        };

        match &tok.kind {
            punctuator!(Semicolon) => break,
            operator!(ScopeRes) => import_path.push('/'),
            identifier!(ident) => import_path.push_str(ident),

            _ => log_error!("Reached invalid token in import path: {:?}", tok),
        }
    }

    Some(import_path)
}

pub(crate) fn parse_plain_typedef(data: &mut PreparseData) -> CXResult<()> {
    let starting_index = data.tokens.index;

    let Some(decl_start) = peek_next_kind!(data.tokens) else {
        panic!("EOF Reached")
    };

    let decl = match decl_start {
        keyword!(Struct) => parse_struct(&mut data.tokens)?,
        keyword!(Enum) => parse_enum(&mut data.tokens)?,
        keyword!(Union) => parse_union(&mut data.tokens)?,

        tok => todo!("parse_plain_typedef: {tok:?}"),
    };

    match (&decl.name, &decl.type_) {
        (Some(_), type_)
            if matches!(type_.kind, CXNaiveTypeKind::Identifier { predeclaration, .. }
                if predeclaration != PredeclarationType::None) =>
        {
            // If we reach some `struct [name]` `enum [name]` or `union [name]`
            // that is actually used either to declare a variable or a function,
            // we can't quite differentiate it from a non-typedef declaration
            // until we realize it isn't one, so we have to backtrack and try parsing
            // it as a full expression.

            data.tokens.index = starting_index;
            return preparse_global_expr(data);
        }

        _ => decl.add_to(data),
    }

    assert_token_matches!(data.tokens, punctuator!(Semicolon));

    Some(())
}

pub(crate) fn parse_typedef(data: &mut PreparseData) -> CXResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef));
    let start_index = data.tokens.index;

    let template_prototype = if peek_next_kind!(data.tokens) == Some(&operator!(Less)) {
        parse_template_prototype(&mut data.tokens)
    } else {
        None
    };

    let Some((name, type_)) = parse_initializer(&mut data.tokens) else {
        log_preparse_error!(
            data.tokens.with_index(start_index),
            "Could not parse typedef."
        );
    };

    let Some(name) = name else {
        log_preparse_error!(
            data.tokens.with_index(start_index),
            "Typedef must have a name!"
        );
    };

    assert_token_matches!(data.tokens, punctuator!(Semicolon));

    TypeDeclaration {
        name: Some(name.clone()),
        type_,
        template_prototype,
    }
    .add_to(data);

    Some(())
}

pub(crate) fn preparse_global_expr(data: &mut PreparseData) -> CXResult<()> {
    let Some((name, return_type)) = parse_initializer(&mut data.tokens) else {
        log_preparse_error!(data.tokens, "Could not parse type for global expression!");
    };

    let Some(name) = name else {
        log_preparse_error!(data.tokens, "Invalid global expression, name not found!");
    };

    let Some(method) = try_function_parse(&mut data.tokens, return_type, name.clone()).flatten()
    else {
        // Global variables are parsed during the full parse, variable identifiers are not
        // at least currently, needed in the process of full parsing an AST like type names are.

        return data.tokens.goto_statement_end();
    };

    data.tokens.goto_statement_end()?;

    method.add_to(data);
    Some(())
}
