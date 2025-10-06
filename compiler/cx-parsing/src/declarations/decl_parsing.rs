use cx_lexer_data::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_parsing_data::{
    assert_token_matches,
    parse::parser::VisibilityMode,
    peek_next_kind,
    preparse::naive_types::{CXNaiveTypeKind, PredeclarationType},
    try_next,
};
use cx_util::{log_error, CXResult};

use crate::declarations::{
    data_parsing::parse_template_prototype,
    function_parsing::{parse_destructor_prototype, try_function_parse},
    type_parsing::{parse_enum, parse_initializer, parse_struct, parse_union},
    DeclarationStatement, TypeDeclaration,
};

pub(crate) fn preparse_decl_stmt(tokens: &mut TokenIter) -> CXResult<DeclarationStatement> {
    let Some(next_token) = tokens.peek() else {
        return Some(DeclarationStatement::None);
    };

    Some(match &next_token.kind {
        keyword!(Import) => DeclarationStatement::Import(parse_import(tokens)?),

        keyword!(Struct, Enum, Union) => parse_plain_typedef(tokens)?,
        keyword!(Typedef) => DeclarationStatement::TypeDeclaration(parse_typedef(tokens)?),

        operator!(Tilda) => {
            let prototype = parse_destructor_prototype(tokens)?;
            tokens.goto_statement_end()?;
            DeclarationStatement::FunctionDeclaration(prototype)
        }

        specifier!(Public) => {
            tokens.next();
            try_next!(tokens, punctuator!(Colon));
            DeclarationStatement::ChangeVisibility(VisibilityMode::Public)
        }

        specifier!(Private) => {
            tokens.next();
            try_next!(tokens, punctuator!(Colon));
            DeclarationStatement::ChangeVisibility(VisibilityMode::Private)
        }

        punctuator!(Semicolon) => {
            tokens.next();
            DeclarationStatement::None
        }

        _ => preparse_global_expr(tokens)?,
    })
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

pub(crate) fn parse_plain_typedef(tokens: &mut TokenIter) -> CXResult<DeclarationStatement> {
    let starting_index = tokens.index;

    let Some(decl_start) = peek_next_kind!(tokens) else {
        panic!("EOF Reached")
    };

    let decl = match decl_start {
        keyword!(Struct) => parse_struct(tokens)?,
        keyword!(Enum) => parse_enum(tokens)?,
        keyword!(Union) => parse_union(tokens)?,

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

            tokens.index = starting_index;
            preparse_global_expr(tokens)
        }

        _ => {
            assert_token_matches!(tokens, punctuator!(Semicolon));
            Some(DeclarationStatement::TypeDeclaration(decl))
        }
    }
}

pub(crate) fn parse_typedef(tokens: &mut TokenIter) -> CXResult<TypeDeclaration> {
    assert_token_matches!(tokens, keyword!(Typedef));
    let start_index = tokens.index;

    let template_prototype = if peek_next_kind!(tokens) == Some(&operator!(Less)) {
        parse_template_prototype(tokens)
    } else {
        None
    };

    let Some((name, type_)) = parse_initializer(tokens) else {
        log_preparse_error!(tokens.with_index(start_index), "Could not parse typedef.");
    };

    let Some(name) = name else {
        log_preparse_error!(tokens.with_index(start_index), "Typedef must have a name!");
    };

    assert_token_matches!(tokens, punctuator!(Semicolon));

    Some(TypeDeclaration {
        name: Some(name.clone()),
        type_,
        template_prototype,
    })
}

pub(crate) fn preparse_global_expr(tokens: &mut TokenIter) -> CXResult<DeclarationStatement> {
    let Some((name, return_type)) = parse_initializer(tokens) else {
        log_preparse_error!(tokens, "Could not parse type for global expression!");
    };

    let Some(name) = name else {
        log_preparse_error!(tokens, "Invalid global expression, name not found!");
    };

    let Some(method) = try_function_parse(tokens, return_type, name.clone()).flatten() else {
        // Global variables are parsed during the full parse, variable identifiers are not
        // at least currently, needed in the process of full parsing an AST like type names are.

        tokens.goto_statement_end();

        return Some(DeclarationStatement::None);
    };

    tokens.goto_statement_end()?;
    Some(DeclarationStatement::FunctionDeclaration(method))
}
