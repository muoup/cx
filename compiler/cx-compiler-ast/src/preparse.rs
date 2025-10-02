use cx_data_ast::{
    assert_token_matches, parse::parser::VisibilityMode, preparse::naive_types::{CXNaiveTypeKind, ModuleResource, PredeclarationType},
    try_next, PreparseContents,
};
use cx_data_lexer::{keyword, operator, punctuator, specifier, identifier, TokenIter};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{
    declarations::{
        decl_parsing::{parse_import, parse_plain_typedef, parse_typedef, try_function_parse},
        type_parsing::parse_initializer,
    },
    definitions::global_scope::destructor_prototype,
    parsing_tools::goto_statement_end,
};

pub(crate) struct PreparseData<'a> {
    pub(crate) contents: &'a mut PreparseContents,
    pub(crate) tokens: TokenIter<'a>,
    pub(crate) visibility_mode: VisibilityMode,
}

pub fn preparse(tokens: TokenIter) -> Option<PreparseContents> {
    let mut contents = PreparseContents::default();

    let mut data = PreparseData {
        contents: &mut contents,
        tokens,
        visibility_mode: VisibilityMode::Private,
    };

    while data.tokens.has_next() {
        let Some(_) = preparse_stmt(&mut data) else {
            log_preparse_error!(data.tokens, "Failed to preparse statement")
        };
    }

    Some(contents)
}

pub(crate) fn preparse_stmt(data: &mut PreparseData) -> CXResult<()> {
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
            data.tokens.next();
            assert_token_matches!(data.tokens, identifier!(destructor_type_name));
            let destructor_type = CXNaiveTypeKind::Identifier {
                name: CXIdent::from(destructor_type_name.as_str()),
                predeclaration: PredeclarationType::None,
            }.to_type();
            
            let prototype = destructor_prototype(destructor_type);
            data.contents.function_definitions.insert_standard(
                prototype.name.mangle(),
                ModuleResource::with_visibility(prototype, data.visibility_mode),
            );

            goto_statement_end(&mut data.tokens);
        }

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

        goto_statement_end(&mut data.tokens);
        return Some(());
    };

    goto_statement_end(&mut data.tokens);

    data.contents.function_definitions.insert_standard(
        name.as_string(),
        ModuleResource::with_visibility(method, data.visibility_mode),
    );

    Some(())
}
