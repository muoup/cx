use cx_lexer_data::{
    keyword, operator, punctuator, specifier,
    token::{SpecifierType, TokenKind},
    TokenIter,
};
use cx_parsing_data::{
    assert_token_matches, next_kind,
    parse::{
        ast::{CXExpr, CXExprKind, CXGlobalStmt, CXAST},
        parser::{ParserData, VisibilityMode},
    },
    preparse::{
        naive_types::{CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, PredeclarationType},
        FunctionTypeIdent, NaiveFnIdent,
    },
    try_next, PreparseContents,
};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{
    declarations::{decl_parsing::parse_typedef, FunctionDeclaration},
    parse::{
        expressions::{expression_requires_semicolon, parse_expr},
        functions::{parse_destructor_prototype, try_function_parse},
        templates::{note_templated_types, unnote_templated_types},
        types::parse_initializer,
    },
};

mod expressions;
mod functions;
mod operators;
mod templates;
mod types;

pub fn parse_ast(iter: TokenIter, pp_contents: &PreparseContents) -> Option<CXAST> {
    let mut data = ParserData {
        ast: CXAST::default(),
        pp_contents,
        tokens: iter,
        visibility: VisibilityMode::Package,
        expr_commas: vec![true],
    };

    while data.tokens.has_next() {
        if let Some(expr) = parse_global_stmt(&mut data)? {
            data.ast.global_stmts.push(expr);
        }
    }

    Some(data.ast)
}

fn parse_global_stmt(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    match data
        .tokens
        .peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind
    {
        keyword!(Import) => {
            data.tokens.goto_statement_end();
            Some(None)
        }

        keyword!(Typedef) => {
            parse_typedef(&mut data.tokens)?.add_to_map(&mut data.ast.type_map, data.visibility);

            Some(None)
        }

        punctuator!(Semicolon) => {
            data.tokens.next();
            Some(None)
        }

        keyword!(Struct) => todo!(), //parse_struct(data),
        keyword!(Enum) => todo!(),   //parse_enum(data),
        specifier!() => parse_access_mods(data),

        operator!(Tilda) => {
            let destructor = parse_destructor_prototype(&mut data.tokens)?;
            parse_fn_merge(data, destructor)
        }

        _ => parse_global_expr(data),
    }
}

fn parse_access_mods(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => data.visibility = VisibilityMode::Public,
        SpecifierType::Private => data.visibility = VisibilityMode::Private,

        _ => log_parse_error!(data, "Unexpected specifier in global scope"),
    };

    try_next!(data.tokens, punctuator!(Colon));

    Some(None)
}

fn parse_fn_merge(
    data: &mut ParserData,
    prototype: FunctionDeclaration,
) -> CXResult<Option<CXGlobalStmt>> {
    if try_next!(data.tokens, punctuator!(Semicolon)) {
        if prototype.template_prototype.is_some() {
            log_parse_error!(data, "Templated functions must be defined in place.");
        }

        Some(Some(CXGlobalStmt::FunctionPrototype {
            prototype: prototype.prototype,
        }))
    } else {
        match &prototype.template_prototype {
            Some(template_prototype) => {
                note_templated_types(data, template_prototype);
                let body = parse_body(data)?;
                unnote_templated_types(data, template_prototype);

                Some(Some(CXGlobalStmt::TemplatedFunction {
                    prototype: prototype.prototype,
                    body: Box::new(body),
                }))
            }

            None => {
                let body = parse_body(data)?;

                Some(Some(CXGlobalStmt::FunctionDefinition {
                    prototype: prototype.prototype,
                    body: Box::new(body),
                }))
            }
        }
    }
}

fn parse_global_expr(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    let Some((Some(name), return_type)) = parse_initializer(&mut data.tokens) else {
        log_parse_error!(data, "Failed to parse initializer in global expression!");
    };

    if !data.tokens.has_next() {
        log_parse_error!(
            data,
            "Reached end of token stream when parsing global expression!"
        );
    }

    if let Some(func) = try_function_parse(&mut data.tokens, return_type.clone(), name.clone())? {
        return parse_fn_merge(data, func);
    }

    match next_kind!(data.tokens) {
        Some(TokenKind::Assignment(_)) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Semicolon));

            Some(Some(CXGlobalStmt::GlobalVariable {
                name,
                type_: return_type,
                initializer: Some(initial_value),
            }))
        }

        Some(punctuator!(Semicolon)) => Some(Some(CXGlobalStmt::GlobalVariable {
            name,
            type_: return_type,
            initializer: None,
        })),

        _ => log_parse_error!(
            data,
            "Unexpected token in global expression: {:#?}",
            data.tokens.peek()
        ),
    }
}

fn parse_body(data: &mut ParserData) -> Option<CXExpr> {
    if try_next!(data.tokens, punctuator!(OpenBrace)) {
        let start_index = data.tokens.index - 1;
        let mut body = Vec::new();

        while !try_next!(data.tokens, punctuator!(CloseBrace)) {
            let Some(stmt) = parse_expr(data) else {
                log_parse_error!(
                    data,
                    "Failed to parse statement in body: {:#?}",
                    data.tokens.peek()
                );
            };

            if expression_requires_semicolon(&stmt) {
                assert_token_matches!(data.tokens, punctuator!(Semicolon));
            }

            body.push(stmt);
        }

        Some(CXExprKind::Block { exprs: body }.into_expr(start_index, data.tokens.index))
    } else {
        let body = parse_expr(data)?;

        if expression_requires_semicolon(&body) {
            assert_token_matches!(data.tokens, punctuator!(Semicolon));
        }

        Some(body)
    }
}
