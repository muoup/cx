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
    peek_next_kind,
    preparse::{naive_types::CXNaivePrototype, templates::CXTemplatePrototype},
    try_next, PreparseContents,
};
use cx_util::{identifier::CXIdent, CXResult};

use crate::parse::{
    expressions::{expression_requires_semicolon, parse_expr}, functions::{parse_destructor_prototype, try_function_parse}, templates::{note_templated_types, parse_template_prototype, unnote_templated_types}, types::parse_initializer
};

mod expressions;
mod functions;
mod operators;
mod templates;
mod types;

pub fn parse_ast(iter: TokenIter, pp_contents: &PreparseContents) -> Option<CXAST> {
    let mut data = ParserData {
        ast: CXAST {
            imports: pp_contents.imports.clone(),
            ..Default::default()
        },
        pp_contents,
        tokens: iter,
        visibility: VisibilityMode::Package,
        expr_commas: vec![true],
    };

    while data.tokens.has_next() {
        parse_global_stmt(&mut data)?;
    }

    Some(data.ast)
}

fn parse_global_stmt(data: &mut ParserData) -> CXResult<()> {
    match data
        .tokens
        .peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind
    {
        keyword!(Import) => data.tokens.goto_statement_end()?,
        keyword!(Typedef) => parse_typedef(data)?,
        punctuator!(Semicolon) => data.tokens.next().map(|_| ())?,
        specifier!() => parse_access_mods(data)?,

        operator!(Tilda) => {
            let destructor = parse_destructor_prototype(data)?;
            parse_fn_merge(data, destructor.prototype, destructor.template_prototype)?;
        }

        _ => parse_global_expr(data)?,
    };

    Some(())
}

fn parse_access_mods(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => data.visibility = VisibilityMode::Public,
        SpecifierType::Private => data.visibility = VisibilityMode::Private,

        _ => log_parse_error!(data, "Unexpected specifier in global scope"),
    };

    try_next!(data.tokens, punctuator!(Colon));

    Some(())
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef));
    let start_index = data.tokens.index;

    let template_prototype = if peek_next_kind!(data.tokens) == Some(&operator!(Less)) {
        parse_template_prototype(&mut data.tokens)
    } else {
        None
    };

    let Some((name, type_)) = parse_initializer(data) else {
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

    data.add_type(name.as_string(), type_, template_prototype);
    Some(())
}

fn parse_fn_merge(
    data: &mut ParserData,
    prototype: CXNaivePrototype,
    template_prototype: Option<CXTemplatePrototype>,
) -> CXResult<()> {
    if try_next!(data.tokens, punctuator!(Semicolon)) {
        if template_prototype.is_some() {
            log_parse_error!(data, "Templated functions must be defined in place.");
        }

        data.add_function(prototype, None);
    } else {
        match template_prototype {
            Some(template_prototype) => {
                note_templated_types(data, &template_prototype);
                let body = parse_body(data)?;
                unnote_templated_types(data, &template_prototype);
                
                data.add_function(prototype.clone(), Some(template_prototype));
                data.ast.global_stmts.push(
                    CXGlobalStmt::TemplatedFunction {
                        prototype: prototype,
                        body: Box::new(body),
                    }
                );
            }

            None => {
                let body = parse_body(data)?;
                
                data.add_function(prototype.clone(), None);
                data.ast.global_stmts.push(
                    CXGlobalStmt::FunctionDefinition {
                        prototype: prototype,
                        body: Box::new(body),
                    }
                );
            }
        }
    }
    
    Some(())
}

fn parse_global_expr(data: &mut ParserData) -> CXResult<()> {
    let Some((name, return_type)) = parse_initializer(data) else {
        log_parse_error!(data, "Failed to parse initializer in global expression!");
    };

    let Some(name) = name else {
        // Blank statement consisting on just a type, (i.e. struct [name] { [fields] };)
        
        assert_token_matches!(data.tokens, punctuator!(Semicolon));
        return Some(());
    };
    
    if !data.tokens.has_next() {
        log_parse_error!(
            data,
            "Reached end of token stream when parsing global expression!"
        );
    }

    if let Some(func) = try_function_parse(data, return_type.clone(), name.clone())? {
        return parse_fn_merge(data, func.prototype, func.template_prototype);
    }

    match next_kind!(data.tokens) {
        Some(TokenKind::Assignment(_)) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Semicolon));
            data.ast.global_stmts.push(
                CXGlobalStmt::GlobalVariable {
                    name,
                    type_: return_type,
                    initializer: Some(initial_value),
                }
            );
        }

        Some(punctuator!(Semicolon)) => {
            data.ast.global_stmts.push(
                CXGlobalStmt::GlobalVariable {
                    name,
                    type_: return_type,
                    initializer: None,
                }
            );
        },

        _ => log_parse_error!(
            data,
            "Unexpected token in global expression: {:#?}",
            data.tokens.peek()
        ),
    }
    
    Some(())
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

pub fn parse_intrinsic(tokens: &mut TokenIter) -> Option<CXIdent> {
    let mut ss = String::new();

    while let Some(TokenKind::Intrinsic(ident)) = peek_next_kind!(tokens) {
        ss.push_str(format!("{ident:?}").to_lowercase().as_str());
        tokens.next();
    }

    if ss.is_empty() {
        return None;
    }

    Some(CXIdent::from(ss))
}

pub fn parse_std_ident(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };

    tokens.next();

    Some(CXIdent::from(ident))
}
