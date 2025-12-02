use cx_lexer_data::{
    keyword, operator, punctuator, specifier,
    token::{SpecifierType, TokenKind},
    TokenIter,
};
use cx_parsing_data::{
    assert_token_matches,
    ast::VisibilityMode,
    ast::{CXExpr, CXExprKind, CXFunctionStmt, CXGlobalVariable, CXAST},
    data::{CXNaivePrototype, CXTemplatePrototype},
    next_kind, peek_next_kind, try_next, PreparseContents,
};
use cx_util::{CXResult, identifier::CXIdent};

use crate::parse::{
    expressions::{expression_requires_semicolon, parse_expr},
    functions::{parse_destructor_prototype, try_function_parse},
    parser::ParserData,
    templates::{note_templatedtype_s, parse_template_prototype, unnote_templatedtype_s},
    types::parse_initializer,
};

mod expressions;
mod functions;
mod operators;
mod parser;
mod templates;
mod types;

pub fn parse_ast(iter: TokenIter, pp_contents: &PreparseContents) -> CXResult<CXAST> {
    let mut data = ParserData::new(iter, pp_contents);

    while data.tokens.has_next() {
        parse_global_stmt(&mut data)?;
    }

    Ok(data.take_ast())
}

fn parse_global_stmt(data: &mut ParserData) -> CXResult<()> {
    match data
        .tokens
        .peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind
    {
        keyword!(Import) => data.tokens.goto_statement_end()?,
        keyword!(Typedef) => parsetype_def(data)?,
        punctuator!(Semicolon) => {
            data.tokens.next();
        }
        specifier!() => parse_access_mods(data)?,

        operator!(Tilda) => {
            let destructor = parse_destructor_prototype(data)?;
            parse_fn_merge(data, destructor.prototype, destructor.template_prototype)?;
        }

        _ => parse_global_expr(data)?,
    };

    Ok(())
}

fn parse_access_mods(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => data.visibility = VisibilityMode::Public,
        SpecifierType::Private => data.visibility = VisibilityMode::Private,

        _ => return log_parse_error!(data, "Unexpected specifier in global scope"),
    };

    try_next!(data.tokens, punctuator!(Colon));

    Ok(())
}

pub(crate) fn parsetype_def(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef));
    let start_index = data.tokens.index;

    let template_prototype = if matches!(peek_next_kind!(data.tokens)?, operator!(Less)) {
        Some(parse_template_prototype(&mut data.tokens)?)
    } else {
        None
    };

    let (name, _type) = parse_initializer(data)?;

    let Some(name) = name else {
        return log_preparse_error!(
            data.tokens.with_index(start_index),
            "Typedef must have a name!"
        );
    };

    assert_token_matches!(data.tokens, punctuator!(Semicolon));

    data.add_type(name.as_string(), _type, template_prototype);
    Ok(())
}

fn parse_fn_merge(
    data: &mut ParserData,
    prototype: CXNaivePrototype,
    template_prototype: Option<CXTemplatePrototype>,
) -> CXResult<()> {
    if try_next!(data.tokens, punctuator!(Semicolon)) {
        if template_prototype.is_some() {
            return log_parse_error!(data, "Templated functions must be defined in place.");
        }

        data.add_function(prototype, None);
    } else {
        match template_prototype {
            Some(template_prototype) => {
                note_templatedtype_s(data, &template_prototype);
                let body = parse_body(data)?;
                unnote_templatedtype_s(data, &template_prototype);

                data.add_function(prototype.clone(), Some(template_prototype));
                data.add_function_stmt(CXFunctionStmt::TemplatedFunction {
                    prototype,
                    body: Box::new(body),
                });
            }

            None => {
                let body = parse_body(data)?;

                data.add_function(prototype.clone(), None);
                data.add_function_stmt(CXFunctionStmt::FunctionDefinition {
                    prototype,
                    body: Box::new(body),
                });
            }
        }
    }

    Ok(())
}

fn parse_global_expr(data: &mut ParserData) -> CXResult<()> {
    let (name, return_type) = parse_initializer(data)?;

    let Some(name) = name else {
        // Blank statement consisting on just a type, (i.e. struct [name] { [fields] };)

        assert_token_matches!(data.tokens, punctuator!(Semicolon));
        return Ok(());
    };

    if !data.tokens.has_next() {
        return log_parse_error!(
            data,
            "Reached end of token stream when parsing global expression!"
        );
    }

    if let Some(func) = try_function_parse(data, return_type.clone(), name.clone())? {
        return parse_fn_merge(data, func.prototype, func.template_prototype);
    }

    match next_kind!(data.tokens)? {
        TokenKind::Assignment(_) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Semicolon));
            data.add_global_variable(
                name.as_string(),
                CXGlobalVariable::Standard {
                    _type: return_type,
                    is_mutable: true,
                    initializer: Some(initial_value),
                },
            );
        }

        punctuator!(Semicolon) => {
            data.add_global_variable(
                name.as_string(),
                CXGlobalVariable::Standard {
                    _type: return_type,
                    is_mutable: true,
                    initializer: None,
                },
            );
        }

        _ => return log_parse_error!(
            data,
            "Unexpected token in global expression: {:#?}",
            data.tokens.peek()
        ),
    }

    Ok(())
}

fn parse_body(data: &mut ParserData) -> CXResult<CXExpr> {
    if try_next!(data.tokens, punctuator!(OpenBrace)) {
        let start_index = data.tokens.index - 1;
        let mut body = Vec::new();

        while !try_next!(data.tokens, punctuator!(CloseBrace)) {
            let Ok(stmt) = parse_expr(data) else {
                return log_parse_error!(
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

        Ok(CXExprKind::Block { exprs: body }.into_expr(start_index, data.tokens.index))
    } else {
        let body = parse_expr(data)?;

        if expression_requires_semicolon(&body) {
            assert_token_matches!(data.tokens, punctuator!(Semicolon));
        }

        Ok(body)
    }
}

pub fn parse_intrinsic(tokens: &mut TokenIter) -> CXResult<CXIdent> {
    let mut ss = String::new();

    while let Some(TokenKind::Intrinsic(ident)) = peek_next_kind!(tokens).ok() {
        ss.push_str(format!("{ident:?}").to_lowercase().as_str());
        tokens.next();
    }

    if ss.is_empty() {
        return log_preparse_error!(tokens, "Expected intrinsic identifier"); 
    }

    Ok(CXIdent::new(ss))
}

pub fn parse_std_ident(tokens: &mut TokenIter) -> CXResult<CXIdent> {
    let TokenKind::Identifier(ident) = peek_next_kind!(tokens)? else {
        return log_preparse_error!(tokens, "Expected standard identifier");
    };

    let ident = ident.clone();
    
    tokens.next();

    Ok(CXIdent::new(ident))
}
