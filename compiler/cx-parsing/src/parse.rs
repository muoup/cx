use cx_ast::ast::{
    CXASTStmt, expression::{CXExprKind, CXExpression}, function::CXFunctionPrototype, global_var::CXGlobalVariable, modifiers::CXLinkageMode, template::CXTemplatePrototype
};
use cx_preparse_data::VisibilityMode;
use cx_tokens::{
    keyword, operator, punctuator, specifier,
    token::{SpecifierType, TokenKind},
    TokenIter,
};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{
    assert_token_matches, next_kind,
    parse::{
        expressions::{expression_requires_semicolon, parse_expr},
        functions::try_function_parse,
        parser::ParserData,
        templates::{note_templated_types, parse_template_prototype, unnote_templated_types},
        types::{parse_initializer, parse_typedef_initializer},
    },
    peek_next_kind, try_next,
};

pub(crate) mod parser;

mod expressions;
mod functions;
mod identifier;
mod operators;
mod templates;
mod types;

pub(crate) use identifier::{try_parse_identifier, try_parse_qualified_name};

pub fn parse_global_stmt(data: &mut ParserData) -> CXResult<()> {
    match data
        .tokens
        .peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind
    {
        keyword!(Import) => data.tokens.goto_statement_end()?,
        keyword!(Typedef) => parse_typedef(data)?,
        punctuator!(Semicolon) => {
            data.tokens.next();
        }
        specifier!(Public) | specifier!(Private) => parse_access_mods(data)?,
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

pub(crate) fn parse_typedef(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef), "'typedef'");
    let start_index = data.tokens.index;

    let template_prototype = if matches!(peek_next_kind!(data.tokens)?, operator!(Less)) {
        Some(parse_template_prototype(&mut data.tokens)?)
    } else {
        None
    };

    let (name, _type) = parse_typedef_initializer(data)?;

    let Some(name) = name else {
        return log_preparse_error!(
            data.tokens.with_index(start_index),
            "Typedef must have a name!"
        );
    };

    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    data.add_stmt(CXASTStmt::TypeDefinition {
        name: Some(name),
        visibility: data.visibility,
        _type: _type.clone(),
        template_prototype: template_prototype.clone(),
    });

    Ok(())
}

fn parse_fn_merge(
    data: &mut ParserData,
    prototype: CXFunctionPrototype,
    template_prototype: Option<CXTemplatePrototype>,
) -> CXResult<()> {
    if try_next!(data.tokens, punctuator!(Semicolon)) {
        if template_prototype.is_some() {
            return log_parse_error!(data, "Templated functions must be defined in place.");
        }

        data.add_stmt(CXASTStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            template_prototype: None,
            body: None,
        });
    } else {
        let body = if let Some(template_prototype) = template_prototype.as_ref() {
            note_templated_types(data, &template_prototype)?;
            let body = parse_body(data);
            unnote_templated_types(data, &template_prototype);
            body
        } else {
            parse_body(data)
        }?;

        data.add_stmt(CXASTStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            body: Some(Box::new(body)),
            template_prototype,
        });
    }

    Ok(())
}

fn parse_global_expr(data: &mut ParserData) -> CXResult<()> {
    let (name, return_type, linkage) = parse_initializer(data)?;

    let Some(name) = name else {
        // Blank statement consisting on just a type, (i.e. struct [name] { [fields] };)

        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
        return Ok(());
    };

    if !data.tokens.has_next() {
        return log_parse_error!(
            data,
            "Reached end of token stream when parsing global expression!"
        );
    }

    if let Some(func) = try_function_parse(data, return_type.clone(), name.clone(), linkage)? {
        return parse_fn_merge(data, func.prototype, func.template_prototype);
    }

    match next_kind!(data.tokens)? {
        TokenKind::Assignment(_) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
            data.add_stmt(CXASTStmt::GlobalVariableDefinition {
                name: name.clone(),
                visibility: data.visibility,
                variable: CXGlobalVariable::Standard {
                    _type: return_type.clone(),
                    is_mutable: true,
                    linkage: CXLinkageMode::Standard,
                    initializer: Some(initial_value.clone()),
                },
            });
        }

        punctuator!(Semicolon) => {
            data.add_stmt(CXASTStmt::GlobalVariableDefinition {
                name: name.clone(),
                visibility: data.visibility,
                variable: CXGlobalVariable::Standard {
                    _type: return_type.clone(),
                    is_mutable: true,
                    linkage: CXLinkageMode::Standard,
                    initializer: None,
                },
            });
        }

        _ => {
            return log_parse_error!(
                data,
                "Unexpected token in global expression: {:#?}",
                data.tokens.peek()
            );
        }
    }

    Ok(())
}

fn parse_body(data: &mut ParserData) -> CXResult<CXExpression> {
    if try_next!(data.tokens, punctuator!(OpenBrace)) {
        let start_index = data.tokens.index - 1;
        let mut body = Vec::new();

        while !try_next!(data.tokens, punctuator!(CloseBrace)) {
            let stmt = parse_expr(data)?;

            if expression_requires_semicolon(&stmt) {
                assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
            }

            body.push(stmt);
        }

        Ok(CXExprKind::Block { exprs: body }.into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        ))
    } else {
        let body = parse_expr(data)?;

        if expression_requires_semicolon(&body) {
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
        }

        Ok(body)
    }
}

pub fn parse_intrinsic(tokens: &mut TokenIter) -> CXResult<CXIdent> {
    let mut ss = String::new();

    while let Ok(TokenKind::Intrinsic(ident)) = peek_next_kind!(tokens) {
        ss.push_str(ident.as_str());
        ss.push(' ');
        tokens.next();
    }

    if ss.is_empty() {
        return log_preparse_error!(tokens, "Expected intrinsic identifier");
    }

    ss.pop();

    Ok(CXIdent::new(ss))
}

pub fn try_parse_simple_identifier(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().map(|token| &token.kind)? else {
        return None;
    };
    let ident = CXIdent::new(ident.clone());
    tokens.next();
    Some(ident)
}
