use cx_ast::{
    assert_token_matches,
    ast::VisibilityMode,
    ast::{CXExprKind, CXExpression, CXFunctionStmt, CXGlobalVariable, CXAST},
    data::{
        CXFunctionKind, CXFunctionPrototype, CXFunctionTypeIdent, CXTemplatePrototype, CXTypeKind,
    },
    next_kind, peek_next_kind, try_next,
};
use cx_preparse_data::{registry::GlobalPreparseRegistry, PreparseContents};
use cx_tokens::{
    keyword, operator, punctuator, specifier,
    token::{SpecifierType, TokenKind},
    TokenIter,
};
use cx_util::{CXResult, identifier::CXIdent, namespace::{NamespacePath, QualifiedName}};

use crate::parse::{
    expressions::{expression_requires_semicolon, parse_expr},
    functions::try_function_parse,
    parser::ParserData,
    templates::{note_templated_types, parse_template_prototype, unnote_templated_types},
    types::{parse_initializer, parse_typedef_initializer},
};

fn active_body_template_prototype(
    prototype: &CXFunctionPrototype,
    function_template: Option<&CXTemplatePrototype>,
) -> Option<CXTemplatePrototype> {
    let mut types = Vec::new();

    let member_type = match &prototype.kind {
        CXFunctionKind::MemberFunction { member_type, .. }
        | CXFunctionKind::StaticMemberFunction { member_type, .. } => Some(member_type),
        CXFunctionKind::Standard(_) => None,
    };

    if let Some(CXFunctionTypeIdent::Templated(_, input)) = member_type {
        for param in &input.params {
            if let CXTypeKind::Identifier { name, .. } = &param.kind {
                let name = name.name.as_string();
                if !types.contains(&name) {
                    types.push(name);
                }
            }
        }
    }

    if let Some(function_template) = function_template {
        for name in &function_template.types {
            if !types.contains(name) {
                types.push(name.clone());
            }
        }
    }

    (!types.is_empty()).then_some(CXTemplatePrototype { types })
}

mod expressions;
mod functions;
mod operators;
mod parser;
mod templates;
mod types;

pub fn parse_ast(
    iter: TokenIter,
    pp_contents: &PreparseContents,
    registry: &GlobalPreparseRegistry,
) -> CXResult<CXAST> {
    let mut data = ParserData::new(iter, pp_contents, registry);

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

    if let CXTypeKind::Identifier {
        name: type_name, ..
    } = &_type.kind
    {
        let existing_complete_aggregate = data
            .ast
            .type_data
            .get_standard(&name.as_string())
            .is_some_and(|existing| {
                matches!(
                    existing.resource.kind,
                    CXTypeKind::Structured { .. }
                        | CXTypeKind::Union { .. }
                        | CXTypeKind::TaggedUnion { .. }
                )
            });

        if type_name.namespace.is_root() && type_name.name == name && existing_complete_aggregate {
            return Ok(());
        }
    }

    data.add_type(name.as_string(), _type, template_prototype);
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

        data.add_function(prototype, None);
    } else {
        let body_template = active_body_template_prototype(&prototype, template_prototype.as_ref());

        match template_prototype {
            Some(template_prototype) => {
                if let Some(body_template) = &body_template {
                    note_templated_types(data, body_template);
                }
                let body = parse_body(data)?;
                if let Some(body_template) = &body_template {
                    unnote_templated_types(data, body_template);
                }

                data.add_function(prototype.clone(), Some(template_prototype.clone()));
                data.add_function_stmt(CXFunctionStmt::TemplatedFunction {
                    prototype,
                    template_prototype,
                    body: Box::new(body),
                });
            }

            None => {
                if let Some(body_template) = &body_template {
                    note_templated_types(data, body_template);
                }
                let body = parse_body(data)?;
                if let Some(body_template) = &body_template {
                    unnote_templated_types(data, body_template);
                }

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
            data.add_global_variable(
                name.as_string(),
                CXGlobalVariable::Standard {
                    _type: return_type,
                    is_mutable: true,
                    initializer: Some(initial_value),
                },
                linkage,
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
                linkage,
            );
        }

        _ => {
            return log_parse_error!(
                data,
                "Unexpected token in global expression: {:#?}",
                data.tokens.peek()
            )
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

pub fn try_parse_identifier(tokens: &mut TokenIter) -> CXResult<Option<QualifiedName>> {
    if !matches!(tokens.peek().map(|token| &token.kind), Some(TokenKind::Identifier(_))) {
        return Ok(None);
    };

    let mut segments = Vec::new();

    loop {
        let TokenKind::Identifier(ident) = next_kind!(tokens)? else {
            return log_preparse_error!(tokens, "Expected identifier after '::' in qualified name");
        };

        segments.push(CXIdent::new(ident.clone()));

        if !try_next!(tokens, operator!(ScopeRes)) {
            break;
        }
    }

    let ident = segments.pop().expect("identifier parser should have at least one segment");
    Ok(Some(QualifiedName::new(
        NamespacePath::new(segments),
        ident,
    )))
}
