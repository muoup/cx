use cx_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    function::HIRFunctionPrototype,
    global_var::HIRGlobalVariable,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    template::HIRTemplatePrototype,
    types::{HIRTypeKind, PredeclarationType},
    HIRStmt,
};
use cx_log::CXResult;
use cx_preparse_data::VisibilityMode;
use cx_tokens::{
    keyword, operator, punctuator, specifier,
    token::{OperatorType, PunctuatorType, SpecifierType, TokenKind},
    TokenIter,
};
use cx_util::identifier::CXIdent;

use crate::{
    assert_token_matches,
    log::parse_point_error,
    next_kind,
    parse::{
        expressions::parse_expr,
        functions::try_function_parse,
        parser::ParserData,
        statement::parse_stmt,
        templates::{note_templated_types, parse_template_prototype, unnote_templated_types},
        types::{parse_base_mods, parse_initializer, parse_typedef_initializer},
    },
    peek_next_kind, try_next,
};

pub(crate) mod parser;

mod expressions;
mod functions;
mod identifier;
mod operators;
mod statement;
mod templates;
mod types;

pub(crate) use identifier::{
    try_parse_identifier, try_parse_qualified_name, try_parse_type_identifier,
};

pub fn parse_global_stmt(data: &mut ParserData) -> CXResult<()> {
    let Some(token) = data.tokens.peek() else {
        return Ok(());
    };

    match &token.kind {
        TokenKind::IncludeBegin => {
            data.tokens.next();
            data.begin_include();
        }
        TokenKind::IncludeEnd => {
            data.tokens.next();
            data.end_include()?;
        }
        keyword!(Import) => {
            data.tokens.goto_statement_end();
        }
        keyword!(Typedef) => parse_typedef(data)?,
        keyword!(Comptime) => parse_comptime_fn_merge(data)?,
        punctuator!(Semicolon) => {
            data.tokens.next();
        }
        specifier!(Extern) | specifier!(Public) | specifier!(Private)
            if is_extern_c_section(data) =>
        {
            parse_extern_c_mod(data)?
        }
        specifier!(Public) | specifier!(Private) => parse_access_mods(data)?,
        _ => parse_global_expr(data)?,
    };

    Ok(())
}

fn is_extern_c_section(data: &ParserData) -> bool {
    let access_offset = usize::from(matches!(
        data.tokens
            .slice
            .get(data.tokens.index)
            .map(|token| &token.kind),
        Some(TokenKind::Specifier(
            SpecifierType::Public | SpecifierType::Private
        ))
    ));

    matches!(
        (
            data.tokens
                .slice
                .get(data.tokens.index + access_offset)
                .map(|token| &token.kind),
            data.tokens
                .slice
                .get(data.tokens.index + access_offset + 1)
                .map(|token| &token.kind),
        ),
        (
            Some(TokenKind::Specifier(SpecifierType::Extern)),
            Some(TokenKind::StringLiteral(abi))
        ) if abi == "C"
    )
}

fn parse_extern_c_mod(data: &mut ParserData) -> CXResult<()> {
    let visibility = if try_next!(data.tokens, specifier!(Public)) {
        VisibilityMode::Public
    } else {
        try_next!(data.tokens, specifier!(Private));
        VisibilityMode::Private
    };

    assert_token_matches!(data.tokens, specifier!(Extern), "'extern'");
    assert_token_matches!(data.tokens, TokenKind::StringLiteral(abi), "\"C\"");
    let abi = abi.clone();

    if abi != "C" {
        return parse_point_error(&data.tokens, format!("Unsupported extern ABI '{}'", abi));
    }

    assert_token_matches!(data.tokens, punctuator!(Colon), "':'");

    data.visibility = visibility;
    data.symbol_naming = HIRSymbolNameScheme::Unmangled;

    Ok(())
}

fn parse_access_mods(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            if !data.in_include() {
                data.symbol_naming = HIRSymbolNameScheme::Namespaced;
            }
        }
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            if !data.in_include() {
                data.symbol_naming = HIRSymbolNameScheme::Namespaced;
            }
        }

        _ => {
            return parse_point_error(
                &data.tokens,
                "Unexpected specifier in global scope".to_string(),
            );
        }
    };

    try_next!(data.tokens, punctuator!(Colon));

    Ok(())
}

fn parse_comptime_fn_merge(data: &mut ParserData) -> CXResult<()> {
    let func = functions::parse_comptime_function(data)?;

    let body = if let Some(template_prototype) = func.template_prototype.as_ref() {
        note_templated_types(data, template_prototype)?;
        let body = parse_body(data);
        unnote_templated_types(data, template_prototype);
        body
    } else {
        parse_body(data)
    }?;

    data.add_stmt(HIRStmt::ComptimeFunctionDefinition {
        prototype: func.prototype,
        visibility: data.visibility,
        template_prototype: func.template_prototype,
        body: Box::new(body),
    });

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
        return parse_point_error(
            &data.tokens.with_index(start_index),
            "Typedef must have a name!".to_string(),
        );
    };

    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    if let HIRTypeKind::Identifier {
        name: type_name,
        predeclaration,
        template_input: None,
    } = &_type.kind
    {
        let is_existing_type_alias = *predeclaration == PredeclarationType::None
            || data.ast.definition_stmts.iter().any(|definition| {
                matches!(
                    &definition.stmt,
                    HIRStmt::TypeDefinition {
                        name: Some(existing),
                        ..
                    } if existing == &name
                )
            });
        if type_name.namespace.is_root() && type_name.name == name && is_existing_type_alias {
            data.add_stmt(HIRStmt::TypeDefinition {
                name: Some(name),
                visibility: data.visibility,
                _type: _type.clone(),
                template_prototype: template_prototype.clone(),
                tag: None,
            });
            return Ok(());
        }
    }

    data.add_stmt(HIRStmt::TypeDefinition {
        name: Some(name),
        visibility: data.visibility,
        _type: _type.clone(),
        template_prototype: template_prototype.clone(),
        tag: None,
    });

    Ok(())
}

fn parse_fn_merge(
    data: &mut ParserData,
    mut prototype: HIRFunctionPrototype,
    template_prototype: Option<HIRTemplatePrototype>,
    inherited_external: bool,
) -> CXResult<()> {
    if try_next!(data.tokens, punctuator!(Semicolon)) {
        if template_prototype.is_some() {
            return parse_point_error(
                &data.tokens,
                "Templated functions must be defined in place.".to_string(),
            );
        }

        if inherited_external {
            prototype.linkage = LinkageMode::Extern;
        }

        data.add_stmt(HIRStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            template_prototype: None,
            body: None,
        });
    } else {
        let body = if let Some(template_prototype) = template_prototype.as_ref() {
            note_templated_types(data, template_prototype)?;
            let body = parse_body(data);
            unnote_templated_types(data, template_prototype);
            body
        } else {
            parse_body(data)
        }?;

        data.add_stmt(HIRStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            body: Some(Box::new(body)),
            template_prototype,
        });
    }

    Ok(())
}

fn parse_global_expr(data: &mut ParserData) -> CXResult<()> {
    let noreturn = matches!(
        data.tokens.peek().map(|token| &token.kind),
        Some(TokenKind::Identifier(name)) if name == "_Noreturn"
    );
    if noreturn {
        data.tokens.next();
    }

    let (name, return_type, linkage) = parse_initializer(data)?;
    let symbol_naming = if data.c_mode {
        if linkage == LinkageMode::Static {
            HIRSymbolNameScheme::Namespaced
        } else {
            HIRSymbolNameScheme::Unmangled
        }
    } else {
        data.symbol_naming
    };
    let inherited_external = !data.c_mode
        && symbol_naming == HIRSymbolNameScheme::Unmangled
        && linkage == LinkageMode::Standard;

    let Some(name) = name else {
        // Blank statement consisting on just a type, (i.e. struct [name] { [fields] };)

        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
        return Ok(());
    };

    if !data.tokens.has_next() {
        return parse_point_error(
            &data.tokens,
            "Reached end of token stream when parsing global expression!".to_string(),
        );
    }

    if let Some(func) = try_function_parse(
        data,
        return_type.clone(),
        name.clone(),
        linkage,
        symbol_naming,
        noreturn,
    )? {
        return parse_fn_merge(
            data,
            func.prototype,
            func.template_prototype,
            inherited_external,
        );
    }

    match next_kind!(data.tokens)? {
        TokenKind::Assignment(_) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
            data.add_stmt(HIRStmt::GlobalVariableDefinition {
                visibility: data.visibility,
                variable: HIRGlobalVariable::Standard {
                    name: name.clone(),
                    _type: return_type.clone(),
                    is_mutable: true,
                    linkage,
                    symbol_name_scheme: symbol_naming,
                    initializer: Some(initial_value.clone()),
                },
            });
        }

        punctuator!(Semicolon) => {
            add_global_variable(
                data,
                name,
                return_type.clone(),
                linkage,
                symbol_naming,
                inherited_external,
                None,
            );
        }

        operator!(Comma) => {
            add_global_variable(
                data,
                name,
                return_type.clone(),
                linkage,
                symbol_naming,
                inherited_external,
                None,
            );

            loop {
                let (next_name, next_type) = parse_base_mods(data, return_type.clone())?;
                let Some(next_name) = next_name else {
                    return parse_point_error(
                        &data.tokens,
                        "Expected variable name after ','".to_string(),
                    );
                };
                let initializer = if try_next!(data.tokens, TokenKind::Assignment(_)) {
                    Some(parse_expr(data)?)
                } else {
                    None
                };

                add_global_variable(
                    data,
                    next_name,
                    next_type,
                    linkage,
                    symbol_naming,
                    inherited_external,
                    initializer,
                );

                match next_kind!(data.tokens)? {
                    TokenKind::Operator(OperatorType::Comma) => {}
                    TokenKind::Punctuator(PunctuatorType::Semicolon) => break,
                    _ => {
                        return parse_point_error(
                            &data.tokens,
                            "Expected ',' or ';' after global declaration".to_string(),
                        );
                    }
                }
            }
        }

        _ => {
            return parse_point_error(
                &data.tokens,
                format!(
                    "Unexpected token in global expression: {:#?}",
                    data.tokens.peek()
                ),
            );
        }
    }

    Ok(())
}

fn add_global_variable(
    data: &mut ParserData,
    name: CXIdent,
    _type: cx_hir::ast::types::HIRType,
    linkage: LinkageMode,
    symbol_naming: HIRSymbolNameScheme,
    inherited_external: bool,
    initializer: Option<cx_hir::ast::expression::HIRExpression>,
) {
    data.add_stmt(HIRStmt::GlobalVariableDefinition {
        visibility: data.visibility,
        variable: HIRGlobalVariable::Standard {
            name,
            _type,
            is_mutable: true,
            linkage: if inherited_external {
                LinkageMode::Extern
            } else {
                linkage
            },
            symbol_name_scheme: symbol_naming,
            initializer,
        },
    });
}

pub(crate) fn parse_block(data: &mut ParserData) -> CXResult<HIRExpression> {
    assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

    let start_index = data.tokens.index - 1;
    let body = parse_block_statements(data)?;

    Ok(HIRExprKind::Block {
        exprs: body,
        creates_scope: true,
    }
    .into_expr(
        start_index,
        data.tokens.index,
        data.file_origin_for_range(start_index, data.tokens.index),
    ))
}

fn parse_block_statements(data: &mut ParserData) -> CXResult<Vec<HIRExpression>> {
    let mut body = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let mut statement = parse_stmt(data)?;
        let then_count = count_then_markers(&statement);
        let capturing_then_count = count_capturing_then_markers(&statement);
        if then_count != capturing_then_count {
            return parse_point_error(
                &data.tokens,
                "'then' must be the direct body of a staged expression".to_string(),
            );
        }
        if then_count > 1 {
            return parse_point_error(
                &data.tokens,
                "A statement may contain only one 'then' continuation".to_string(),
            );
        }

        if then_count == 1 {
            let continuation_start = data.tokens.index;
            let continuation = parse_block_statements(data)?;

            let continuation = HIRExprKind::Block {
                exprs: continuation,
                creates_scope: false,
            }
            .into_expr(
                continuation_start,
                data.tokens.index,
                data.file_origin_for_range(continuation_start, data.tokens.index),
            );
            replace_then_marker(&mut statement, continuation);
            body.push(statement);
            break;
        }

        body.push(statement);
    }

    Ok(body)
}

pub(crate) fn count_then_markers(expr: &HIRExpression) -> usize {
    match &expr.kind {
        HIRExprKind::Then => 1,
        HIRExprKind::BinOp { lhs, rhs, .. } => count_then_markers(lhs) + count_then_markers(rhs),
        HIRExprKind::UnOp { operand, .. }
        | HIRExprKind::Defer { expr: operand }
        | HIRExprKind::Emit { expr: operand }
        | HIRExprKind::Unsafe { expr: operand }
        | HIRExprKind::Leak { expr: operand }
        | HIRExprKind::Adopt { expr: operand } => count_then_markers(operand),
        HIRExprKind::StagedExpression { body, .. } => count_then_markers(body),
        HIRExprKind::Block { exprs, .. } => exprs.iter().map(count_then_markers).sum(),
        _ => 0,
    }
}

pub(crate) fn count_capturing_then_markers(expr: &HIRExpression) -> usize {
    match &expr.kind {
        HIRExprKind::StagedExpression { body, .. } if matches!(body.kind, HIRExprKind::Then) => 1,
        HIRExprKind::StagedExpression { body, .. } => count_capturing_then_markers(body),
        HIRExprKind::BinOp { lhs, rhs, .. } => {
            count_capturing_then_markers(lhs) + count_capturing_then_markers(rhs)
        }
        HIRExprKind::UnOp { operand, .. }
        | HIRExprKind::Defer { expr: operand }
        | HIRExprKind::Emit { expr: operand }
        | HIRExprKind::Unsafe { expr: operand }
        | HIRExprKind::Leak { expr: operand }
        | HIRExprKind::Adopt { expr: operand } => count_capturing_then_markers(operand),
        HIRExprKind::Block { exprs, .. } => exprs.iter().map(count_capturing_then_markers).sum(),
        _ => 0,
    }
}

fn replace_then_marker(expr: &mut HIRExpression, continuation: HIRExpression) {
    fn replace(expr: &mut HIRExpression, continuation: &mut Option<HIRExpression>) {
        match &mut expr.kind {
            HIRExprKind::Then => *expr = continuation.take().unwrap(),
            HIRExprKind::BinOp { lhs, rhs, .. } => {
                replace(lhs, continuation);
                if continuation.is_some() {
                    replace(rhs, continuation);
                }
            }
            HIRExprKind::UnOp { operand, .. }
            | HIRExprKind::Defer { expr: operand }
            | HIRExprKind::Emit { expr: operand }
            | HIRExprKind::Unsafe { expr: operand }
            | HIRExprKind::Leak { expr: operand }
            | HIRExprKind::Adopt { expr: operand } => replace(operand, continuation),
            HIRExprKind::StagedExpression { body, .. } => replace(body, continuation),
            HIRExprKind::Block { exprs, .. } => {
                for expr in exprs {
                    replace(expr, continuation);
                    if continuation.is_none() {
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    replace(expr, &mut Some(continuation));
}

pub(crate) fn parse_body(data: &mut ParserData) -> CXResult<HIRExpression> {
    if try_next!(data.tokens, punctuator!(OpenBrace)) {
        data.tokens.back();
        parse_block(data)
    } else {
        Ok(parse_stmt(data)?)
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
        return parse_point_error(tokens, "Expected intrinsic identifier".to_string());
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
