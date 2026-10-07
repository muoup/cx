use cx_hir::ast::expression::{HIRBlockKind, HIRExprKind, HIRExpression};
use cx_hir::ast::modifiers::LinkageMode;
use cx_hir::ast::HIRStmt;
use cx_log::catalogue::parse::*;
use cx_log::CXResult;
use cx_tokens::{
    identifier, keyword, punctuator,
    token::{IntegerBase, IntegerSuffix, KeywordType, OperatorType, TokenKind},
};

use crate::{
    assert_token_matches,
    log::parse_point_error,
    next_kind,
    parse::{
        expressions::{parse_expr, parse_keyword_expr},
        functions::try_function_parse,
        parse_block,
        parser::ParserData,
        try_parse_simple_identifier,
        types::{is_type_decl, parse_base_mods, parse_type_base},
    },
    peek_kind, try_next,
};
use cx_util::identifier::CXIdent;

pub(crate) fn parse_stmt(data: &mut ParserData) -> CXResult<HIRExpression> {
    let start = data.tokens.index;

    let stmt = match next_kind!(data.tokens)? {
        punctuator!(Semicolon) => return Ok(data.expr_from(start, HIRExprKind::Void)),
        punctuator!(OpenBrace) => {
            data.tokens.back();
            parse_block(data)?
        }
        identifier!(name) if try_next!(data.tokens, punctuator!(Colon)) => {
            let name = CXIdent::new(name.clone());
            let statement = Box::new(parse_stmt(data)?);
            data.expr_from(start, HIRExprKind::Label { name, statement })
        }

        keyword!(If) => parse_if(data, start)?,
        keyword!(Switch) => parse_switch(data, start)?,
        // Parsed here rather than by the fallback so that it ends at its closing brace.
        keyword!(Match) => parse_keyword_expr(data, KeywordType::Match)?,
        keyword!(While) => parse_while(data, start)?,
        keyword!(Do) => parse_do_while(data, start)?,
        keyword!(For) => parse_for(data, start)?,
        keyword!(Goto) => parse_goto(data, start)?,
        keyword!(Defer) => {
            let expr = Box::new(parse_expr(data)?);
            data.expr_from(start, HIRExprKind::Defer { expr })
        }
        keyword!(Break) => data.expr_from(start, HIRExprKind::Break),
        keyword!(Continue) => data.expr_from(start, HIRExprKind::Continue),

        _ => {
            data.tokens.back();
            parse_declaration_or_expr(data)?
        }
    };

    let ends_with_then = matches!(
        data.tokens.prev().map(|token| &token.kind),
        Some(keyword!(Then))
    );
    if !ends_with_then && expects_semicolon(&stmt) {
        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
    }

    Ok(stmt)
}

fn expects_semicolon(stmt: &HIRExpression) -> bool {
    !matches!(
        stmt.kind,
        HIRExprKind::If { .. }
            | HIRExprKind::Switch { .. }
            | HIRExprKind::Match { .. }
            | HIRExprKind::For { .. }
            | HIRExprKind::While { pre_eval: true, .. }
            | HIRExprKind::Label { .. }
            | HIRExprKind::Block {
                kind: HIRBlockKind::Statement,
                ..
            }
    )
}

fn parse_declaration_or_expr(data: &mut ParserData) -> CXResult<HIRExpression> {
    if is_type_decl(data)? {
        parse_declaration_stmt(data)
    } else {
        parse_expr(data)
    }
}

fn parse_condition(data: &mut ParserData) -> CXResult<Box<HIRExpression>> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
    let condition = parse_expr(data)?;
    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

    Ok(Box::new(condition))
}

fn parse_if(data: &mut ParserData, start: usize) -> CXResult<HIRExpression> {
    let condition = parse_condition(data)?;
    let then_branch = Box::new(parse_stmt(data)?);
    let else_branch = if try_next!(data.tokens, keyword!(Else)) {
        Some(Box::new(parse_stmt(data)?))
    } else {
        None
    };

    Ok(data.expr_from(
        start,
        HIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        },
    ))
}

fn parse_switch(data: &mut ParserData, start: usize) -> CXResult<HIRExpression> {
    let condition = parse_condition(data)?;
    assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

    let mut block = Vec::new();
    let mut cases = Vec::new();
    let mut default_case = None;

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        if try_next!(data.tokens, keyword!(Case)) {
            let case_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
            cases.push((case_value, block.len()));
        } else if try_next!(data.tokens, keyword!(Default)) {
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
            if default_case.is_some() {
                return parse_point_error(
                    &data.tokens,
                    &DUPLICATE_ITEM,
                    ("default match arm".into(), "match".into()),
                );
            }
            default_case = Some(block.len());
        } else {
            block.push(parse_stmt(data)?);
        }
    }

    Ok(data.expr_from(
        start,
        HIRExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        },
    ))
}

fn parse_while(data: &mut ParserData, start: usize) -> CXResult<HIRExpression> {
    let condition = parse_condition(data)?;
    let body = Box::new(parse_stmt(data)?);

    Ok(data.expr_from(
        start,
        HIRExprKind::While {
            condition,
            body,
            pre_eval: true,
        },
    ))
}

fn parse_do_while(data: &mut ParserData, start: usize) -> CXResult<HIRExpression> {
    let body = Box::new(parse_stmt(data)?);
    assert_token_matches!(data.tokens, keyword!(While), "'while'");
    let condition = parse_condition(data)?;

    Ok(data.expr_from(
        start,
        HIRExprKind::While {
            condition,
            body,
            pre_eval: false,
        },
    ))
}

fn parse_for(data: &mut ParserData, start: usize) -> CXResult<HIRExpression> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

    let init = if peek_kind!(data.tokens, punctuator!(Semicolon)) {
        data.expr_from(data.tokens.index, HIRExprKind::Void)
    } else {
        parse_declaration_or_expr(data)?
    };
    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    let condition = if peek_kind!(data.tokens, punctuator!(Semicolon)) {
        data.expr_from(
            data.tokens.index,
            HIRExprKind::IntLiteral {
                magnitude: 1,
                base: IntegerBase::Decimal,
                suffix: IntegerSuffix::default(),
            },
        )
    } else {
        parse_expr(data)?
    };
    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    let increment = if peek_kind!(data.tokens, punctuator!(CloseParen)) {
        data.expr_from(data.tokens.index, HIRExprKind::Void)
    } else {
        parse_expr(data)?
    };
    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

    let body = parse_stmt(data)?;

    Ok(data.expr_from(
        start,
        HIRExprKind::For {
            init: Box::new(init),
            condition: Box::new(condition),
            increment: Box::new(increment),
            body: Box::new(body),
        },
    ))
}

fn parse_goto(data: &mut ParserData, start: usize) -> CXResult<HIRExpression> {
    let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
        return parse_point_error(
            &data.tokens,
            &EXPECTED_SYNTAX,
            ("a goto label".into(), None, None),
        );
    };

    Ok(data.expr_from(start, HIRExprKind::Goto { name }))
}

pub(crate) fn parse_declaration_stmt(data: &mut ParserData) -> CXResult<HIRExpression> {
    let start_index = data.tokens.index;

    try_next!(data.tokens, keyword!(Register));
    let mut specifiers = super::types::parse_decl_specifiers(&mut data.tokens);
    let base_type = parse_type_base(data)?.add_specifier(specifiers.qualifiers);
    super::types::parse_attributes(&mut data.tokens, &mut specifiers.attributes);

    let mut decls = Vec::new();
    data.change_comma_mode(false);

    loop {
        let (name, ty) = parse_base_mods(data, base_type.clone())?;

        if let Some(name) = name {
            if data.c_mode || specifiers.linkage == LinkageMode::Extern {
                let linkage = if data.c_mode
                    && specifiers.linkage == LinkageMode::Standard
                {
                    LinkageMode::Extern
                } else {
                    specifiers.linkage
                };
                if let Some(function) = try_function_parse(
                    data,
                    ty.clone(),
                    name.clone(),
                    linkage,
                    data.symbol_naming,
                    specifiers.attributes,
                )? {
                    data.add_stmt(HIRStmt::FunctionDefinition {
                        prototype: function.prototype,
                        visibility: data.visibility,
                        template_prototype: function.template_prototype,
                        body: None,
                    });
                    data.pop_comma_mode();
                    return Ok(HIRExprKind::Void.into_expr(
                        start_index,
                        data.tokens.index,
                        data.token_range(start_index, data.tokens.index),
                    ));
                }
            }

            // Check for initializer after variable name
            let initial_value = if try_next!(data.tokens, TokenKind::Assignment(None)) {
                data.change_comma_mode(false);
                let init_expr = parse_expr(data)?;
                data.pop_comma_mode();
                Some(Box::new(init_expr))
            } else {
                None
            };

            decls.push(
                HIRExprKind::VarDeclaration {
                    ty,
                    name,
                    initial_value,
                    linkage: specifiers.linkage,
                }
                .into_expr(
                    start_index,
                    data.tokens.index,
                    data.token_range(start_index, data.tokens.index),
                ),
            );
        } else if decls.is_empty() {
            return Ok(HIRExprKind::Void.into_expr(
                start_index,
                data.tokens.index,
                data.token_range(start_index, data.tokens.index),
            ));
        } else {
            return parse_point_error(&data.tokens, &EXPECTED_SYNTAX, ("a declaration name".into(), None, None));
        }

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    data.pop_comma_mode();

    if decls.len() == 1 {
        Ok(decls.pop().unwrap())
    } else {
        Ok(HIRExprKind::Block {
            exprs: decls.into(),
            kind: HIRBlockKind::Sequence,
        }
        .into_expr(
            start_index,
            data.tokens.index,
            data.token_range(start_index, data.tokens.index),
        ))
    }
}
