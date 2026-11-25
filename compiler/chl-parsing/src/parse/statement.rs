use super::types::TagScope;
use chl_hir::ast::expression::{HIRBlockKind, HIRExprKind, HIRExpression};
use chl_hir::ast::modifiers::LinkageMode;
use chl_hir::ast::HIRStmt;
use chl_log::catalogue::parse::*;
use chl_log::CHLResult;
use chl_tokens::{
    identifier, keyword, operator, punctuator,
    token::{KeywordType, OperatorType, TokenKind},
};

use crate::{
    assert_token_matches,
    log::parse_point_error,
    next_kind,
    parse::{
        expressions::{parse_expr, parse_keyword_expr},
        functions::try_function_parse,
        parse_block,
        parser::{LocalName, ParserData},
        try_parse_simple_identifier,
        types::{
            parse_declaration_head, parse_declarator, parse_variable_initializer, starts_type,
            DeclaratorKind, TypePosition,
        },
    },
    peek_kind, try_next,
};
use chl_util::identifier::CHLIdent;

pub(super) fn parse_block_item(data: &mut ParserData) -> CHLResult<HIRExpression> {
    parse_item(data, true)
}

fn parse_stmt(data: &mut ParserData) -> CHLResult<HIRExpression> {
    data.with_name_scope(|data| parse_item(data, false))
}

fn parse_item(data: &mut ParserData, allow_declaration: bool) -> CHLResult<HIRExpression> {
    let start = data.tokens.index;

    let stmt = match next_kind!(data.tokens)? {
        punctuator!(Semicolon) => return Ok(data.expr_from(start, HIRExprKind::Void)),
        punctuator!(OpenBrace) => {
            data.tokens.back();
            return parse_block(data);
        }
        identifier!(name) if try_next!(data.tokens, punctuator!(Colon)) => {
            let name = CHLIdent::new(name.clone());
            let statement = parse_labelled_stmt(data, allow_declaration)?;
            return Ok(data.expr_from(start, HIRExprKind::Label { name, statement }));
        }
        keyword!(Case) => {
            let value = Some(Box::new(parse_expr(data)?));
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
            let statement = parse_labelled_stmt(data, allow_declaration)?;
            return Ok(data.expr_from(start, HIRExprKind::Case { value, statement }));
        }
        keyword!(Default) => {
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
            let statement = parse_labelled_stmt(data, allow_declaration)?;
            return Ok(data.expr_from(start, HIRExprKind::Case { value: None, statement }));
        }

        keyword!(If) => return parse_if(data, start),
        keyword!(Switch) => return parse_switch(data, start),
        keyword!(Match) => return parse_keyword_expr(data, KeywordType::Match),
        keyword!(While) => return data.with_name_scope(|data| parse_while(data, start)),
        keyword!(Do) => return data.with_name_scope(|data| parse_do_while(data, start)),
        keyword!(For) => return data.with_name_scope(|data| parse_for(data, start)),
        keyword!(Goto) => parse_goto(data, start)?,
        keyword!(Defer) => {
            let expr = Box::new(parse_expr(data)?);
            data.expr_from(start, HIRExprKind::Defer { expr })
        }
        keyword!(Break) => data.expr_from(start, HIRExprKind::Break),
        keyword!(Continue) => data.expr_from(start, HIRExprKind::Continue),

        _ => {
            data.tokens.back();
            parse_simple_clause(data, allow_declaration)?
        }
    };

    let ends_with_then = matches!(
        data.tokens.prev().map(|token| &token.kind),
        Some(keyword!(Then))
    );
    if !ends_with_then {
        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
    }

    Ok(stmt)
}

/// The statement a label applies to. A label may also end a block, where it labels nothing.
fn parse_labelled_stmt(data: &mut ParserData, allow_declaration: bool) -> CHLResult<Box<HIRExpression>> {
    if peek_kind!(data.tokens, punctuator!(CloseBrace)) {
        return Ok(Box::new(data.expr_from(data.tokens.index, HIRExprKind::Void)));
    }

    Ok(Box::new(parse_item(data, allow_declaration)?))
}

fn parse_simple_clause(data: &mut ParserData, allow_declaration: bool) -> CHLResult<HIRExpression> {
    if allow_declaration && starts_type(data, TypePosition::Statement)? {
        parse_declaration(data)
    } else {
        parse_expr(data)
    }
}

fn parse_condition(data: &mut ParserData) -> CHLResult<Box<HIRExpression>> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
    let condition = parse_expr(data)?;
    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

    Ok(Box::new(condition))
}

fn parse_if(data: &mut ParserData, start: usize) -> CHLResult<HIRExpression> {
    let (condition, then_branch) = data.with_name_scope(|data| {
        Ok((parse_condition(data)?, Box::new(parse_stmt(data)?)))
    })?;
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

fn parse_switch(data: &mut ParserData, start: usize) -> CHLResult<HIRExpression> {
    let condition = parse_condition(data)?;
    let body = Box::new(parse_stmt(data)?);

    Ok(data.expr_from(start, HIRExprKind::Switch { condition, body }))
}

fn parse_while(data: &mut ParserData, start: usize) -> CHLResult<HIRExpression> {
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

fn parse_do_while(data: &mut ParserData, start: usize) -> CHLResult<HIRExpression> {
    let body = Box::new(parse_stmt(data)?);
    assert_token_matches!(data.tokens, keyword!(While), "'while'");
    let condition = parse_condition(data)?;
    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    Ok(data.expr_from(
        start,
        HIRExprKind::While {
            condition,
            body,
            pre_eval: false,
        },
    ))
}

fn parse_for(data: &mut ParserData, start: usize) -> CHLResult<HIRExpression> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

    let init = if peek_kind!(data.tokens, punctuator!(Semicolon)) {
        data.expr_from(data.tokens.index, HIRExprKind::Void)
    } else {
        parse_simple_clause(data, true)?
    };
    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    let condition = if peek_kind!(data.tokens, punctuator!(Semicolon)) {
        None
    } else {
        Some(Box::new(parse_expr(data)?))
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
            condition,
            increment: Box::new(increment),
            body: Box::new(body),
        },
    ))
}

fn parse_goto(data: &mut ParserData, start: usize) -> CHLResult<HIRExpression> {
    if try_next!(data.tokens, operator!(Asterisk)) {
        let target = Box::new(parse_expr(data)?);
        return Ok(data.expr_from(start, HIRExprKind::IndirectGoto { target }));
    }

    let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
        return parse_point_error(
            &data.tokens,
            &EXPECTED_SYNTAX,
            ("a goto label".into(), None),
        );
    };

    Ok(data.expr_from(start, HIRExprKind::Goto { name }))
}

fn parse_declaration(data: &mut ParserData) -> CHLResult<HIRExpression> {
    let start_index = data.tokens.index;

    try_next!(data.tokens, keyword!(Register));
    let (base, specifiers) = parse_declaration_head(data, TagScope::Module)?;

    let mut decls = Vec::new();
    data.change_comma_mode(false);

    loop {
        let (name, ty) = parse_declarator(data, &base, TagScope::Module, DeclaratorKind::Named)?;

        if let Some(name) = name {
            if data.c_mode || specifiers.linkage == LinkageMode::Extern {
                let linkage = if data.c_mode
                    && specifiers.linkage == LinkageMode::Standard
                {
                    LinkageMode::Extern
                } else {
                    specifiers.linkage
                };
                if let Some(mut prototype) = try_function_parse(
                    data,
                    start_index,
                    ty.clone(),
                    name.clone(),
                    false,
                    super::types::ParsedSpecifiers { linkage, ..specifiers },
                    data.symbol_naming,
                )? {
                    data.note_local_name(name, LocalName::Value);
                    super::functions::linkage::resolve(data, &mut prototype, false);
                    data.add_stmt(HIRStmt::FunctionDefinition {
                        prototype,
                        visibility: data.visibility,
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

            data.note_local_name(name.clone(), LocalName::Value);
            let initial_value = parse_variable_initializer(data)?.map(Box::new);

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
            data.pop_comma_mode();
            return Ok(HIRExprKind::Void.into_expr(
                start_index,
                data.tokens.index,
                data.token_range(start_index, data.tokens.index),
            ));
        } else {
            return parse_point_error(&data.tokens, &EXPECTED_SYNTAX, ("a declaration name".into(), None));
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
