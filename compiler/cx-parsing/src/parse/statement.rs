use cx_ast::ast::expression::{CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_tokens::{
    keyword, punctuator,
    token::{IntegerBase, KeywordType, OperatorType, PunctuatorType, TokenKind},
};

use crate::{
    assert_token_matches,
    log::parse_point_error,
    next_kind,
    parse::{
        expressions::{parse_expr, parse_pattern},
        parse_block,
        parser::ParserData,
        types::{is_type_decl, parse_base_mods, parse_specifier, parse_type_base},
    },
    try_next,
};

pub(crate) fn parse_stmt(data: &mut ParserData) -> CXResult<CXExpression> {
    let start_index = data.tokens.index;

    try_parse_stmt(data)?.map(Result::Ok).unwrap_or_else(|| {
        data.tokens.index = start_index;
        let expr = parse_expr(data);
        assert_token_matches!(
            data.tokens,
            punctuator!(Semicolon),
            "';' after expression statement"
        );
        expr
    })
}

pub(crate) fn try_parse_stmt(data: &mut ParserData) -> CXResult<Option<CXExpression>> {
    match next_kind!(data.tokens)? {
        TokenKind::Keyword(keyword) => {
            let keyword = *keyword;

            if let Some(result) = try_parse_keyword_stmt(data, keyword)? {
                return Ok(Some(result));
            }
        }

        punctuator!(Semicolon) => {
            return Ok(Some(CXExprKind::Unit.into_expr(
                data.tokens.index,
                data.tokens.index,
                data.file_origin.clone(),
            )));
        }

        punctuator!(OpenBrace) => {
            data.tokens.back();
            return Ok(Some(parse_block(data)?));
        }

        _ => {}
    }

    data.back();
    if is_type_decl(data)? {
        let stmt = parse_declaration_stmt(data)?;
        assert_token_matches!(data.tokens, punctuator!(Semicolon), ";");
        Ok(Some(stmt))
    } else {
        Ok(None)
    }
}

pub(crate) fn try_parse_keyword_stmt(
    data: &mut ParserData,
    keyword_type: KeywordType,
) -> CXResult<Option<CXExpression>> {
    let start = data.tokens.index - 1;

    Ok(match keyword_type {
        KeywordType::If => {
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::OpenParen),
                "'('"
            );
            let expr = parse_expr(data)?;
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen),
                "')'"
            );
            let then_body = parse_stmt(data)?;
            let else_body = if try_next!(data.tokens, TokenKind::Keyword(KeywordType::Else)) {
                Some(parse_stmt(data)?)
            } else {
                None
            };

            Some(CXExprKind::If {
                condition: Box::new(expr),
                then_branch: Box::new(then_body),
                else_branch: else_body.map(Box::new),
            })
        }

        KeywordType::Switch => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

            let mut block = Vec::new();
            let mut cases = Vec::new();
            let mut default_case = None;
            let mut index = 0;

            while !try_next!(data.tokens, punctuator!(CloseBrace)) {
                if try_next!(data.tokens, keyword!(Case)) {
                    assert_token_matches!(data.tokens, TokenKind::IntLiteral(literal));
                    cases.push((literal.magnitude, index as usize));
                    assert_token_matches!(
                        data.tokens,
                        TokenKind::Punctuator(PunctuatorType::Colon),
                        "':'"
                    );
                    continue;
                } else if try_next!(data.tokens, keyword!(Default)) {
                    assert_token_matches!(
                        data.tokens,
                        TokenKind::Punctuator(PunctuatorType::Colon),
                        "':'"
                    );
                    if default_case.is_some() {
                        return parse_point_error(
                            &data.tokens,
                            "Multiple default cases in switch statement".to_string(),
                        );
                    }
                    default_case = Some(index as usize);
                    continue;
                }

                let expr = parse_stmt(data)?;
                index += 1;
                block.push(expr);
            }

            Some(CXExprKind::Switch {
                condition: Box::new(expr),
                block,
                cases,
                default_case,
            })
        }

        KeywordType::Defer => {
            let deferred = parse_expr(data)?;
            assert_token_matches!(
                data.tokens,
                punctuator!(Semicolon),
                "';' after deferred expression"
            );

            Some(CXExprKind::Defer {
                expr: Box::new(deferred),
            })
        }

        KeywordType::Match => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

            let mut arms = Vec::new();
            let mut default_arm = None;

            data.change_comma_mode(false);

            while !try_next!(data.tokens, punctuator!(CloseBrace)) {
                if try_next!(data.tokens, keyword!(Default)) {
                    assert_token_matches!(data.tokens, punctuator!(ThickArrow), "'=>'");
                    if default_arm.is_some() {
                        return parse_point_error(
                            &data.tokens,
                            "Multiple default cases in match statement".to_string(),
                        );
                    }
                    default_arm = Some(Box::new(parse_stmt(data)?));
                    continue;
                }

                let value = parse_pattern(data)?;
                assert_token_matches!(data.tokens, punctuator!(ThickArrow), "'=>'");
                let body = parse_stmt(data)?;
                arms.push((value, body));
            }

            data.pop_comma_mode();

            Some(CXExprKind::Match {
                condition: Box::new(expr),
                arms,
                default: default_arm,
            })
        }

        KeywordType::Do => {
            let body = parse_stmt(data)?;
            assert_token_matches!(data.tokens, keyword!(While), "'while'");
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

            Some(CXExprKind::While {
                condition: Box::new(expr),
                body: Box::new(body),
                pre_eval: false,
            })
        }

        KeywordType::While => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            let body = parse_stmt(data)?;

            Some(CXExprKind::While {
                condition: Box::new(expr),
                body: Box::new(body),
                pre_eval: true,
            })
        }

        KeywordType::Break => Some(CXExprKind::Break),
        KeywordType::Continue => Some(CXExprKind::Continue),
        KeywordType::For => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

            let init = parse_stmt(data)?;

            let condition = if matches!(
                data.tokens.peek().map(|token| &token.kind),
                Some(punctuator!(Semicolon))
            ) {
                CXExprKind::IntLiteral {
                    magnitude: 1,
                    base: IntegerBase::Decimal,
                    suffix: cx_tokens::token::IntegerSuffix::default(),
                }
                .into_expr(
                    data.tokens.index,
                    data.tokens.index,
                    data.file_origin_for_range(data.tokens.index, data.tokens.index),
                )
            } else {
                parse_expr(data)?
            };
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

            let increment = if matches!(
                data.tokens.peek().map(|token| &token.kind),
                Some(punctuator!(CloseParen))
            ) {
                CXExprKind::Unit.into_expr(
                    data.tokens.index,
                    data.tokens.index,
                    data.file_origin_for_range(data.tokens.index, data.tokens.index),
                )
            } else {
                parse_expr(data)?
            };
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

            let body = parse_stmt(data)?;

            Some(CXExprKind::For {
                init: Box::new(init),
                condition: Box::new(condition),
                increment: Box::new(increment),
                body: Box::new(body),
            })
        }

        _ => return Ok(None),
    }
    .map(|kind| {
        kind.into_expr(
            start,
            data.tokens.index,
            data.file_origin_for_range(start, data.tokens.index),
        )
    }))
}

pub(crate) fn parse_declaration_stmt(data: &mut ParserData) -> CXResult<CXExpression> {
    let start_index = data.tokens.index;

    let specifiers = parse_specifier(&mut data.tokens);
    let base_type = parse_type_base(data)?.add_specifier(specifiers);

    let mut decls = Vec::new();
    data.change_comma_mode(false);

    loop {
        let (name, _type) = parse_base_mods(data, base_type.clone())?;

        if let Some(name) = name {
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
                CXExprKind::VarDeclaration {
                    _type,
                    name,
                    initial_value,
                }
                .into_expr(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                ),
            );
        } else {
            return parse_point_error(
                &data.tokens,
                "Expected variable name in declaration".to_string(),
            );
        }

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    data.pop_comma_mode();

    if decls.len() == 1 {
        Ok(decls.pop().unwrap())
    } else {
        Ok(CXExprKind::Block {
            exprs: decls,
            creates_scope: false,
        }
        .into_expr(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        ))
    }
}
