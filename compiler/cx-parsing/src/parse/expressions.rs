use crate::parse::{try_parse_simple_identifier, ParserData};
use crate::{
    assert_token_matches,
    log::{parse_point_error, parse_underline_error},
    next_kind, peek_next_kind, try_next,
};
use cx_hir::ast::expression::{
    HIRBinOp, HIRExprKind, HIRExpression, HIRInitIndex, HIRUnpackBinding,
};
use cx_hir::ast::pattern::HIRPattern;
use cx_log::CXResult;
use cx_tokens::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_tokens::{identifier, keyword, operator, punctuator};
use cx_util::namespace::QualifiedName;
use cx_util::unsafe_float::FloatWrapper;

use crate::parse::operators::{
    binop_prec, parse_binop, parse_postfix_unop, parse_prefix_unop, unop_prec, PrecOperator,
};
use crate::parse::types::{is_type_decl, parse_initializer};
use crate::parse::{parse_block, parse_body, parse_intrinsic, try_parse_identifier};

fn parse_at_intrinsic_expr(
    data: &mut ParserData,
    ident: &str,
    start_index: usize,
) -> CXResult<HIRExpression> {
    assert_token_matches!(data.tokens, TokenKind::CompilerIdentifier(_));

    match ident {
        "unsafe" => {
            let expr = if try_next!(data.tokens, punctuator!(OpenBrace)) {
                data.tokens.back();
                parse_body(data)?
            } else {
                assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                let expr = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                expr
            };

            Ok(HIRExprKind::Unsafe {
                expr: Box::new(expr),
            }
            .into_expr(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            ))
        }

        "leak" => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

            Ok(HIRExprKind::Leak {
                expr: Box::new(expr),
            }
            .into_expr(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            ))
        }

        "adopt" => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

            Ok(HIRExprKind::Adopt {
                expr: Box::new(expr),
            }
            .into_expr(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            ))
        }

        "unpack" => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

            let mut bindings = Vec::new();
            while !try_next!(data.tokens, punctuator!(CloseBrace)) {
                let Some(field) = try_parse_simple_identifier(&mut data.tokens) else {
                    return parse_point_error(
                        &data.tokens,
                        "Expected field name in @unpack binding".to_string(),
                    );
                };
                assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
                let Some(binding) = try_parse_simple_identifier(&mut data.tokens) else {
                    return parse_point_error(
                        &data.tokens,
                        "Expected binding name in @unpack binding".to_string(),
                    );
                };

                bindings.push(HIRUnpackBinding { field, binding });

                if !try_next!(data.tokens, operator!(Comma)) {
                    assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
                    break;
                }
            }

            Ok(HIRExprKind::Unpack {
                expr: Box::new(expr),
                bindings,
            }
            .into_expr(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            ))
        }

        _ => {
            data.tokens.back();

            parse_point_error(
                &data.tokens,
                format!("Unknown intrinsic expression '{}'", ident),
            )
        }
    }
}

pub(crate) fn parse_expr(data: &mut ParserData) -> CXResult<HIRExpression> {
    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    parse_expr_val(data, &mut expr_stack, &mut op_stack)?;
    while let Some(()) = parse_expr_op_concat(data, &mut expr_stack, &mut op_stack)? {}

    let ternary = if try_next!(data.tokens, punctuator!(QuestionMark)) {
        compress_stack(
            data,
            &mut expr_stack,
            &mut op_stack,
            binop_prec(HIRBinOp::Assign(None)).saturating_sub(1),
        )?;

        let Some(condition) = expr_stack.pop() else {
            return parse_point_error(&data.tokens, "Expected expression before '?'".to_string());
        };

        let start_index = condition.range.start_token().unwrap_or(data.tokens.index);
        let then_branch = parse_expr(data)?;
        assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        let else_branch = parse_expr(data)?;
        let end_index = else_branch.range.end_token().unwrap_or(data.tokens.index);

        Some(
            HIRExprKind::Ternary {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            }
            .into_expr(
                start_index,
                end_index,
                data.file_origin_for_range(start_index, end_index),
            ),
        )
    } else {
        None
    };

    if let Some(ternary) = ternary {
        expr_stack.push(ternary);
    }

    compress_stack(data, &mut expr_stack, &mut op_stack, 100)?;

    let Some(expr) = expr_stack.pop() else {
        return parse_point_error(
            &data.tokens,
            format!(
                "Failed to parse expression value after operator: {:#?}",
                data.tokens.peek()
            ),
        );
    };

    if !expr_stack.is_empty() {
        return parse_point_error(
            &data.tokens,
            format!(
                "Expression stack is not empty after parsing expression: {:#?} {:#?}",
                expr_stack, op_stack
            ),
        );
    }

    if !op_stack.is_empty() {
        return parse_point_error(
            &data.tokens,
            format!(
                "Operator stack is not empty after parsing expression: {:#?} {:#?}",
                expr_stack, op_stack
            ),
        );
    }

    Ok(expr)
}

pub(crate) fn parse_expr_op_concat(
    data: &mut ParserData,
    expr_stack: &mut Vec<HIRExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<Option<()>> {
    let Some(op) = parse_binop(data).ok() else {
        return Ok(None);
    };

    let op_prec = binop_prec(op.clone());
    let right_associative = matches!(op, HIRBinOp::Assign(_));
    compress_stack(
        data,
        expr_stack,
        op_stack,
        if right_associative {
            op_prec.saturating_sub(1)
        } else {
            op_prec
        },
    )?;

    op_stack.push(PrecOperator::BinOp(op));

    parse_expr_val(data, expr_stack, op_stack)?;
    Ok(Some(()))
}

fn is_va_arg_callee(expression: &HIRExpression) -> bool {
    matches!(
        &expression.kind,
        HIRExprKind::Identifier {
            name,
            template_input: None,
        } if name.root_name_ref().is_some_and(|name| matches!(name.as_str(), "va_arg" | "__builtin_va_arg"))
    )
}

fn parse_va_arg_call(data: &mut ParserData, expr_stack: &mut Vec<HIRExpression>) -> CXResult<()> {
    let callee = expr_stack
        .pop()
        .expect("va_arg callee missing from expression stack");
    let start_index = callee.range.start_token().unwrap_or(data.tokens.index);

    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
    data.change_comma_mode(false);
    let list = parse_expr(data)?;
    data.pop_comma_mode();
    assert_token_matches!(data.tokens, operator!(Comma), "','");
    let (_, _type, _) = parse_initializer(data)?;
    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

    expr_stack.push(
        HIRExprKind::VaArg {
            list: Box::new(list),
            _type,
        }
        .into_expr(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        ),
    );
    Ok(())
}

pub(crate) fn parse_pattern(data: &mut ParserData) -> CXResult<HIRPattern> {
    match peek_next_kind!(data.tokens)? {
        TokenKind::IntLiteral(literal) => {
            let value = literal.magnitude as i64;
            data.tokens.next();
            Ok(HIRPattern::Integer(value))
        }

        TokenKind::FloatLiteral(literal) => {
            let value = literal.value;
            data.tokens.next();
            Ok(HIRPattern::Float(FloatWrapper::from(value)))
        }

        TokenKind::Identifier(_) => {
            let Some(ident) = try_parse_identifier(data)? else {
                unreachable!()
            };

            if ident.name.namespace.is_root() {
                if ident.template_input.is_some() {
                    return parse_point_error(
                        &data.tokens,
                        "Binding patterns may not have template input".to_string(),
                    );
                }

                Ok(HIRPattern::Binding(ident.name.root_name().unwrap()))
            } else {
                let binding = if try_next!(data.tokens, punctuator!(OpenParen)) {
                    if try_next!(data.tokens, punctuator!(CloseParen)) {
                        None
                    } else {
                        data.change_comma_mode(true);
                        let binding = parse_pattern(data)?;
                        data.pop_comma_mode();
                        assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                        Some(Box::new(binding))
                    }
                } else {
                    None
                };

                Ok(HIRPattern::Variant {
                    constructor: ident.name,
                    template_input: ident.template_input,
                    inner: binding,
                })
            }
        }

        _ => parse_point_error(&data.tokens, "Expected pattern value".to_string()),
    }
}

fn compress_one_expr(
    data: &mut ParserData,
    expr_stack: &mut Vec<HIRExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<HIRExpression> {
    let Some(op) = op_stack.pop() else {
        return parse_point_error(
            &data.tokens,
            "Operator stack is empty when trying to compress expression".to_string(),
        );
    };

    match op {
        PrecOperator::UnOp(un_op) => {
            let rhs = expr_stack.pop().unwrap();

            let start_index = rhs.range.start_token().unwrap_or(data.tokens.index);
            let end_index = rhs.range.end_token().unwrap_or(data.tokens.index);

            let acc = HIRExprKind::UnOp {
                operator: un_op,
                operand: Box::new(rhs),
            };

            Ok(acc.into_expr(
                start_index,
                end_index,
                data.file_origin_for_range(start_index, end_index),
            ))
        }
        PrecOperator::BinOp(bin_op) => {
            let rhs = expr_stack.pop().unwrap();
            let lhs = expr_stack.pop().unwrap();

            let start_index = lhs.range.start_token().unwrap_or(data.tokens.index);
            let end_index = rhs.range.end_token().unwrap_or(data.tokens.index);

            let acc = HIRExprKind::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: bin_op,
            };

            Ok(acc.into_expr(
                start_index,
                end_index,
                data.file_origin_for_range(start_index, end_index),
            ))
        }
    }
}

pub(crate) fn compress_stack(
    data: &mut ParserData,
    expr_stack: &mut Vec<HIRExpression>,
    op_stack: &mut Vec<PrecOperator>,
    rprec: u8,
) -> CXResult<()> {
    if op_stack.is_empty() {
        return Ok(());
    }

    while let Some(op2) = op_stack.last() {
        if op2.get_precedence() > rprec {
            break;
        }

        let expr = compress_one_expr(data, expr_stack, op_stack)?;
        expr_stack.push(expr);
    }

    Ok(())
}

pub(crate) fn parse_expr_val(
    data: &mut ParserData,
    expr_stack: &mut Vec<HIRExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<()> {
    let start_index = data.tokens.index;

    while let Some(op) = parse_prefix_unop(data)? {
        op_stack.push(PrecOperator::UnOp(op));
    }

    let acc = match &next_kind!(data.tokens)? {
        TokenKind::IntLiteral(literal) => HIRExprKind::IntLiteral {
            magnitude: literal.magnitude,
            base: literal.base,
            suffix: literal.suffix,
        },
        TokenKind::FloatLiteral(literal) => HIRExprKind::FloatLiteral {
            suffix: literal.suffix,
            val: literal.value.into(),
        },
        TokenKind::StringLiteral(value) => HIRExprKind::StringLiteral { val: value.clone() },

        TokenKind::Operator(OperatorType::Access) => {
            if !try_next!(data.tokens, punctuator!(OpenBrace)) {
                return parse_point_error(
                    &data.tokens,
                    "Expected '{' after '.' in expression".to_string(),
                );
            }

            data.tokens.back();
            parse_block(data)?.kind
        }

        TokenKind::Operator(OperatorType::Bar) => {
            let mut params = Vec::new();
            loop {
                let Some(param) = try_parse_simple_identifier(&mut data.tokens) else {
                    return parse_point_error(
                        &data.tokens,
                        "Expected staged expression parameter name".to_string(),
                    );
                };
                params.push(param);

                if try_next!(data.tokens, operator!(Bar)) {
                    break;
                }
                assert_token_matches!(data.tokens, operator!(Comma), "',' or '|'");
            }

            let body = if try_next!(data.tokens, punctuator!(OpenBrace)) {
                data.tokens.back();
                parse_block(data)?
            } else {
                parse_expr(data)?
            };

            HIRExprKind::ParamStagedExpression {
                params,
                body: Box::new(body),
            }
        }

        TokenKind::Intrinsic(_) => HIRExprKind::Identifier {
            name: QualifiedName::new_raw(parse_intrinsic(&mut data.back().tokens)?),
            template_input: None,
        },
        TokenKind::CompilerIdentifier(ident) => {
            let ident = ident.clone();
            data.back();
            expr_stack.push(parse_at_intrinsic_expr(data, ident.as_str(), start_index)?);
            return Ok(());
        }
        TokenKind::Identifier(_) => {
            data.back();

            let expr = parse_expr_identifier(data)?;

            if is_va_arg_callee(&expr)
                && data.tokens.peek().is_some_and(|token| {
                    matches!(token.kind, TokenKind::Punctuator(PunctuatorType::OpenParen))
                })
            {
                expr_stack.push(expr);
                parse_va_arg_call(data, expr_stack)?;
                return Ok(());
            }

            if try_next!(data.tokens, TokenKind::Identifier(_)) {
                // A common type error is of the form `A b` where A is not a recognized type, this would be picked up
                // as an "found unknown token identifer" error but can be far more accurately diagnosed as a missing
                // type error. There is no valid syntax of an identifier followed by another identifier otherwise
                return parse_underline_error(
                    &data.tokens,
                    "Could not resolve type for variable declaration".to_string(),
                    expr.token_range(),
                );
            }

            expr.kind
        }

        TokenKind::Keyword(keyword) => {
            let keyword = *keyword;
            parse_keyword_expr(data, keyword)?.kind
        }

        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            if try_next!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen)
            ) {
                expr_stack.push(HIRExprKind::Void.into_expr(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                ));
                return Ok(());
            }

            data.change_comma_mode(true);

            let expr = parse_expr(data)?;

            data.pop_comma_mode();

            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen),
                "')'"
            );

            expr.kind
        }

        TokenKind::Punctuator(PunctuatorType::OpenBrace) => {
            data.tokens.back();

            parse_structured_initialization(data)?.kind
        }

        TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
            if try_next!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseBracket)
            ) {
                expr_stack.push(HIRExprKind::Void.into_expr(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                ));
                return Ok(());
            }

            let index = parse_expr(data)?;
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseBracket),
                "']'"
            );

            index.kind
        }

        _ => {
            data.back();
            return parse_point_error(&data.tokens, "Expected expression value".to_string());
        }
    }
    .into_expr(
        start_index,
        data.tokens.index,
        data.file_origin_for_range(start_index, data.tokens.index),
    );

    expr_stack.push(acc);

    while let Some(op) = parse_postfix_unop(data)? {
        let prec = unop_prec(op.clone());

        compress_stack(data, expr_stack, op_stack, prec)?;
        op_stack.push(PrecOperator::UnOp(op));
    }

    Ok(())
}

pub(crate) fn parse_expr_identifier(data: &mut ParserData) -> CXResult<HIRExpression> {
    let start_index = data.tokens.index;
    let Some(ident) = try_parse_identifier(data)? else {
        return parse_point_error(&data.tokens, "Expected identifier".to_string());
    };

    Ok(ident.into_expr(
        start_index,
        data.tokens.index,
        data.file_origin_for_range(start_index, data.tokens.index),
    ))
}

pub(crate) fn parse_keyword_expr(
    data: &mut ParserData,
    keyword_type: KeywordType,
) -> CXResult<HIRExpression> {
    let start_index = data.tokens.index - 1;

    match keyword_type {
        KeywordType::True => Ok(HIRExprKind::BoolLiteral(true)),
        KeywordType::False => Ok(HIRExprKind::BoolLiteral(false)),

        KeywordType::Sizeof | KeywordType::Alignof => {
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::OpenParen),
                "'('"
            );

            let return_type = if is_type_decl(data)? {
                let (None, _type, _) = parse_initializer(data)? else {
                    return parse_point_error(
                        &data.tokens,
                        format!(
                            "Failed to parse type declaration for {}",
                            match keyword_type {
                                KeywordType::Sizeof => "sizeof",
                                KeywordType::Alignof => "alignof",
                                _ => unreachable!(),
                            }
                        ),
                    );
                };

                match keyword_type {
                    KeywordType::Sizeof => HIRExprKind::SizeOfType { _type },
                    KeywordType::Alignof => HIRExprKind::AlignOfType { _type },
                    _ => unreachable!(),
                }
            } else {
                let expr = parse_expr(data)?;

                match keyword_type {
                    KeywordType::Sizeof => HIRExprKind::SizeOfExpr {
                        expr: Box::new(expr),
                    },
                    KeywordType::Alignof => HIRExprKind::AlignOfExpr {
                        expr: Box::new(expr),
                    },
                    _ => unreachable!(),
                }
            };

            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen),
                "')'"
            );

            Ok(return_type)
        }

        KeywordType::Return => {
            let value = if try_next!(data.tokens, punctuator!(Semicolon)) {
                data.tokens.back();

                None
            } else {
                Some(Box::new(parse_expr(data)?))
            };

            Ok(HIRExprKind::Return { value })
        }

        KeywordType::Yield => {
            let value = if try_next!(data.tokens, punctuator!(Semicolon)) {
                data.tokens.back();

                None
            } else {
                Some(Box::new(parse_expr(data)?))
            };

            Ok(HIRExprKind::Yield { value })
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
                            "Multiple default cases in match expression".to_string(),
                        );
                    }
                    default_arm = Some(Box::new(parse_body(data)?));
                    continue;
                }

                let value = parse_pattern(data)?;
                assert_token_matches!(data.tokens, punctuator!(ThickArrow), "'=>'");
                let body = parse_body(data)?;
                arms.push((value, body));
            }

            data.pop_comma_mode();

            Ok(HIRExprKind::Match {
                condition: Box::new(expr),
                arms,
                default: default_arm,
            })
        }

        KeywordType::Comptime => parse_point_error(
            &data.tokens,
            "'comptime' is reserved but is not implemented yet".to_string(),
        ),
        KeywordType::Expr => parse_point_error(
            &data.tokens,
            "'expr' is reserved but is not implemented yet".to_string(),
        ),
        KeywordType::Emit => {
            let expr = parse_expr(data)?;

            Ok(HIRExprKind::Emit {
                expr: Box::new(expr),
            })
        }
        KeywordType::Then => Ok(HIRExprKind::Then),

        _ => {
            data.tokens.back();

            return parse_point_error(&data.tokens, "Unexpected token".to_string());
        }
    }
    .map(|e| {
        e.into_expr(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        )
    })
}

pub(crate) fn parse_structured_initialization(data: &mut ParserData) -> CXResult<HIRExpression> {
    let init_index = data.tokens.index;
    assert_token_matches!(
        data.tokens,
        TokenKind::Punctuator(PunctuatorType::OpenBrace),
        "'{'"
    );

    let mut inits = Vec::new();

    while !try_next!(
        data.tokens,
        TokenKind::Punctuator(PunctuatorType::CloseBrace)
    ) {
        let field_name = if try_next!(data.tokens, TokenKind::Operator(OperatorType::Access)) {
            assert_token_matches!(data.tokens, identifier!(field_name));
            let field_name = field_name.clone();
            assert_token_matches!(data.tokens, TokenKind::Assignment(None));
            Some(field_name)
        } else {
            None
        };

        data.change_comma_mode(false);
        let val = parse_expr(data)?;
        data.pop_comma_mode();

        inits.push(HIRInitIndex {
            name: field_name,
            value: val,
            index: 0,
        });

        if !try_next!(data.tokens, operator!(Comma)) {
            // If we didn't find a comma, it must be the end of the initializer list
            assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
            break;
        }
    }

    Ok(HIRExprKind::InitializerList { indices: inits }.into_expr(
        init_index,
        data.tokens.index,
        data.file_origin_for_range(init_index, data.tokens.index),
    ))
}
