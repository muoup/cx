use crate::parse::{try_parse_simple_identifier, ParserData};
use crate::{assert_token_matches, log::ParserLogExt, next_kind, peek_next_kind, try_next};
use cx_ast::ast::expression::{CXExprKind, CXExpression, CXInitIndex, CXUnpackBinding};
use cx_ast::ast::pattern::CXPattern;
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
) -> CXResult<CXExpression> {
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

            Ok(CXExprKind::Unsafe {
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

            Ok(CXExprKind::Leak {
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

            Ok(CXExprKind::Adopt {
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
                    return data.log_error(format!("Expected field name in @unpack binding"));
                };
                assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
                let Some(binding) = try_parse_simple_identifier(&mut data.tokens) else {
                    return data.log_error(format!("Expected binding name in @unpack binding"));
                };

                bindings.push(CXUnpackBinding { field, binding });

                if !try_next!(data.tokens, operator!(Comma)) {
                    assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
                    break;
                }
            }

            Ok(CXExprKind::Unpack {
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

            data.log_error(format!("Unknown intrinsic expression '{}'", ident))
        }
    }
}

pub(crate) fn parse_expr(data: &mut ParserData) -> CXResult<CXExpression> {
    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    parse_expr_val(data, &mut expr_stack, &mut op_stack)?;
    while let Some(()) = parse_expr_op_concat(data, &mut expr_stack, &mut op_stack)? {}

    compress_stack(data, &mut expr_stack, &mut op_stack, 100)?;

    let Some(expr) = expr_stack.pop() else {
        return data.log_error(format!(
            "Failed to parse expression value after operator: {:#?}",
            data.tokens.peek()
        ));
    };

    if !expr_stack.is_empty() {
        return data.log_error(format!(
            "Expression stack is not empty after parsing expression: {:#?} {:#?}",
            expr_stack, op_stack
        ));
    }

    if !op_stack.is_empty() {
        return data.log_error(format!(
            "Operator stack is not empty after parsing expression: {:#?} {:#?}",
            expr_stack, op_stack
        ));
    }

    if try_next!(data.tokens, punctuator!(QuestionMark)) {
        let start_index = expr.range.start_token().unwrap_or(data.tokens.index);
        let condition = expr;
        let then_branch = parse_expr(data)?;
        assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        let else_branch = parse_expr(data)?;
        let end_index = else_branch.range.end_token().unwrap_or(data.tokens.index);

        return Ok(CXExprKind::Ternary {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
        .into_expr(
            start_index,
            end_index,
            data.file_origin_for_range(start_index, end_index),
        ));
    }

    Ok(expr)
}

pub(crate) fn parse_expr_op_concat(
    data: &mut ParserData,
    expr_stack: &mut Vec<CXExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<Option<()>> {
    let Some(op) = parse_binop(data).ok() else {
        return Ok(None);
    };

    let op_prec = binop_prec(op.clone());
    compress_stack(data, expr_stack, op_stack, op_prec)?;

    op_stack.push(PrecOperator::BinOp(op));

    parse_expr_val(data, expr_stack, op_stack)?;
    Ok(Some(()))
}

pub(crate) fn parse_pattern(data: &mut ParserData) -> CXResult<CXPattern> {
    match peek_next_kind!(data.tokens)? {
        TokenKind::IntLiteral(value) => {
            let value = *value;
            data.tokens.next();
            Ok(CXPattern::Integer(value))
        }

        TokenKind::FloatLiteral(f64, _) => {
            let value = *f64;
            data.tokens.next();
            Ok(CXPattern::Float(FloatWrapper::from(value)))
        }

        TokenKind::Identifier(_) => {
            let Some(ident) = try_parse_identifier(data)? else {
                unreachable!()
            };

            if ident.name.namespace.is_root() {
                if ident.template_input.is_some() {
                    return data.log_error(format!("Binding patterns may not have template input"));
                }

                Ok(CXPattern::Binding(ident.name.root_name().unwrap()))
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

                Ok(CXPattern::Variant {
                    constructor: ident.name,
                    template_input: ident.template_input,
                    inner: binding,
                })
            }
        }

        _ => data.log_error(format!("Expected pattern value")),
    }
}

fn compress_one_expr(
    data: &mut ParserData,
    expr_stack: &mut Vec<CXExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<CXExpression> {
    let Some(op) = op_stack.pop() else {
        return data.log_error(format!(
            "Operator stack is empty when trying to compress expression"
        ));
    };

    match op {
        PrecOperator::UnOp(un_op) => {
            let rhs = expr_stack.pop().unwrap();

            let start_index = rhs.range.start_token().unwrap_or(data.tokens.index);
            let end_index = rhs.range.end_token().unwrap_or(data.tokens.index);

            let acc = CXExprKind::UnOp {
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

            let acc = CXExprKind::BinOp {
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
    expr_stack: &mut Vec<CXExpression>,
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
    expr_stack: &mut Vec<CXExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<()> {
    let start_index = data.tokens.index;

    while let Some(op) = parse_prefix_unop(data)? {
        op_stack.push(PrecOperator::UnOp(op));
    }

    let acc = match &next_kind!(data.tokens)? {
        TokenKind::IntLiteral(value) => CXExprKind::IntLiteral {
            bytes: 4,
            val: *value,
        },
        TokenKind::FloatLiteral(value, bytes) => CXExprKind::FloatLiteral {
            bytes: *bytes,
            val: (*value).into(),
        },
        TokenKind::StringLiteral(value) => CXExprKind::StringLiteral { val: value.clone() },

        TokenKind::Operator(OperatorType::Access) => {
            if !try_next!(data.tokens, punctuator!(OpenBrace)) {
                return data.log_error(format!("Expected '{{' after '.' in expression"));
            }

            data.tokens.back();
            parse_block(data)?.kind
        }

        TokenKind::Intrinsic(_) => CXExprKind::Identifier {
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

            if try_next!(data.tokens, TokenKind::Identifier(_)) {
                // A common type error is of the form `A b` where A is not a recognized type, this would be picked up
                // as an "found unknown token identifer" error but can be far more accurately diagnosed as a missing
                // type error. There is no valid syntax of an identifier followed by another identifier otherwise
                return data.log_range_error(
                    expr.token_range(),
                    format!("Could not resolve type for variable declaration"),
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
                expr_stack.push(CXExprKind::Unit.into_expr(
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
                expr_stack.push(CXExprKind::Unit.into_expr(
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
            return data.log_error(format!("Expected expression value"));
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

pub(crate) fn parse_expr_identifier(data: &mut ParserData) -> CXResult<CXExpression> {
    let start_index = data.tokens.index;
    let Some(ident) = try_parse_identifier(data)? else {
        return data.log_error(format!("Expected identifier"));
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
) -> CXResult<CXExpression> {
    let start_index = data.tokens.index - 1;

    match keyword_type {
        KeywordType::Sizeof => {
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::OpenParen),
                "'('"
            );

            let return_type = if is_type_decl(data)? {
                let (None, _type, _) = parse_initializer(data)? else {
                    return data.log_error(format!("Failed to parse type declaration for sizeof"));
                };

                CXExprKind::SizeOfType { _type }
            } else {
                let expr = parse_expr(data)?;

                CXExprKind::SizeOfExpr {
                    expr: Box::new(expr),
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

            Ok(CXExprKind::Return { value })
        }

        KeywordType::Yield => {
            let value = if try_next!(data.tokens, punctuator!(Semicolon)) {
                data.tokens.back();

                None
            } else {
                Some(Box::new(parse_expr(data)?))
            };

            Ok(CXExprKind::Yield { value })
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
                        return data
                            .log_error(format!("Multiple default cases in match expression"));
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

            Ok(CXExprKind::Match {
                condition: Box::new(expr),
                arms,
                default: default_arm,
            })
        }

        KeywordType::Comptime => {
            data.log_error(format!("'comptime' is reserved but is not implemented yet"))
        }
        KeywordType::Expr => {
            data.log_error(format!("'expr' is reserved but is not implemented yet"))
        }
        KeywordType::Emit => {
            let expr = parse_expr(data)?;

            Ok(CXExprKind::Emit {
                expr: Box::new(expr),
            })
        }

        _ => {
            data.tokens.back();

            return data.log_error(format!("Unexpected token"));
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

pub(crate) fn parse_structured_initialization(data: &mut ParserData) -> CXResult<CXExpression> {
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

        inits.push(CXInitIndex {
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

    Ok(CXExprKind::InitializerList { indices: inits }.into_expr(
        init_index,
        data.tokens.index,
        data.file_origin_for_range(init_index, data.tokens.index),
    ))
}
