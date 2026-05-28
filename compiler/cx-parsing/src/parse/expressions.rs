use crate::parse::{ParserData, try_parse_simple_identifier};
use cx_ast::ast::{CXBinOp, CXExprKind, CXExpression, CXInitIndex, CXUnpackBinding};
use cx_ast::data::CXTypeKind;
use cx_ast::{assert_token_matches, next_kind, try_next};
use cx_mir::intrinsic_types::is_intrinsic_type;
use cx_tokens::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_tokens::{identifier, intrinsic, keyword, operator, punctuator, specifier};
use cx_util::namespace::QualifiedName;
use cx_util::{CXResult, log_error};

use crate::parse::operators::{
    PrecOperator, binop_prec, parse_binop, parse_postfix_unop, parse_prefix_unop, unop_prec,
};
use crate::parse::templates::parse_template_args;
use crate::parse::types::{parse_base_mods, parse_initializer, parse_specifier, parse_type_base};
use crate::parse::{parse_body, parse_intrinsic, try_parse_identifier};

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
            .into_expr_with_origin(
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
            .into_expr_with_origin(
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
            .into_expr_with_origin(
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
                    return log_parse_error!(data, "Expected field name in @unpack binding");
                };
                assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
                let Some(binding) = try_parse_simple_identifier(&mut data.tokens) else {
                    return log_parse_error!(data, "Expected binding name in @unpack binding");
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
            .into_expr_with_origin(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            ))
        }

        _ => {
            data.tokens.back();

            log_parse_error!(data, "Unknown intrinsic expression '{}'", ident)
        }
    }
}

pub fn is_type_decl(data: &mut ParserData) -> CXResult<bool> {
    let tok = data.tokens.peek().map(|tok| tok.kind.clone());

    if tok.is_none() {
        return Ok(false);
    }

    match &tok.unwrap() {
        intrinsic!() | specifier!() | keyword!(Struct, Union, Enum) => Ok(true),

        identifier!(name) if is_intrinsic_type(name) => Ok(true),

        TokenKind::Identifier(_) => {
            let pre_idx = data.tokens.index;
            let Some(ident) = try_parse_identifier(&mut data.tokens)? else {
                unreachable!()
            };
            data.tokens.index = pre_idx;

            data.is_type_ident(&ident)
        }

        _ => Ok(false),
    }
}

pub(crate) fn expression_requires_semicolon(expr: &CXExpression) -> bool {
    !matches!(
        expr.kind,
        CXExprKind::If { .. }
            | CXExprKind::While { .. }
            | CXExprKind::For { .. }
            | CXExprKind::Match { .. }
            | CXExprKind::Switch { .. }
    )
}

pub(crate) fn parse_expr(data: &mut ParserData) -> CXResult<CXExpression> {
    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    parse_expr_val(data, &mut expr_stack, &mut op_stack)?;
    while let Some(()) = parse_expr_op_concat(data, &mut expr_stack, &mut op_stack)? {}

    compress_stack(data, &mut expr_stack, &mut op_stack, 100)?;

    let Some(expr) = expr_stack.pop() else {
        return log_parse_error!(
            data,
            "Failed to parse expression value after operator: {:#?}",
            data.tokens.peek()
        );
    };

    if !expr_stack.is_empty() {
        return log_parse_error!(
            data,
            "Expression stack is not empty after parsing expression: {:#?} {:#?}",
            expr_stack,
            op_stack
        );
    }

    if !op_stack.is_empty() {
        return log_parse_error!(
            data,
            "Operator stack is not empty after parsing expression: {:#?} {:#?}",
            expr_stack,
            op_stack
        );
    }

    if try_next!(data.tokens, punctuator!(QuestionMark)) {
        let start_index = expr.range.start_token;
        let condition = expr;
        let then_branch = parse_expr(data)?;
        assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        let else_branch = parse_expr(data)?;
        let end_index = else_branch.range.end_token;

        return Ok(CXExprKind::Ternary {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        }
        .into_expr_with_origin(
            start_index,
            end_index,
            data.file_origin_for_range(start_index, end_index),
        ));
    }

    Ok(expr)
}

pub(crate) fn parse_declaration(data: &mut ParserData) -> CXResult<CXExpression> {
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
                .into_expr_with_origin(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                ),
            );
        } else {
            // If our expression starts with a type but has no name, we have a few options:
            //  1. We could be in a sizeof expression (e.g. sizeof(T)), in which we should just return the type as a dummy expression
            //  2. We could be in a scope resolution expression for either a static member function or a variant of a tagged enum

            assert_token_matches!(data.tokens, operator!(ScopeRes), "'::'");
            let variant_expr = parse_expr_identifier(data)?;

            let type_expr = match _type.kind {
                CXTypeKind::Identifier {
                    name: type_name, ..
                } => CXExprKind::Identifier(type_name).into_expr_with_origin(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                ),
                CXTypeKind::TemplatedIdentifier {
                    name,
                    input: template_input,
                } => CXExprKind::TemplatedIdentifier {
                    name,
                    template_input,
                }
                .into_expr_with_origin(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                ),
                _ => {
                    return log_parse_error!(
                        data,
                        "Expected identifier or templated identifier before scope resolution"
                    );
                }
            };

            let scope_res_expr = CXExprKind::BinOp {
                lhs: Box::new(type_expr),
                rhs: Box::new(variant_expr),
                op: CXBinOp::ScopeRes,
            }
            .into_expr_with_origin(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            );

            if !try_next!(data.tokens, punctuator!(OpenParen)) {
                decls.push(scope_res_expr);
                break;
            }

            // FIXME: Unify this logic with the logic for creating a argument list for a function call.
            let inner_expr = if try_next!(data.tokens, punctuator!(CloseParen)) {
                CXExprKind::Unit.into_expr_with_origin(
                    start_index,
                    data.tokens.index,
                    data.file_origin_for_range(start_index, data.tokens.index),
                )
            } else {
                data.change_comma_mode(true);
                let inner_expr = parse_expr(data)?;
                data.pop_comma_mode();
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                inner_expr
            };

            let method_call_expr = CXExprKind::BinOp {
                lhs: Box::new(scope_res_expr),
                rhs: Box::new(inner_expr),
                op: CXBinOp::MethodCall,
            }
            .into_expr_with_origin(
                start_index,
                data.tokens.index,
                data.file_origin_for_range(start_index, data.tokens.index),
            );

            decls.push(method_call_expr);
        }

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    data.pop_comma_mode();

    if decls.len() == 1 {
        Ok(decls.pop().unwrap())
    } else {
        Ok(CXExprKind::Block { exprs: decls }.into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        ))
    }
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

    if matches!(op_stack.last(), Some(PrecOperator::BinOp(CXBinOp::Is))) {
        expr_stack.push(parse_pattern(data)?);
    } else {
        parse_expr_val(data, expr_stack, op_stack)?;
    }

    Ok(Some(()))
}

pub(crate) fn parse_pattern(data: &mut ParserData) -> CXResult<CXExpression> {
    let start_index = data.tokens.index;

    if let Some(TokenKind::IntLiteral(value)) = data.tokens.peek().map(|token| &token.kind) {
        let value = *value;
        data.tokens.next();
        return Ok(CXExprKind::IntLiteral {
            bytes: 4,
            val: value,
        }
        .into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        ));
    }

    let constructor = parse_expr_identifier(data)?;
    if !is_qualified_pattern_constructor(&constructor) {
        return log_parse_error!(
            data,
            "Tagged union patterns must use a qualified variant name"
        );
    }

    if !try_next!(data.tokens, punctuator!(OpenParen)) {
        return Ok(constructor);
    }

    let inner = if try_next!(data.tokens, punctuator!(CloseParen)) {
        CXExprKind::Unit.into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        )
    } else {
        let Some(binding) = try_parse_simple_identifier(&mut data.tokens) else {
            return log_parse_error!(data, "Expected binding name in tagged union pattern");
        };
        assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
        CXExprKind::Identifier(QualifiedName::new_raw(binding)).into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        )
    };

    Ok(CXExprKind::BinOp {
        lhs: Box::new(constructor),
        rhs: Box::new(inner),
        op: CXBinOp::MethodCall,
    }
    .into_expr_with_origin(
        start_index,
        data.tokens.index,
        data.file_origin_for_range(start_index, data.tokens.index),
    ))
}

fn is_qualified_pattern_constructor(expr: &CXExpression) -> bool {
    match &expr.kind {
        CXExprKind::Identifier(name) => !name.namespace.is_root(),
        CXExprKind::BinOp {
            op: CXBinOp::ScopeRes,
            ..
        } => true,
        _ => false,
    }
}

fn compress_one_expr(
    data: &mut ParserData,
    expr_stack: &mut Vec<CXExpression>,
    op_stack: &mut Vec<PrecOperator>,
) -> CXResult<CXExpression> {
    let Some(op) = op_stack.pop() else {
        return log_parse_error!(
            data,
            "Operator stack is empty when trying to compress expression"
        );
    };

    match op {
        PrecOperator::UnOp(un_op) => {
            let rhs = expr_stack.pop().unwrap();

            let start_index = rhs.range.start_token;
            let end_index = rhs.range.end_token;

            let acc = CXExprKind::UnOp {
                operator: un_op,
                operand: Box::new(rhs),
            };

            Ok(acc.into_expr_with_origin(
                start_index,
                end_index,
                data.file_origin_for_range(start_index, end_index),
            ))
        }
        PrecOperator::BinOp(bin_op) => {
            let rhs = expr_stack.pop().unwrap();
            let lhs = expr_stack.pop().unwrap();

            let start_index = lhs.range.start_token;
            let end_index = rhs.range.end_token;

            let acc = CXExprKind::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: bin_op,
            };

            Ok(acc.into_expr_with_origin(
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
    if is_type_decl(data)? {
        expr_stack.push(parse_declaration(data)?);
        return Ok(());
    }

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

        TokenKind::Intrinsic(_) => CXExprKind::Identifier(QualifiedName::new_raw(parse_intrinsic(
            &mut data.back().tokens,
        )?)),
        TokenKind::CompilerIdentifier(ident) => {
            let ident = ident.clone();
            data.back();
            expr_stack.push(parse_at_intrinsic_expr(data, ident.as_str(), start_index)?);
            return Ok(());
        }
        TokenKind::Identifier(_) => {
            data.back();
            parse_expr_identifier(data)?.kind
        }

        TokenKind::Keyword(_) => {
            data.back();
            parse_keyword_expr(data)?.kind
        }

        TokenKind::Operator(OperatorType::Move) => {
            let expr = parse_expr(data)?;

            CXExprKind::Move {
                expr: Box::new(expr),
            }
        }

        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            if try_next!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen)
            ) {
                expr_stack.push(CXExprKind::Unit.into_expr_with_origin(
                    0,
                    0,
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
                expr_stack.push(CXExprKind::Unit.into_expr_with_origin(
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
            return log_parse_error!(data, "Expected expression value");
        }
    }
    .into_expr_with_origin(
        start_index,
        data.tokens.index,
        data.file_origin_for_range(start_index, data.tokens.index),
    );

    expr_stack.push(acc);

    while let Some(op) = parse_postfix_unop(data) {
        let prec = unop_prec(op.clone());

        compress_stack(data, expr_stack, op_stack, prec)?;
        op_stack.push(PrecOperator::UnOp(op));
    }

    Ok(())
}

pub(crate) fn parse_expr_identifier(data: &mut ParserData) -> CXResult<CXExpression> {
    let start_index = data.tokens.index;
    let Some(ident) = try_parse_identifier(&mut data.tokens)? else {
        return log_parse_error!(data, "Expected identifier");
    };

    let lhs = if !matches!(next_kind!(data.tokens)?, operator!(Less)) || !is_type_decl(data)? {
        data.tokens.back();
        CXExprKind::Identifier(ident).into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        )
    } else {
        data.tokens.back();

        let args = parse_template_args(data)?;

        CXExprKind::TemplatedIdentifier {
            name: ident,
            template_input: args,
        }
        .into_expr_with_origin(
            start_index,
            data.tokens.index,
            data.file_origin_for_range(start_index, data.tokens.index),
        )
    };

    if try_next!(data.tokens, operator!(ScopeRes)) {
        let rhs = parse_expr_identifier(data)?;
        let end_index = rhs.range.end_token;

        return Ok(CXExprKind::BinOp {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: CXBinOp::ScopeRes,
        }
        .into_expr_with_origin(
            start_index,
            end_index,
            data.file_origin_for_range(start_index, end_index),
        ));
    }

    Ok(lhs)
}

pub(crate) fn parse_keyword_expr(data: &mut ParserData) -> CXResult<CXExpression> {
    let start_index = data.tokens.index;

    let TokenKind::Keyword(keyword_type) = next_kind!(data.tokens)? else {
        unreachable!(
            "PANIC: parse_keyword_expr called with non-keyword token: {:#?}",
            data.tokens.peek()
        );
    };

    match keyword_type {
        KeywordType::Return => {
            let value = if try_next!(data.tokens, punctuator!(Semicolon)) {
                data.tokens.back();

                None
            } else {
                Some(Box::new(parse_expr(data)?))
            };

            Ok(CXExprKind::Return { value })
        }
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
            let then_body = parse_body(data)?;
            let else_body = if try_next!(data.tokens, TokenKind::Keyword(KeywordType::Else)) {
                Some(parse_body(data)?)
            } else {
                None
            };

            Ok(CXExprKind::If {
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
                    assert_token_matches!(data.tokens, TokenKind::IntLiteral(val));
                    cases.push((*val as u64, index as usize));
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
                        log_error!("Multiple default cases in switch statement");
                    }
                    default_case = Some(index as usize);
                    continue;
                }

                let expr = parse_expr(data)?;
                index += 1;

                if expression_requires_semicolon(&expr) {
                    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
                }
                block.push(expr);
            }

            Ok(CXExprKind::Switch {
                condition: Box::new(expr),
                block,
                cases,
                default_case,
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
                        log_error!("Multiple default cases in match statement");
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

        KeywordType::Do => {
            let body = parse_body(data)?;
            assert_token_matches!(data.tokens, keyword!(While), "'while'");
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

            Ok(CXExprKind::While {
                condition: Box::new(expr),
                body: Box::new(body),
                pre_eval: false,
            })
        }
        KeywordType::While => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
            let expr = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            let body = parse_body(data)?;

            Ok(CXExprKind::While {
                condition: Box::new(expr),
                body: Box::new(body),
                pre_eval: true,
            })
        }

        KeywordType::Sizeof => {
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::OpenParen),
                "'('"
            );

            let return_type = if is_type_decl(data)? {
                let (None, _type, _) = parse_initializer(data)? else {
                    return log_parse_error!(data, "Failed to parse type declaration for sizeof");
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

        KeywordType::Break => Ok(CXExprKind::Break),
        KeywordType::Continue => Ok(CXExprKind::Continue),
        KeywordType::For => {
            assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

            let init = if matches!(
                data.tokens.peek().map(|token| &token.kind),
                Some(punctuator!(Semicolon))
            ) {
                CXExprKind::Unit.into_expr_with_origin(
                    data.tokens.index,
                    data.tokens.index,
                    data.file_origin_for_range(data.tokens.index, data.tokens.index),
                )
            } else {
                parse_expr(data)?
            };
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

            let condition = if matches!(
                data.tokens.peek().map(|token| &token.kind),
                Some(punctuator!(Semicolon))
            ) {
                CXExprKind::IntLiteral { val: 1, bytes: 4 }.into_expr_with_origin(
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
                CXExprKind::Unit.into_expr_with_origin(
                    data.tokens.index,
                    data.tokens.index,
                    data.file_origin_for_range(data.tokens.index, data.tokens.index),
                )
            } else {
                parse_expr(data)?
            };
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

            let body = parse_body(data)?;

            Ok(CXExprKind::For {
                init: Box::new(init),
                condition: Box::new(condition),
                increment: Box::new(increment),
                body: Box::new(body),
            })
        }

        _ => {
            let keyword_type = *keyword_type;
            data.tokens.back();

            return log_parse_error!(
                data,
                "Unexpected keyword in expression: {:#?}",
                keyword_type
            );
        }
    }
    .map(|e| {
        e.into_expr_with_origin(
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

    Ok(
        CXExprKind::InitializerList { indices: inits }.into_expr_with_origin(
            init_index,
            data.tokens.index,
            data.file_origin_for_range(init_index, data.tokens.index),
        ),
    )
}
