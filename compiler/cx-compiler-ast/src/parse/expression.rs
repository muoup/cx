use cx_data_ast::parse::macros::error_pointer;
use std::clone;
use std::collections::VecDeque;
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::global_scope::parse_body;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr};
use cx_data_ast::parse::identifier::{parse_identifier, parse_intrinsic, parse_std_ident};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::parse::value_type::CXValType;
use crate::parse::lvalues::reformat_lvalue;
use crate::parse::operators::{binop_prec, comma_separated, comma_separated_owned, parse_binop, parse_post_unop, parse_pre_unop, unop_prec, PrecOperator};
use cx_util::{log_error, point_log_error};
use crate::parse::typing::{is_type_decl, parse_initializer};

pub(crate) fn requires_semicolon(expr: &CXExpr) -> bool {
    match expr {
        CXExpr::If { .. } => false,
        CXExpr::While { .. } => false,
        CXExpr::For { .. } => false,

        _ => true
    }
}

pub(crate) fn parse_expr(data: &mut ParserData) -> Option<CXExpr> {
    if let Some(Token::Keyword(keyword)) = data.toks.peek() {
        let keyword = keyword.clone();
        data.toks.next();

        if let Some(expr) = parse_keyword_val(data, keyword) {
            return Some(expr);
        }
    }

    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    let Some(_) = parse_expr_val(data, &mut expr_stack, &mut op_stack) else {
        return Some(CXExpr::Unit);
    };

    loop {
        if let Some(()) = parse_expr_op_concat(data, &mut expr_stack, &mut op_stack) {
            continue;
        }

        break;
    }

    compress_stack(&mut expr_stack, &mut op_stack, 100)?;

    let Some(expr) = expr_stack.pop() else {
        log_error!("Failed to parse expression value after operator: {:#?}", data.toks.peek());
    };

    if !expr_stack.is_empty() {
        log_error!("PARSER ERROR: Expression stack is not empty after parsing expression: {:#?} {:#?}", expr_stack, op_stack);
    }

    Some(expr)
}

pub(crate) fn parse_expr_op_concat(data: &mut ParserData, expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>) -> Option<()> {
    let Some(op) = parse_binop(data) else {
        return None;
    };

    let op_prec = binop_prec(op.clone());
    compress_stack(expr_stack, op_stack, op_prec)?;

    op_stack.push(PrecOperator::BinOp(op));

    let Some(_) = parse_expr_val(data, expr_stack, op_stack) else {
        log_error!("PARSER ERROR: Failed to parse expression value after operator: {:#?}", data.toks.peek());
    };

    Some(())
}

fn compress_one_expr(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>) -> Option<CXExpr> {
    let op = op_stack.pop().unwrap();

    match op {
        PrecOperator::UnOp(un_op) => {
            let rhs = expr_stack.pop().unwrap();

            let acc = CXExpr::UnOp {
                operator: un_op,
                operand: Box::new(rhs)
            };

            Some(acc)
        },
        PrecOperator::BinOp(bin_op) => {
            let lhs = expr_stack.pop().unwrap();
            let rhs = expr_stack.pop().unwrap();

            let acc = CXExpr::BinOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: bin_op
            };

            Some(acc)
        }
    }
}

pub(crate) fn compress_stack(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>, rprec: u8) -> Option<()> {
    if op_stack.is_empty() {
        return Some(());
    }

    let mut ops = Vec::new();
    let mut exprs = Vec::new();

    exprs.push(expr_stack.pop().unwrap());

    while let Some(op2) = op_stack.last() {
        if op2.get_precedence() > rprec {
            break;
        }

        if matches!(op2, PrecOperator::UnOp(_)) {
            let un_expr = compress_one_expr(&mut exprs, op_stack)?;
            exprs.push(un_expr);
            continue;
        }

        ops.push(op_stack.pop().unwrap());
        exprs.push(expr_stack.pop().unwrap());
    }

    while !ops.is_empty() {
        let expr = compress_one_expr(&mut exprs, &mut ops)?;
        exprs.push(expr);
    }

    expr_stack.push(exprs.pop().unwrap());

    if !exprs.is_empty() {
        log_error!("PARSER ERROR: Expression stack is not empty after parsing expression: {:#?} {:#?}", exprs, op_stack);
    }

    Some(())
}

pub(crate) fn parse_expr_val(data: &mut ParserData, expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<PrecOperator>) -> Option<()> {
    if is_type_decl(data) {
        let Some((Some(name), type_)) = parse_initializer(data) else {
            println!("{}", error_pointer(&data.toks));
            log_error!("PARSER ERROR: Failed to parse type declaration");
        };

        expr_stack.push(CXExpr::VarDeclaration { type_, name });
        return Some(());
    }

    while let Some(op) = parse_pre_unop(data) {
        op_stack.push(PrecOperator::UnOp(op));
    }

    let acc = match data.toks.next()? {
        Token::IntLiteral(value) =>
            CXExpr::IntLiteral { bytes: 4, val: value.clone() },
        Token::FloatLiteral(value) =>
            CXExpr::FloatLiteral { bytes: 4, val: value.clone() },
        Token::StringLiteral(value) =>
            CXExpr::StringLiteral { val: value.clone() },

        Token::Intrinsic(_) =>
            CXExpr::Identifier(parse_intrinsic(data.back())?),
        Token::Identifier(_) =>
            CXExpr::Identifier(parse_std_ident(data.back())?),

        Token::Punctuator(PunctuatorType::OpenParen) => {
            if try_next!(data, Token::Punctuator(PunctuatorType::CloseParen)) {
                expr_stack.push(CXExpr::Unit);
                return Some(());
            }

            let expr = parse_expr(data)?;

            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

            expr
        },
        Token::Punctuator(PunctuatorType::OpenBracket) => {
            if try_next!(data, Token::Punctuator(PunctuatorType::CloseBracket)) {
                expr_stack.push(CXExpr::Unit);
                return Some(());
            }

            let index = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBracket));

            index
        },

        _ => {
            data.back();
            return None
        }
    };

    expr_stack.push(acc);

    while let Some(op) = parse_post_unop(data) {
        let prec = unop_prec(op.clone());

        compress_stack(expr_stack, op_stack, prec);
        op_stack.push(PrecOperator::UnOp(op));
    }

    Some(())
}

pub(crate) fn parse_keyword_val(data: &mut ParserData, keyword: KeywordType) -> Option<CXExpr> {
    match keyword {
        KeywordType::Return => {
            let val = parse_expr(data)?;

            if matches!(val, CXExpr::Unit) {
                return Some(CXExpr::Return { value: None });
            }

            Some(
                CXExpr::Return {
                    value: Some(Box::new(val))
                }
            )
        },
        KeywordType::If => {
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            let then_body = parse_body(data)?;
            let else_body =
                if try_next!(data, Token::Keyword(KeywordType::Else)) {
                    parse_body(data)
                } else {
                    None
                };

            Some(
                CXExpr::If {
                    condition: Box::new(expr),
                    then_branch: Box::new(then_body),
                    else_branch: else_body.map(|b| Box::new(b))
                }
            )
        },
        KeywordType::Do => {
            let body = parse_body(data)?;
            assert_token_matches!(data, Token::Keyword(KeywordType::While));
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

            Some(
                CXExpr::While {
                    condition: Box::new(expr),
                    body: Box::new(body),
                    pre_eval: false,
                }
            )
        },
        KeywordType::While => {
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenParen));
            let expr = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            let body = parse_body(data)?;

            Some(
                CXExpr::While {
                    condition: Box::new(expr),
                    body: Box::new(body),
                    pre_eval: true
                }
            )
        },
        KeywordType::Break => Some(CXExpr::Break),
        KeywordType::Continue => Some(CXExpr::Continue),
        KeywordType::For => {
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenParen));

            let init = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

            let condition = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

            let increment = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

            let body = parse_body(data)?;

            Some(
                CXExpr::For {
                    init: Box::new(init),
                    condition: Box::new(condition),
                    increment: Box::new(increment),
                    body: Box::new(body)
                }
            )
        },

        _ => log_error!("Unsupported keyword: {:#?}", keyword)
    }
}

pub(crate) fn parse_braced_expr(data: &mut ParserData) -> Option<CXExpr> {
    todo!()
}

pub(crate) fn parse_name(data: &mut ParserData) -> Option<String> {
    let Some(Token::Identifier(name)) = data.toks.next() else {
        log_error!("PARSER ERROR: Expected identifier for name found: {:#?}", data.toks.peek());
    };

    Some(name.clone())
}