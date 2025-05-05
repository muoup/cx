use std::clone;
use std::collections::VecDeque;
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::global_scope::parse_body;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr};
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::value_type::CXValType;
use cx_data_ast::{assert_token_matches, try_next};
use crate::parse::lvalues::reformat_lvalue;
use crate::parse::operators::{binop_prec, comma_separated, comma_separated_owned, parse_binop, parse_post_unop, parse_pre_unop};
use cx_util::log_error;

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

    let Some(expr) = parse_expr_val(data) else {
        return Some(CXExpr::Unit);
    };

    expr_stack.push(expr);

    loop {
        if let Some(()) = parse_expr_op_concat(data, &mut expr_stack, &mut op_stack) {
            continue;
        }

        if let Some(val) = parse_expr_val(data) {
            let lhs = expr_stack.pop().unwrap();
            let Some(compound) = form_compound_expr(lhs, val) else {
                log_error!("PARSER ERROR: Failed to form compound expression: {:#?}", data.toks.peek());
            };

            expr_stack.push(compound);
            continue;
        };

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

pub(crate) fn form_compound_expr(lhs: CXExpr, rhs: CXExpr) -> Option<CXExpr> {
    match (lhs, rhs) {
        (CXExpr::Identifier(lident), CXExpr::Identifier(rident)) => {
            Some(
                CXExpr::VarDeclaration {
                    type_: CXValType::Identifier(lident),
                    name: rident
                }
            )
        }

        (lhs, rhs) => log_error!("Invalid compound expression: {:?} {:?}", lhs, rhs)
    }
}

pub(crate) fn parse_expr_op_concat(data: &mut ParserData, expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<CXBinOp>) -> Option<()> {
    let Some(op) = parse_binop(data) else {
        return None;
    };

    match op {
        // TODO: Handle LValue assignment

        _ => {
            let op_prec = binop_prec(op.clone());

            let Some(next_val) = parse_expr_val(data) else {
                log_error!("PARSER ERROR: Failed to parse expression value after operator: {:#?}", data.toks.peek());
            };

            expr_stack.push(next_val);
            op_stack.push(op);

            if op_stack.len() > 1 {
                compress_stack(expr_stack, op_stack, op_prec)?;
            }
        }
    }

    Some(())
}

pub(crate) fn compress_stack(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<CXBinOp>, rprec: u8) -> Option<()> {
    if op_stack.is_empty() {
        return Some(());
    }

    // if matches!(op_stack.last(), Some(CXBinOp::Assign(_))) {
    //     let top = op_stack.pop().unwrap();
    //     compress_lvalue(expr_stack, op_stack)?;
    //     op_stack.push(top);
    // }

    let mut ops = Vec::new();
    let mut exprs = Vec::new();

    while let Some(op2) = op_stack.pop() {
        let op_prec = binop_prec(op2.clone());

        if op_prec > rprec {
            op_stack.push(op2);
            break;
        }

        ops.push(op2);
        exprs.push(expr_stack.pop().unwrap());
    }

    let mut acc = expr_stack.pop().unwrap();

    while let Some(op) = ops.pop() {
        let rhs = exprs.pop().unwrap();

        acc = CXExpr::BinOp {
            lhs: Box::new(acc),
            rhs: Box::new(rhs),
            op
        };
    }

    expr_stack.push(acc);

    Some(())
}

pub(crate) fn compress_lvalue(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<CXBinOp>) -> Option<()> {
    match expr_stack.len() {
        0 => panic!("compress_lvalue reached with no value left of assignment"),
        1 => {
            let new_lvalue = reformat_lvalue(expr_stack.pop().unwrap())?;
            expr_stack.push(new_lvalue);

            Some(())
        },
        2 => {
            let lexpr = expr_stack.pop().unwrap();
            let rexpr = expr_stack.pop().unwrap();
            let op = op_stack.pop().unwrap();

            None
        },

        _ => log_error!("PARSER ERROR: No LValue can be formed from expression stack: {:#?}", expr_stack)
    }
}

pub(crate) fn parse_expr_val(data: &mut ParserData) -> Option<CXExpr> {
    let mut unop_stack = Vec::new();

    while let Some(op) = parse_pre_unop(data) {
        unop_stack.push(op);
    }

    let mut acc = match data.toks.next()? {
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
                return Some(CXExpr::Unit);
            }

            let expr = parse_expr(data)?;
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

            expr
        },
        Token::Punctuator(PunctuatorType::OpenBracket) => {
            if try_next!(data, Token::Punctuator(PunctuatorType::CloseBracket)) {
                return Some(CXExpr::Unit);
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

    while let Some(op) = parse_post_unop(data) {
        unop_stack.push(op);
    }

    for op in unop_stack.into_iter().rev() {
        let temp_acc = std::mem::replace(&mut acc, CXExpr::Taken);
        acc = CXExpr::UnOp {
            operand: Box::new(temp_acc),
            operator: op
        }
    }

    Some(acc)
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
            let expr = parse_expr_val(data)?;
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
        KeywordType::While => {
            let expr = parse_expr_val(data)?;
            let body = parse_body(data)?;

            Some(
                CXExpr::While {
                    condition: Box::new(expr),
                    body: Box::new(body)
                }
            )
        },
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