use std::clone;
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error, point_log_error, try_next, try_token_matches};
use crate::mangling::namespace_mangle;
use crate::parse::macros::error_pointer;
use crate::parse::parser::{ParserData};
use crate::parse::pass_ast::{CXBinOp, CXExpr, CXUnOp};
use crate::parse::pass_ast::global_scope::parse_body;
use crate::parse::pass_ast::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use crate::parse::pass_ast::operators::{binop_prec, parse_binop, parse_pre_unop};
use crate::parse::value_type::CXValType;

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
        return None;
    };

    expr_stack.push(expr);

    loop {
        let Some(op) = parse_binop(data) else {
            if matches!(expr_stack.last(), Some(CXExpr::Identifier(_))) {
                let CXExpr::Identifier(val_type) = expr_stack.pop().unwrap() else {
                    log_error!("PARSER ERROR: Expected identifier for initialization type, found {:?}", expr_stack.last());
                };

                let Some(val) = parse_expr_val(data) else {
                    expr_stack.push(CXExpr::Identifier(val_type));
                    break;
                };

                let CXExpr::Identifier(val_name) = val else {
                    log_error!("PARSER ERROR: Expected identifier for initialization name, found {:?}", val);
                };

                expr_stack.push(
                    CXExpr::VarDeclaration {
                        name: val_name,
                        type_: CXValType::Identifier(val_type)
                    }
                );

                continue;
            }

            break;
        };

        let op_prec = binop_prec(op.clone());

        compress_stack(&mut expr_stack, &mut op_stack, op_prec)?;

        let Some(expr) = parse_expr_val(data) else {
            log_error!("Failed to parse expression value after operator: {:#?}", data.toks.peek());
        };

        op_stack.push(op);
        expr_stack.push(expr);
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

pub(crate) fn compress_stack(expr_stack: &mut Vec<CXExpr>, op_stack: &mut Vec<CXBinOp>, rprec: u8) -> Option<()> {
    if op_stack.is_empty() {
        return Some(());
    }

    let stop = op_stack.iter()
        .rposition(|e| binop_prec(e.clone()) >= rprec)
        .map(|e| op_stack.len() - e - 1)
        .unwrap_or(0);

    if op_stack.len() == stop {
        return Some(());
    }

    let mut exprs = Vec::new();
    let mut ops = Vec::new();

    exprs.push(expr_stack.pop().unwrap());

    for _ in 0 .. (op_stack.len() - stop) {
        exprs.push(expr_stack.pop().unwrap());
        ops.push(op_stack.pop().unwrap());
    }

    exprs.reverse();
    ops.reverse();

    let mut acc = exprs.pop().unwrap();

    while !exprs.is_empty() {
        let Some(op) = ops.pop() else {
            log_error!("PARSER ERROR: Operator stack is empty when trying to pop operator");
        };
        let Some(left) = exprs.pop() else {
            log_error!("PARSER ERROR: Expression stack is empty when trying to pop left operand");
        };

        let temp_acc = std::mem::replace(&mut acc, CXExpr::Taken);

        acc = CXExpr::BinOp {
            lhs: Box::new(left),
            rhs: Box::new(temp_acc),
            op
        };
    }

    expr_stack.push(acc);

    Some(())
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

        _ => {
            data.back();
            return None
        }
    };

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
            let val = parse_expr(data);

            Some(
                CXExpr::Return {
                    value: val.map(|v| Box::new(v))
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