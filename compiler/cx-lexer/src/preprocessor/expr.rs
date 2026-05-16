use std::collections::HashMap;

use cx_tokens::token::{OperatorType, PunctuatorType, Token, TokenKind};
use cx_util::CXResult;

use crate::{
    context::{LexingContext, Macro},
    lexer::scanner::tokenize_text,
};

pub(crate) fn eval(
    context: &LexingContext,
    expression: &str,
    directive_start: usize,
) -> CXResult<bool> {
    let expanded = expand_defined_ops(context, expression);
    let frame = context.current_frame();

    let mut tokens = tokenize_text(&expanded, frame.file_path.as_path())?;
    tokens = context.expand_macros(tokens);
    let mut parser = PreprocessorExprParser {
        tokens: &tokens,
        index: 0,
        macros: &context.macros,
    };

    match parser.parse_expression() {
        Some(value) => Ok(value != 0),
        None => log_lexer_error!(
            frame.file_path.as_path(),
            &frame.source,
            directive_start,
            frame.cursor,
            "Failed to evaluate preprocessor expression"
        ),
    }
}

fn expand_defined_ops(context: &LexingContext, expression: &str) -> String {
    let mut result = String::new();
    let bytes = expression.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        if expression[index..].starts_with("defined") {
            let after = index + "defined".len();
            let prev_ok = index == 0
                || !expression[..index]
                    .chars()
                    .last()
                    .map(|c| c.is_ascii_alphanumeric() || c == '_')
                    .unwrap_or(false);
            let next_ok = after >= bytes.len()
                || !expression[after..]
                    .chars()
                    .next()
                    .map(|c| c.is_ascii_alphanumeric() || c == '_')
                    .unwrap_or(false);

            if prev_ok
                && next_ok
                && let Some((name, next_index)) = parse_defined_operand(expression, after)
            {
                result.push_str(if context.macros.contains_key(&name) {
                    "1"
                } else {
                    "0"
                });
                index = next_index;
                continue;
            }
        }

        result.push(bytes[index] as char);
        index += 1;
    }

    result
}

fn parse_defined_operand(expression: &str, mut index: usize) -> Option<(String, usize)> {
    index = skip_ascii_whitespace(expression, index);
    if expression.as_bytes().get(index) == Some(&b'(') {
        index += 1;
        index = skip_ascii_whitespace(expression, index);
        let (name, next) = parse_ident(expression, index)?;
        index = skip_ascii_whitespace(expression, next);
        if expression.as_bytes().get(index) != Some(&b')') {
            return None;
        }
        return Some((name, index + 1));
    }

    parse_ident(expression, index)
}

fn skip_ascii_whitespace(expression: &str, mut index: usize) -> usize {
    while expression
        .as_bytes()
        .get(index)
        .map(|byte| byte.is_ascii_whitespace())
        .unwrap_or(false)
    {
        index += 1;
    }
    index
}

fn parse_ident(expression: &str, index: usize) -> Option<(String, usize)> {
    let bytes = expression.as_bytes();
    let first = *bytes.get(index)?;
    if !(first.is_ascii_alphabetic() || first == b'_') {
        return None;
    }

    let mut end = index + 1;
    while bytes
        .get(end)
        .map(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
        .unwrap_or(false)
    {
        end += 1;
    }

    Some((expression[index..end].to_string(), end))
}

struct PreprocessorExprParser<'a> {
    tokens: &'a [Token],
    index: usize,
    macros: &'a HashMap<String, Macro>,
}

#[derive(Clone, Copy)]
struct PreprocessorBinOp {
    token_count: usize,
    precedence: u8,
    apply: fn(i64, i64) -> i64,
}

impl PreprocessorExprParser<'_> {
    fn parse_expression(&mut self) -> Option<i64> {
        self.parse_conditional()
    }

    fn parse_conditional(&mut self) -> Option<i64> {
        let condition = self.parse_binary(0)?;
        if !self.consume_punctuator(PunctuatorType::QuestionMark) {
            return Some(condition);
        }

        let true_value = self.parse_conditional()?;
        if !self.consume_punctuator(PunctuatorType::Colon) {
            return None;
        }
        let false_value = self.parse_conditional()?;

        Some(if condition != 0 {
            true_value
        } else {
            false_value
        })
    }

    fn parse_binary(&mut self, min_precedence: u8) -> Option<i64> {
        let mut lhs = self.parse_unary()?;

        while let Some(op) = self.peek_binop() {
            if op.precedence < min_precedence {
                break;
            }

            self.index += op.token_count;
            let rhs = self.parse_binary(op.precedence + 1)?;
            lhs = (op.apply)(lhs, rhs);
        }

        Some(lhs)
    }

    fn parse_unary(&mut self) -> Option<i64> {
        if self.consume_operator(OperatorType::Exclamation) {
            return Some(i64::from(self.parse_unary()? == 0));
        }
        if self.consume_operator(OperatorType::Minus) {
            return Some(-self.parse_unary()?);
        }
        if self.consume_operator(OperatorType::Plus) {
            return self.parse_unary();
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Option<i64> {
        match self.tokens.get(self.index).map(|token| &token.kind)? {
            TokenKind::IntLiteral(value) => {
                self.index += 1;
                Some(*value)
            }
            TokenKind::Identifier(name) => {
                self.index += 1;
                match self.macros.get(name) {
                    Some(Macro::Object(body)) if body.len() == 1 => match body[0].kind {
                        TokenKind::IntLiteral(value) => Some(value),
                        _ => Some(0),
                    },
                    _ => Some(0),
                }
            }
            TokenKind::Punctuator(PunctuatorType::OpenParen) => {
                self.index += 1;
                let value = self.parse_conditional()?;
                if !self.consume_punctuator(PunctuatorType::CloseParen) {
                    return None;
                }
                Some(value)
            }
            _ => None,
        }
    }

    fn peek_binop(&self) -> Option<PreprocessorBinOp> {
        let kind = self.tokens.get(self.index).map(|token| &token.kind)?;

        if matches!(
            (
                kind,
                self.tokens.get(self.index + 1).map(|token| &token.kind)
            ),
            (
                TokenKind::Operator(OperatorType::Less),
                Some(TokenKind::Operator(OperatorType::Less))
            )
        ) {
            return Some(binop(2, 8, |lhs, rhs| lhs << rhs));
        }

        if matches!(
            (
                kind,
                self.tokens.get(self.index + 1).map(|token| &token.kind)
            ),
            (
                TokenKind::Operator(OperatorType::Greater),
                Some(TokenKind::Operator(OperatorType::Greater))
            )
        ) {
            return Some(binop(2, 8, |lhs, rhs| lhs >> rhs));
        }

        let TokenKind::Operator(operator) = kind else {
            return None;
        };

        match operator {
            OperatorType::DoubleBar => {
                Some(binop(1, 1, |lhs, rhs| i64::from(lhs != 0 || rhs != 0)))
            }
            OperatorType::DoubleAmpersand => {
                Some(binop(1, 2, |lhs, rhs| i64::from(lhs != 0 && rhs != 0)))
            }
            OperatorType::Bar => Some(binop(1, 3, |lhs, rhs| lhs | rhs)),
            OperatorType::Caret => Some(binop(1, 4, |lhs, rhs| lhs ^ rhs)),
            OperatorType::Ampersand => Some(binop(1, 5, |lhs, rhs| lhs & rhs)),
            OperatorType::Equal => Some(binop(1, 6, |lhs, rhs| i64::from(lhs == rhs))),
            OperatorType::NotEqual => Some(binop(1, 6, |lhs, rhs| i64::from(lhs != rhs))),
            OperatorType::Less => Some(binop(1, 7, |lhs, rhs| i64::from(lhs < rhs))),
            OperatorType::LessEqual => Some(binop(1, 7, |lhs, rhs| i64::from(lhs <= rhs))),
            OperatorType::Greater => Some(binop(1, 7, |lhs, rhs| i64::from(lhs > rhs))),
            OperatorType::GreaterEqual => Some(binop(1, 7, |lhs, rhs| i64::from(lhs >= rhs))),
            OperatorType::Plus => Some(binop(1, 9, |lhs, rhs| lhs + rhs)),
            OperatorType::Minus => Some(binop(1, 9, |lhs, rhs| lhs - rhs)),
            OperatorType::Asterisk => Some(binop(1, 10, |lhs, rhs| lhs * rhs)),
            OperatorType::Slash => Some(binop(1, 10, |lhs, rhs| lhs / rhs)),
            OperatorType::Percent => Some(binop(1, 10, |lhs, rhs| lhs % rhs)),
            _ => None,
        }
    }

    fn consume_operator(&mut self, operator: OperatorType) -> bool {
        if matches!(
            self.tokens.get(self.index).map(|token| &token.kind),
            Some(TokenKind::Operator(op)) if *op == operator
        ) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn consume_punctuator(&mut self, punctuator: PunctuatorType) -> bool {
        if matches!(
            self.tokens.get(self.index).map(|token| &token.kind),
            Some(TokenKind::Punctuator(punc)) if *punc == punctuator
        ) {
            self.index += 1;
            true
        } else {
            false
        }
    }
}

fn binop(token_count: usize, precedence: u8, apply: fn(i64, i64) -> i64) -> PreprocessorBinOp {
    PreprocessorBinOp {
        token_count,
        precedence,
        apply,
    }
}
