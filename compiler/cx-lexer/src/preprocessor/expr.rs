use std::collections::HashMap;

use cx_tokens::token::{OperatorType, PunctuatorType, Token, TokenKind};
use cx_util::CXResult;

use crate::{context::{LexingContext, Macro}, lexer::scanner::tokenize_text};

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

impl PreprocessorExprParser<'_> {
    fn parse_expression(&mut self) -> Option<i64> {
        self.parse_conditional()
    }

    fn parse_conditional(&mut self) -> Option<i64> {
        let condition = self.parse_logical_or()?;
        if !self.consume_punctuator(PunctuatorType::QuestionMark) {
            return Some(condition);
        }

        let true_value = self.parse_expression()?;
        if !self.consume_punctuator(PunctuatorType::Colon) {
            return None;
        }
        let false_value = self.parse_expression()?;

        Some(if condition != 0 {
            true_value
        } else {
            false_value
        })
    }

    fn parse_logical_or(&mut self) -> Option<i64> {
        let mut lhs = self.parse_logical_and()?;
        while self.consume_operator(OperatorType::DoubleBar) {
            let rhs = self.parse_logical_and()?;
            lhs = i64::from(lhs != 0 || rhs != 0);
        }
        Some(lhs)
    }

    fn parse_logical_and(&mut self) -> Option<i64> {
        let mut lhs = self.parse_equality()?;
        while self.consume_operator(OperatorType::DoubleAmpersand) {
            let rhs = self.parse_equality()?;
            lhs = i64::from(lhs != 0 && rhs != 0);
        }
        Some(lhs)
    }

    fn parse_equality(&mut self) -> Option<i64> {
        let mut lhs = self.parse_relational()?;
        loop {
            if self.consume_operator(OperatorType::Equal) {
                lhs = i64::from(lhs == self.parse_relational()?);
            } else if self.consume_operator(OperatorType::NotEqual) {
                lhs = i64::from(lhs != self.parse_relational()?);
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_relational(&mut self) -> Option<i64> {
        let mut lhs = self.parse_additive()?;
        loop {
            if self.consume_operator(OperatorType::Less) {
                lhs = i64::from(lhs < self.parse_additive()?);
            } else if self.consume_operator(OperatorType::LessEqual) {
                lhs = i64::from(lhs <= self.parse_additive()?);
            } else if self.consume_operator(OperatorType::Greater) {
                lhs = i64::from(lhs > self.parse_additive()?);
            } else if self.consume_operator(OperatorType::GreaterEqual) {
                lhs = i64::from(lhs >= self.parse_additive()?);
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_additive(&mut self) -> Option<i64> {
        let mut lhs = self.parse_multiplicative()?;
        loop {
            if self.consume_operator(OperatorType::Plus) {
                lhs += self.parse_multiplicative()?;
            } else if self.consume_operator(OperatorType::Minus) {
                lhs -= self.parse_multiplicative()?;
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Option<i64> {
        let mut lhs = self.parse_unary()?;
        loop {
            if self.consume_operator(OperatorType::Asterisk) {
                lhs *= self.parse_unary()?;
            } else if self.consume_operator(OperatorType::Slash) {
                lhs /= self.parse_unary()?;
            } else if self.consume_operator(OperatorType::Percent) {
                lhs %= self.parse_unary()?;
            } else {
                return Some(lhs);
            }
        }
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
                let value = self.parse_expression()?;
                if !self.consume_punctuator(PunctuatorType::CloseParen) {
                    return None;
                }
                Some(value)
            }
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
