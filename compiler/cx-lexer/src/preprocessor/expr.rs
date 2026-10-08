use cx_log::CXResult;
use cx_log::catalogue::parse::EVAL_EXPRESSION;
use cx_tokens::token::{OperatorType, PunctuatorType, Token, TokenKind};

use crate::{context::LexingContext, lexer::scanner::tokenize_text};

mod value;

use value::IntegerValue;

pub(crate) fn eval(
    context: &LexingContext,
    expression: &str,
    directive_start: usize,
) -> CXResult<bool> {
    let expanded = expand_defined_ops(context, expression);
    let frame = context.current_frame();

    let mut tokens = tokenize_text(&expanded, frame.file_path.as_path(), frame.language_mode)?;
    tokens = context.expand_macros(tokens);
    let mut parser = PreprocessorExprParser {
        tokens: &tokens,
        index: 0,
    };

    match parser.parse_expression() {
        Ok(value) => Ok(value.value != 0),
        Err(reason) => {
            frame
                .cursor_view()
                .log_error(directive_start, &EVAL_EXPRESSION, reason.into())
        }
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
}

#[derive(Clone, Copy)]
struct PreprocessorBinOp {
    token_count: usize,
    precedence: u8,
    operator: OperatorType,
}

impl PreprocessorExprParser<'_> {
    fn parse_expression(&mut self) -> Result<IntegerValue, &'static str> {
        self.parse_conditional(true)
    }

    fn parse_conditional(&mut self, evaluate: bool) -> Result<IntegerValue, &'static str> {
        let condition = self.parse_binary(0, evaluate)?;
        if !self.consume_punctuator(PunctuatorType::QuestionMark) {
            return Ok(condition);
        }

        let true_value = self.parse_conditional(evaluate && condition.value != 0)?;
        if !self.consume_punctuator(PunctuatorType::Colon) {
            return Err("expected ':' in conditional expression");
        }
        let false_value = self.parse_conditional(evaluate && condition.value == 0)?;

        Ok(IntegerValue {
            value: if condition.value != 0 {
                true_value.value
            } else {
                false_value.value
            },
            signed: true_value.signed && false_value.signed,
        })
    }

    fn parse_binary(
        &mut self,
        min_precedence: u8,
        evaluate: bool,
    ) -> Result<IntegerValue, &'static str> {
        let mut lhs = self.parse_unary(evaluate)?;

        while let Some(op) = self.peek_binop() {
            if op.precedence < min_precedence {
                break;
            }

            self.index += op.token_count;
            let evaluate_rhs = evaluate
                && match op.operator {
                    OperatorType::DoubleAmpersand => lhs.value != 0,
                    OperatorType::DoubleBar => lhs.value == 0,
                    _ => true,
                };
            let rhs = self.parse_binary(op.precedence + 1, evaluate_rhs)?;
            lhs = lhs.binary(op.operator, rhs, evaluate)?;
        }

        Ok(lhs)
    }

    fn parse_unary(&mut self, evaluate: bool) -> Result<IntegerValue, &'static str> {
        if let Some(TokenKind::Operator(operator)) =
            self.tokens.get(self.index).map(|token| &token.kind)
            && matches!(
                operator,
                OperatorType::Exclamation
                    | OperatorType::Minus
                    | OperatorType::Plus
                    | OperatorType::Tilda
            )
        {
            let operator = *operator;
            self.index += 1;
            return self.parse_unary(evaluate)?.unary(operator, evaluate);
        }

        self.parse_primary(evaluate)
    }

    fn parse_primary(&mut self, evaluate: bool) -> Result<IntegerValue, &'static str> {
        match self.tokens.get(self.index).map(|token| &token.kind) {
            Some(TokenKind::IntLiteral(literal)) => {
                self.index += 1;
                IntegerValue::literal(*literal)
            }
            Some(TokenKind::Identifier(_)) => {
                self.index += 1;
                Ok(IntegerValue::boolean(false))
            }
            Some(TokenKind::Punctuator(PunctuatorType::OpenParen)) => {
                self.index += 1;
                let value = self.parse_conditional(evaluate)?;
                if !self.consume_punctuator(PunctuatorType::CloseParen) {
                    return Err("expected ')' in expression");
                }
                Ok(value)
            }
            Some(_) => Err("expected an integer operand"),
            None => Err("expected an operand before end of expression"),
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
            return Some(binop(2, 8, OperatorType::LShift));
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
            return Some(binop(2, 8, OperatorType::RShift));
        }

        let TokenKind::Operator(operator) = kind else {
            return None;
        };

        let precedence = match operator {
            OperatorType::DoubleBar => 1,
            OperatorType::DoubleAmpersand => 2,
            OperatorType::Bar => 3,
            OperatorType::Caret => 4,
            OperatorType::Ampersand => 5,
            OperatorType::Equal | OperatorType::NotEqual => 6,
            OperatorType::Less
            | OperatorType::LessEqual
            | OperatorType::Greater
            | OperatorType::GreaterEqual => 7,
            OperatorType::LShift | OperatorType::RShift => 8,
            OperatorType::Plus | OperatorType::Minus => 9,
            OperatorType::Asterisk | OperatorType::Slash | OperatorType::Percent => 10,
            _ => return None,
        };
        Some(binop(1, precedence, *operator))
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

fn binop(token_count: usize, precedence: u8, operator: OperatorType) -> PreprocessorBinOp {
    PreprocessorBinOp {
        token_count,
        precedence,
        operator,
    }
}
