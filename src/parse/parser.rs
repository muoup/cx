use crate::lex::token::{PunctuatorType, Token};
use crate::{assert_token_matches, log_error};
use crate::parse::ast::{Expression, ValueType, AST};
use crate::parse::ast::GlobalStatement::HandledInternally;
use crate::parse::contextless_expression::{contextualize_lvalue, contextualize_rvalue, maybe_contextualize_rvalue};
use crate::parse::expression::{parse_expression, parse_list, parse_rvals, parse_rvalue};
use crate::parse::global_scope::parse_global_stmt;
use crate::util::{MaybeResult, ScopedMap};

pub(crate) type VarTable = ScopedMap<ValueType>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum VisibilityMode {
    Package,
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub(crate) struct ParserData<'a> {
    pub(crate) toks: TokenIter<'a>,
    pub(crate) visibility: VisibilityMode,
}

#[derive(Debug, Clone)]
pub(crate) struct TokenIter<'a> {
    pub(crate) slice: &'a [Token],
    pub(crate) index: usize,
}

impl<'a> TokenIter<'_> {
    pub(crate) fn next(&mut self) -> Option<&Token> {
        let next = self.slice.get(self.index);
        self.index += 1;
        next
    }

    pub(crate) fn peek(&self) -> Option<&Token> {
        self.slice.get(self.index)
    }

    pub(crate) fn back(&mut self) {
        self.index -= 1;
    }

    pub(crate) fn has_next(&self) -> bool {
        self.slice.get(self.index).is_some()
    }
}

pub(crate) fn parse_ast(data: &mut ParserData) -> Option<AST> {
    let mut ast = AST {
        statements: Vec::new(),
        public_interface: Vec::new()
    };

    while data.toks.peek() != None {
        let Some(stmt) = parse_global_stmt(data) else {
            log_error!("Failed to parse global statement: {:#?}", data.toks.peek());
        };

        if matches!(stmt, HandledInternally) {
            continue;
        }

        ast.statements.push(stmt);

        if data.visibility == VisibilityMode::Public {
            ast.public_interface.push(ast.statements.len() - 1);
        }
    }

    Some(ast)
}

pub(crate) fn parse_body_expr(data: &mut ParserData) -> Option<Expression> {
    let first = data.toks.peek().cloned();

    let Some(expr) = parse_expression(data) else {
        log_error!("Failed to parse expression: {:#?}", first?);
    };

    match maybe_contextualize_rvalue(data, expr) {
        MaybeResult::Consumed(expr)
            => Some(expr),
        MaybeResult::Unconsumed(expr)
            => {
                let Some(lval) = contextualize_lvalue(data, expr) else {
                    log_error!("Body expression failed to parse ending at: {:#?}", data.toks.peek());
                };

                assert_token_matches!(data, Token::Punctuator(Semicolon));

                Some(lval)
            },
        MaybeResult::Error(err)
            => log_error!("Error parsing body: {:#?}", err)
    }
}

pub(crate) fn parse_body(data: &mut ParserData) -> Vec<Expression> {
    match data.toks.peek() {
        Some(&Token::Punctuator(PunctuatorType::Semicolon)) => {
            data.toks.next();
            vec![]
        },
        Some(&Token::Punctuator(PunctuatorType::OpenBrace)) => {
            data.toks.next();

            let body = parse_list(
                data,
                Token::Punctuator(PunctuatorType::Semicolon), Token::Punctuator(PunctuatorType::CloseBrace),
                parse_body_expr
            ).unwrap();
            assert_eq!(data.toks.next(), Some(&Token::Punctuator(PunctuatorType::CloseBrace)));

            body
        },
        _ => {
            vec![parse_body_expr(data).unwrap()]
        }
    }
}