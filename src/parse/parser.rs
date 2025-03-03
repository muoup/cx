use crate::lex::token::PunctuatorType::{CloseBrace, OpenBrace, Semicolon};
use crate::lex::token::Token;
use crate::log_error;
use crate::parse::ast::{Expression, ValueType, AST};
use crate::parse::contextless_expression::{contextualize_lvalue, contextualize_rvalue, maybe_contextualize_rvalue};
use crate::parse::expression::{parse_expression, parse_list, parse_rvals, parse_rvalue};
use crate::parse::global_scope::parse_global_stmt;
use crate::util::{MaybeResult, ScopedMap};

pub(crate) type VarTable = ScopedMap<ValueType>;

#[derive(Debug, Clone)]
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
        statements: Vec::new()
    };

    while data.toks.peek() != None {
        let stmt = parse_global_stmt(data)?;

        // println!("{:#?}", stmt);

        ast.statements.push(stmt);
    }

    Some(ast)
}

pub(crate) fn parse_body(data: &mut ParserData) -> Vec<Expression> {
    if data.toks.peek() == Some(&&Token::Punctuator(OpenBrace)) {
        data.toks.next();

        fn parser(data: &mut ParserData) -> Option<Expression> {
            let Some(expr) = parse_expression(data) else {
                println!("Failed to parse expression");
                println!("{:#?}", data.toks.peek());
                return None;
            };

            match maybe_contextualize_rvalue(data, expr) {
                MaybeResult::Consumed(expr)
                    => Some(expr),
                MaybeResult::Unconsumed(expr)
                    => {
                        let lval = contextualize_lvalue(data, expr);

                        if lval.is_none() {
                            log_error!("Body expression failed to parse ending at: {:#?}", data.toks.peek());
                        };

                        lval
                    },
                MaybeResult::Error(err)
                    => log_error!("Error parsing body: {:#?}", err)
            }
        }

        let body = parse_list(
            data,
            Token::Punctuator(Semicolon), Token::Punctuator(CloseBrace),
            parser
        ).unwrap();
        assert_eq!(data.toks.next(), Some(&Token::Punctuator(CloseBrace)));

        body
    } else {
        let body = vec![parse_rvalue(data).unwrap()];

        body
    }
}