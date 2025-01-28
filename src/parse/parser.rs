use crate::lex::token::Punctuator::{CloseParen, OpenBrace, OpenParen};
use crate::lex::token::{KeywordType, Token};
use crate::parse::ast::{Expression, FunctionDeclaration, Node, Root, AST};

pub fn parse_ast(toks: &[Token]) -> Option<AST> {
    Some(
        AST {
            root: parse_root(toks)?
        }
    )
}

fn parse_root(toks: &[Token]) -> Option<Root> {
    Some(
        Root {
            fn_declarations: parse_fn_declarations(toks)?
        }
    )
}

fn parse_fn_declarations(toks: &[Token]) -> Option<Vec<FunctionDeclaration>> {
    let mut fns = Vec::new();
    fns.push(parse_fn_declaration(toks)?);
    Some(fns)
}

fn parse_fn_declaration(toks: &[Token]) -> Option<FunctionDeclaration> {
    let return_type = match &toks[0] {
        Token::Keyword(keyword) => *keyword,
        _ => return None
    };
    let name = match &toks[1] {
        Token::Identifier(name) => name.clone(),
        _ => return None
    };
    assert_eq!(toks.get(2), Some(&Token::Punctuator(OpenParen)));
    assert_eq!(toks.get(3), Some(&Token::Punctuator(CloseParen)));
    let body = parse_fn_body(&toks[4..])?;
    Some(
        FunctionDeclaration {
            return_type,
            name,
            body
        }
    )
}

fn parse_fn_body(toks: &[Token]) -> Option<Vec<Box<dyn Node>>> {
    assert_eq!(toks.get(0), Some(&Token::Punctuator(OpenBrace)));

    let mut statement = Vec::new();

    statement.push(parse_statement(&toks[1..])?);

    Some(statement)
}

fn parse_statement(toks: &[Token]) -> Option<Box<dyn Node>> {
    match toks.get(0)? {
        Token::Keyword(keyword) => {
            match keyword {
                KeywordType::Return => {
                    let value = parse_expression(&toks[1..])?;
                    Some(Box::new(Expression::Return(Box::new(value))))
                }
                _ => None
            }
        },
        _ => None
    }
}

fn parse_expression(toks: &[Token]) -> Option<Expression> {
    match toks.get(0)? {
        Token::IntLiteral(value) => Some(Expression::IntLiteral(*value)),
        Token::FloatLiteral(value) => Some(Expression::FloatLiteral(*value)),
        _ => None
    }
}