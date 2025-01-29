use crate::lex::token::{KeywordType, OperatorType, Token};
use crate::parse::parser::TokenIter;

#[derive(Debug, Clone)]
pub enum ValType {
    Integer {
        size: u8,
        signed: bool
    },
    Float {
        size: u8
    },
    Struct {
        name: String
    },

    Pointer(Box<ValType>),
    Array(Box<ValType>, usize)
}

pub fn parse_type(toks: &mut TokenIter) -> Option<ValType> {
    let mut base = match toks.next()? {
        Token::Keyword(keyword) => match keyword {
            KeywordType::Bool       => ValType::Integer { size: 1, signed: true },
            KeywordType::Char       => ValType::Integer { size: 1, signed: true },
            KeywordType::Short      => ValType::Integer { size: 2, signed: true },
            KeywordType::Int        => ValType::Integer { size: 4, signed: true },
            KeywordType::Long       => ValType::Integer { size: 8, signed: true },

            KeywordType::Float      => ValType::Float   { size: 4 },
            KeywordType::Double     => ValType::Float   { size: 8 },

            KeywordType::Unsigned => {
                match parse_type(toks).unwrap() {
                    ValType::Integer { size, signed: true } => ValType::Integer { size, signed: true },
                    ValType::Integer { size: _, signed: false } => panic!("Cannot have unsigned unsigned type"),
                    _ => panic!("Unsigned must be followed by an integer type")
                }
            },

            _ => panic!("Expected type")
        },
        Token::Identifier(name) => ValType::Struct { name: name.clone() },

        _ => panic!("Expected type")
    };

    while let Some(&Token::Operator(OperatorType::Multiply)) = toks.peek() {
        toks.next();
        base = ValType::Pointer(Box::new(base));
    }

    Some(base)
}