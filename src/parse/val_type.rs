use std::fmt::{Debug, Formatter};
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::parser::TokenIter;

#[derive(Clone)]
pub enum ValType {
    Unit,
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

impl Debug for ValType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValType::Unit => write!(f, "void"),
            ValType::Integer { size, signed } => {
                if *signed {
                    write!(f, "u")
                } else {
                    write!(f, "i")
                }.expect("Cannot write signed");

                write!(f, "{}", size * 8)
            },
            ValType::Float { size } => write!(f, "f{}", size * 8),
            ValType::Struct { name } => write!(f, "struct {}", name),
            ValType::Pointer(inner) => write!(f, "{:?}*", inner),
            ValType::Array(inner, size) => write!(f, "{:?}[{}]", inner, size)
        }
    }
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

            KeywordType::Void       => ValType::Unit,

            KeywordType::Unsigned => {
                match parse_type(toks).unwrap() {
                    ValType::Integer { size, signed: true } => ValType::Integer { size, signed: true },
                    ValType::Integer { size: _, signed: false } => panic!("Cannot have unsigned unsigned type"),
                    _ => panic!("Unsigned must be followed by an integer type")
                }
            },

            keyword => panic!("Expected type got {:?}", keyword)
        },
        Token::Identifier(name) => ValType::Struct { name: name.clone() },

        _ => panic!("Expected type")
    };

    while let Some(&Token::Operator(OperatorType::Multiply)) = toks.peek() {
        toks.next();
        base = ValType::Pointer(Box::new(base));
    }

    while let Some(&Token::Punctuator(PunctuatorType::OpenBracket)) = toks.peek() {
        toks.next();
        if let Token::IntLiteral(size) = *toks.next().unwrap() {
            assert_eq!(toks.next(), Some(&Token::Punctuator(PunctuatorType::CloseBracket)));
            base = ValType::Array(Box::new(base), size as usize);
        } else {
            base = ValType::Pointer(Box::new(base));
        }
    }

    Some(base)
}