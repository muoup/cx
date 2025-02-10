/*
 *  Use Cases:
 *    - Function Declaration, first parse should be to detect [type] [name w/ modifiers]
 *    - Expression Parsing, should give either [identifier] or [identifier] [name w/ modifiers]
 *    - Type Declaration, should give [type] [optional name w/ modifiers]
 */
use crate::lex::token::Token;
use crate::parse::ast::{Expression, UnverifiedExpression};
use crate::parse::parser::TokenIter;

/**
 *  This function is essentially a function for the sake of backwards compatibility. While modern
 *  languages will have clear types like "u32" or "i32", C has the possibility of multi-word types
 *  like "unsigned int" or "long long". This function will handle those cases.
 */
pub fn parse_identifier(toks: &mut TokenIter) -> Option<String> {
    let mut accumulator = String::new();

    while let Some(Token::Intrinsic(name)) = toks.peek() {
        accumulator.push_str(format!("{:?} ", name).to_lowercase().as_str());
        toks.next();
    }

    accumulator.pop(); // Remove trailing space
    Some(accumulator)
}