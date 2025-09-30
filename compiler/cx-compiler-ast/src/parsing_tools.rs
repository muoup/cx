use cx_data_ast::try_next;
use cx_data_lexer::{punctuator, TokenIter};

pub fn goto_statement_end(tokens: &mut TokenIter) -> Option<()> {
    let mut bracket_stack = 0;
    
    while let Some(token) = tokens.next() {
        match token.kind {
            punctuator!(OpenBrace) => bracket_stack += 1,
            punctuator!(CloseBrace) => {
                bracket_stack -= 1;
                
                if bracket_stack == 0 {
                    try_next!(tokens, punctuator!(Semicolon));
                    break;
                }
            },
            punctuator!(Semicolon) if bracket_stack == 0 => { break },
            
            _ => ()
        }
    }

    Some(())
}