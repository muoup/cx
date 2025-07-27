use cx_data_ast::assert_token_matches;
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::parser::TokenIter;
use cx_util::point_log_error;
use crate::preparse::{PreparseMap, PreparseTokenType};

pub(crate) fn preparse_type_tokens(tokens: &mut TokenIter, map: &mut PreparseMap) -> Option<()> {
    while tokens.has_next() {
        let mut template_mode = false;
        
        if matches!(tokens.peek()?.kind, TokenKind::Keyword(KeywordType::Template)) {
            tokens.next();
            assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Less));
        }
        
        let mut add_type = |name: &str| {
            if template_mode {
                map.insert(name.to_string(), PreparseTokenType::TemplatedTypeName);
            } else {
                map.insert(name.to_string(), PreparseTokenType::TypeName);
            }
        };
        
        match tokens.next()?.kind {
            TokenKind::Keyword(KeywordType::Struct) |
            TokenKind::Keyword(KeywordType::Enum) |
            TokenKind::Keyword(KeywordType::Union) => {
                if let TokenKind::Identifier(name) = &tokens.peek()?.kind {
                    add_type(name);
                }
            },
            
            TokenKind::Keyword(KeywordType::Typedef) => {
                while !matches!(tokens.next()?.kind, TokenKind::Punctuator(PunctuatorType::Semicolon)) {}
            
                tokens.back();
                
                let TokenKind::Identifier(name) = &tokens.peek()?.kind else {
                    point_log_error!(tokens, "Expected identifier after typedef keyword");
                };

                add_type(name);
            },
            
            _ => ()
        };
        
        tokens.next();
    }
    
    Some(())
}