use cx_data_ast::{assert_token_matches, try_next};
use cx_data_lexer::token::{OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXInitIndex};
use cx_data_ast::parse::parser::ParserData;
use crate::parse::expression::parse_expr;

pub(crate) fn parse_structured_initialization(data: &mut ParserData) -> Option<CXExpr> {
    let init_index = data.tokens.index;
    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenBrace));
    
    let mut inits = Vec::new();
    
    while !try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBrace)) {
        let field_name = if try_next!(data.tokens, TokenKind::Operator(OperatorType::Access)) {
            assert_token_matches!(data.tokens, TokenKind::Identifier(field_name));
            let field_name = field_name.clone();
            assert_token_matches!(data.tokens, TokenKind::Assignment(None));
            Some(field_name)
        } else { None };
        
        data.change_comma_mode(false);
        let val = parse_expr(data)?;
        data.pop_comma_mode();
        
        inits.push(CXInitIndex { name: field_name, value: val, index: 0 });
        
        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            // If we didn't find a comma, it must be the end of the initializer list
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBrace));
            break;
        }
    }
    
    Some(
        CXExprKind::InitializerList { indices: inits, }
            .into_expr(
                init_index,
                data.tokens.index,
            )
    )
}