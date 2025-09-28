use cx_data_lexer::token::TokenKind;
use cx_data_ast::parse::parser::ParserData;
use cx_data_lexer::{identifier, intrinsic, keyword, specifier};
use cx_data_typechecker::intrinsic_types::is_intrinsic_type;

pub fn is_type_decl(data: &mut ParserData) -> bool {
    let tok = data.tokens.peek();

    if tok.is_none() {
        return false;
    }

    match &tok.unwrap().kind {
        intrinsic!() |
        specifier!() |
        keyword!(Struct, Union, Enum) => true,
        
        identifier!(name) if is_intrinsic_type(name) => true,
        identifier!(name) if data.ast.type_map.templates.contains_key(name) => true,
        identifier!(name) if data.ast.type_map.standard.contains_key(name) => true,

        _ => false
    }
}