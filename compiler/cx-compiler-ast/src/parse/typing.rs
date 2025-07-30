use cx_data_ast::lex::token::TokenKind;
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::value_type::CXType;
use cx_data_ast::keyword;

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXType,
}

pub fn is_type_decl(data: &mut ParserData) -> bool {
    let tok = data.tokens.peek();

    if tok.is_none() {
        return false;
    }

    match &tok.unwrap().kind {
        TokenKind::Intrinsic(_) |
        TokenKind::Specifier(_) |
        keyword!(Struct, Union, Enum) => true,

        TokenKind::Identifier(name) => data.ast.type_map.contains_key(name),

        _ => false
    }
}