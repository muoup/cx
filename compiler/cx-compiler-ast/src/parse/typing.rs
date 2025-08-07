use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::maps::CXTypeMap;
use cx_data_lexer::token::TokenKind;
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::type_mapping::contextualize_type;
use cx_data_ast::parse::value_type::CXType;
use cx_data_lexer::{identifier, intrinsic, keyword, specifier};
use cx_util::{point_log_error, CXResult};
use crate::preparse::typing::parse_initializer;

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
        intrinsic!() |
        specifier!() |
        keyword!(Struct, Union, Enum) => true,
        
        identifier!(name) if data.ast.type_map.has_template(name) => true,
        identifier!(name) if data.ast.type_map.contains_key(name) => true,

        _ => false
    }
}

pub fn parse_contextualized_initializer(data: &mut ParserData) -> CXResult<(Option<CXIdent>, CXType)> {
    let (name, type_) = parse_initializer(&mut data.tokens)?;
    let Some(cx_type) = contextualize_type(&data.ast.type_map, &type_) else {
        point_log_error!(data.tokens, "Failed to contextualize type: {:#?}", type_);
    };

    Some((name, cx_type))
}