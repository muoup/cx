mod preparser;
mod macros;

use crate::preparse::preparser::preparse_type_tokens;
use cx_data_ast::parse::parser::TokenIter;
use std::collections::HashMap;

pub enum PreparseTokenType {
    TypeName,
    FunctionName,
    TemplatedTypeName,
    TemplatedFunctionName,
}

pub type PreparseMap = HashMap<String, PreparseTokenType>;

pub fn generate_preparse_map(mut tokens: TokenIter) -> Option<PreparseMap> {
    let mut map = PreparseMap::new();

    preparse_type_tokens(&mut tokens, &mut map)?;
    
    Some(map)
}

