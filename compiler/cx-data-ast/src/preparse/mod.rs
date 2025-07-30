use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum PreparseTokenType {
    TypeName,
    FunctionName,
    TemplatedTypeName,
    TemplatedFunctionName,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXPreparseToken {
    pub token_type: PreparseTokenType
}

pub type PreparseTokenMap = HashMap<String, CXPreparseToken>;

pub type CXPreparseTokens = Vec<CXPreparseToken>;

pub type PreparseMap = HashMap<String, PreparseTokenType>;