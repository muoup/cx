use std::collections::HashMap;

use crate::ast::{
    expression::CXExpression, function::CXFunctionPrototype, global_var::CXGlobalVariable,
    modifiers::VisibilityMode, template::CXTemplatePrototype, types::CXType,
};

#[derive(Debug, Clone)]
pub struct CXSymbol {
    pub visibility: VisibilityMode,
    pub kind: CXSymbolKind,
}

impl CXSymbol {
    pub fn new(visibility: VisibilityMode, kind: CXSymbolKind) -> Self {
        Self { visibility, kind }
    }
}

#[derive(Debug, Clone)]
pub enum CXSymbolKind {
    Type(CXType),
    Expression {
        expr: CXExpression,
        is_constexpr: bool,
    },
    TypeTemplate {
        input: CXTemplatePrototype,
        definition: CXType,
    },
    FunctionTemplate {
        input: CXTemplatePrototype,
        definition: CXFunctionPrototype,
        body: Box<CXExpression>,
    },
    // Templated variables should not be supported
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    symbols: HashMap<String, CXSymbol>,
}

impl SymbolNamespaceData {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn insert_symbol(&mut self, name: impl Into<String>, symbol: CXSymbol) {
        self.symbols.insert(name.into(), symbol);
    }

    pub fn get_symbol(&self, name: &str) -> Option<&CXSymbol> {
        self.symbols.get(name)
    }
}
