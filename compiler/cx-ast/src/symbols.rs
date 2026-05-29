use std::collections::HashMap;

use crate::ast::{expression::CXExpression, function::CXFunctionPrototype, global_var::CXGlobalVariable, modifiers::VisibilityMode, template::CXTemplateInput, types::CXType};

#[derive(Debug, Clone)]
pub struct UntypedSymbol {
    pub visibility: VisibilityMode,
    pub kind: UntypedSymbolKind
}

impl UntypedSymbol {
    pub fn new(visibility: VisibilityMode, kind: UntypedSymbolKind) -> Self {
        Self { visibility, kind }
    }
}

#[derive(Debug, Clone)]
pub enum UntypedSymbolKind {
    Type(CXType),
    Function(CXFunctionPrototype),
    Global(CXGlobalVariable),
    TypeTemplate {
        input: CXTemplateInput,
        definition: CXType,
    },
    FunctionTemplate {
        input: CXTemplateInput,
        definition: CXFunctionPrototype,
        body: Box<CXExpression>,
    },
    // Templated variables should not be supported
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    symbols: HashMap<String, UntypedSymbol>,
}

impl SymbolNamespaceData {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
    
    pub fn insert_symbol(&mut self, name: impl Into<String>, symbol: UntypedSymbol) {
        self.symbols.insert(name.into(), symbol);
    }

    pub fn get_symbol(&self, name: &str) -> Option<&UntypedSymbol> {
        self.symbols.get(name)
    }
}
