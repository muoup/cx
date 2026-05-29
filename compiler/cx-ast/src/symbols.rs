use std::collections::HashMap;

use crate::ast::{function::CXFunctionPrototype, global_var::CXGlobalVariable, modifiers::VisibilityMode, template::CXTemplateInput, types::CXType};

#[derive(Debug, Clone)]
pub struct UntypedSymbol {
    visibility: VisibilityMode,
    kind: UntypedSymbolKind
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
    },
    // Templated variables should not be supported
}

#[derive(Debug, Default, Clone)]
pub struct SymbolNamespaceData {
    symbols: HashMap<String, UntypedSymbol>,
}

impl SymbolNamespaceData {
    pub fn insert_symbol(&mut self, name: impl Into<String>, symbol: UntypedSymbol) {
        self.symbols.insert(name.into(), symbol);
    }

    pub fn get_symbol(&self, name: &str) -> Option<&UntypedSymbol> {
        self.symbols.get(name)
    }
}
