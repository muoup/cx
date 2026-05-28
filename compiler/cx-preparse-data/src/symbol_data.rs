use std::collections::{HashMap, HashSet};

use cx_util::{identifier::CXIdent, namespace::NamespacePath};
use speedy::{Readable, Writable};

use crate::VisibilityMode;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Readable, Writable)]
pub enum PreparseSymbolKind {
    Type,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct PreparseSymbol {
    pub name: CXIdent,
    pub kind: PreparseSymbolKind,
    pub visibility: VisibilityMode,
}

impl PreparseSymbol {
    pub fn type_name(name: CXIdent, visibility: VisibilityMode) -> Self {
        Self {
            name,
            kind: PreparseSymbolKind::Type,
            visibility,
        }
    }
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct PreparseNamespaceData {
    symbols: HashMap<CXIdent, PreparseSymbolKind>,
    children: HashSet<CXIdent>,
}

impl PreparseNamespaceData {
    pub fn insert_symbol(&mut self, symbol: PreparseSymbol) {
        self.symbols.insert(symbol.name, symbol.kind);
    }

    pub fn get_symbol(&self, name: &CXIdent) -> Option<PreparseSymbolKind> {
        self.symbols.get(name).copied()
    }

    pub fn add_child(&mut self, name: CXIdent) {
        self.children.insert(name);
    }

    pub fn has_child(&self, name: &CXIdent) -> bool {
        self.children.contains(name)
    }
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct PreparseModuleSymbols {
    pub namespace: NamespacePath,
    pub symbols: Vec<PreparseSymbol>,
}

impl PreparseModuleSymbols {
    pub fn new(namespace: NamespacePath) -> Self {
        Self {
            namespace,
            symbols: Vec::new(),
        }
    }

    pub fn add_type(&mut self, name: CXIdent, visibility: VisibilityMode) {
        self.symbols
            .push(PreparseSymbol::type_name(name, visibility));
    }
}
