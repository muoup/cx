use std::collections::{HashMap, HashSet};
use std::sync::RwLock;

use cx_util::{identifier::CXIdent, namespace::NamespacePath};
use speedy::{Readable, Writable};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Readable, Writable)]
pub enum PreparseSymbolKind {
    Type,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct PreparseSymbol {
    pub name: CXIdent,
    pub kind: PreparseSymbolKind,
}

impl PreparseSymbol {
    pub fn type_name(name: CXIdent) -> Self {
        Self {
            name,
            kind: PreparseSymbolKind::Type,
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

    pub fn add_type(&mut self, name: CXIdent) {
        self.symbols.push(PreparseSymbol::type_name(name));
    }
}

#[derive(Debug, Default)]
pub struct GlobalPreparseRegistry {
    namespaces: RwLock<HashMap<NamespacePath, PreparseNamespaceData>>,
}

impl GlobalPreparseRegistry {
    pub fn insert_module(&self, module: PreparseModuleSymbols) {
        let mut namespaces = self
            .namespaces
            .write()
            .expect("GlobalPreparseRegistry write lock poisoned");
        register_namespace_path(&mut namespaces, &module.namespace);
        let data = namespaces.entry(module.namespace).or_default();
        for symbol in module.symbols {
            data.insert_symbol(symbol);
        }
    }

    pub fn get_symbol(
        &self,
        namespace: &NamespacePath,
        name: &CXIdent,
    ) -> Option<PreparseSymbolKind> {
        self.namespaces
            .read()
            .expect("GlobalPreparseRegistry read lock poisoned")
            .get(namespace)
            .and_then(|data| data.get_symbol(name))
    }

    pub fn namespace_exists(&self, namespace: &NamespacePath) -> bool {
        self.namespaces
            .read()
            .expect("GlobalPreparseRegistry read lock poisoned")
            .contains_key(namespace)
    }

    pub fn has_child(&self, namespace: &NamespacePath, name: &CXIdent) -> bool {
        self.namespaces
            .read()
            .expect("GlobalPreparseRegistry read lock poisoned")
            .get(namespace)
            .is_some_and(|data| data.has_child(name))
    }
}

fn register_namespace_path(
    namespaces: &mut HashMap<NamespacePath, PreparseNamespaceData>,
    path: &NamespacePath,
) {
    let mut parent = NamespacePath::root();
    namespaces.entry(parent.clone()).or_default();

    for segment in path.segments() {
        namespaces
            .entry(parent.clone())
            .or_default()
            .add_child(segment.clone());
        parent = parent.child(segment.clone());
        namespaces.entry(parent.clone()).or_default();
    }
}
