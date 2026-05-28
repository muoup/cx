use std::collections::HashMap;
use std::sync::RwLock;

use cx_util::{
    identifier::CXIdent,
    module_path::ModulePath,
    namespace::{NamespacePath, QualifiedName},
};
use speedy::{Readable, Writable};

use crate::{
    ast::{CXFunctionStmt, CXGlobalVariable},
    data::{
        CXFunctionKey, CXFunctionPrototype, CXFunctionTemplate, CXType, CXTypeTemplate,
        ModuleResource,
    },
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Readable, Writable)]
pub struct SymbolId(pub u64);

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum SymbolKey {
    Type(String),
    Function(CXFunctionKey),
    Global(String),
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum UntypedSymbol {
    Type(ModuleResource<CXType>),
    TypeTemplate(ModuleResource<CXTypeTemplate>),
    Function(ModuleResource<CXFunctionPrototype>),
    FunctionTemplate(
        ModuleResource<CXFunctionTemplate>,
        Option<Box<CXFunctionStmt>>,
    ),
    Global(ModuleResource<CXGlobalVariable>),
}

impl UntypedSymbol {
    pub fn module_origin(&self) -> Option<&String> {
        match self {
            UntypedSymbol::Type(resource) => resource.external_module.as_ref(),
            UntypedSymbol::TypeTemplate(resource) => resource.external_module.as_ref(),
            UntypedSymbol::Function(resource) => resource.external_module.as_ref(),
            UntypedSymbol::FunctionTemplate(resource, _) => resource.external_module.as_ref(),
            UntypedSymbol::Global(resource) => resource.external_module.as_ref(),
        }
    }
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct SymbolNamespaceData {
    symbols: HashMap<String, SymbolId>,
    children: Vec<CXIdent>,
}

impl SymbolNamespaceData {
    pub fn insert_symbol(&mut self, name: impl Into<String>, id: SymbolId) -> Option<SymbolId> {
        self.symbols.insert(name.into(), id)
    }

    pub fn get_symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }

    pub fn add_child(&mut self, name: CXIdent) {
        if !self.children.contains(&name) {
            self.children.push(name);
        }
    }
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct DecomposedModuleSymbols {
    pub namespace: NamespacePath,
    pub symbols: Vec<(SymbolKey, UntypedSymbol)>,
}

impl DecomposedModuleSymbols {
    pub fn new(namespace: NamespacePath) -> Self {
        Self {
            namespace,
            symbols: Vec::new(),
        }
    }
}

#[derive(Debug, Default)]
pub struct GlobalSymbolRegistry {
    inner: RwLock<GlobalSymbolRegistryData>,
}

#[derive(Debug, Default)]
struct GlobalSymbolRegistryData {
    next_id: u64,
    namespaces: HashMap<NamespacePath, SymbolNamespaceData>,
    symbols: HashMap<SymbolId, UntypedSymbol>,
    keys: HashMap<(NamespacePath, SymbolKey), SymbolId>,
}

impl GlobalSymbolRegistry {
    pub fn insert_module(&self, module: DecomposedModuleSymbols) {
        let mut inner = self
            .inner
            .write()
            .expect("GlobalSymbolRegistry write lock poisoned");
        register_namespace_path(&mut inner.namespaces, &module.namespace);

        for (key, symbol) in module.symbols {
            let id = if let Some(id) = inner.keys.get(&(module.namespace.clone(), key.clone())) {
                *id
            } else {
                inner.next_id += 1;
                let id = SymbolId(inner.next_id);
                inner
                    .keys
                    .insert((module.namespace.clone(), key.clone()), id);
                id
            };

            let name = key_name(&key);
            inner
                .namespaces
                .entry(module.namespace.clone())
                .or_default()
                .insert_symbol(name, id);
            inner.symbols.insert(id, symbol);
        }
    }

    pub fn resolve(&self, name: &QualifiedName) -> Option<(SymbolId, UntypedSymbol)> {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");
        let id = inner
            .namespaces
            .get(&name.namespace)
            .and_then(|namespace| namespace.get_symbol(name.name.as_str()))?;
        let symbol = inner.symbols.get(&id)?.clone();
        Some((id, symbol))
    }

    pub fn symbol(&self, id: SymbolId) -> Option<UntypedSymbol> {
        self.inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned")
            .symbols
            .get(&id)
            .cloned()
    }
}

fn key_name(key: &SymbolKey) -> String {
    match key {
        SymbolKey::Type(name) | SymbolKey::Global(name) => name.clone(),
        SymbolKey::Function(name) => name.as_flat_name(),
    }
}

fn register_namespace_path(
    namespaces: &mut HashMap<NamespacePath, SymbolNamespaceData>,
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

pub fn namespace_from_module(module: impl AsRef<str>) -> NamespacePath {
    NamespacePath::from(ModulePath::new(module.as_ref().replace("::", "/")))
}
