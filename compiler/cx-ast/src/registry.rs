use std::{collections::HashMap, sync::RwLock};

use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::symbols::{SymbolNamespaceData, UntypedSymbol};

#[derive(Debug, Default)]
pub struct GlobalSymbolRegistry {
    inner: RwLock<GlobalSymbolRegistryData>,
}

#[derive(Debug, Default)]
struct GlobalSymbolRegistryData {
    namespaces: HashMap<NamespacePath, SymbolNamespaceData>,
}

impl GlobalSymbolRegistry {
    pub fn insert_module(&self, namespace: NamespacePath, data: SymbolNamespaceData) {
        let mut inner = self
            .inner
            .write()
            .expect("GlobalSymbolRegistry write lock poisoned");
        inner.namespaces.insert(namespace, data);
    }

    pub fn resolve(&self, name: &QualifiedName) -> Option<UntypedSymbol> {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");

        // TODO: Try to avoid cloning here
        inner
            .namespaces
            .get(&name.namespace)?
            .get_symbol(name.name.as_str())
            .cloned()
    }
}
