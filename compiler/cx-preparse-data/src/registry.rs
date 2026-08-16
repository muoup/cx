use crate::symbol_data::{PreparseModuleSymbols, PreparseNamespaceData, PreparseSymbolKind};
use cx_util::{identifier::CXIdent, namespace::NamespacePath};
use std::collections::HashMap;
use std::sync::RwLock;

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
