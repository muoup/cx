use std::{collections::HashMap, sync::RwLock};

use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::symbols::{SymbolNamespaceData, CXSymbol};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum ExportNameMode {
    #[default]
    Namespaced,
    Root,
}

#[derive(Debug, Default)]
pub struct GlobalSymbolRegistry {
    inner: RwLock<GlobalSymbolRegistryData>,
}

#[derive(Debug, Default)]
struct GlobalSymbolRegistryData {
    namespaces: HashMap<NamespacePath, SymbolNamespaceData>,
    export_name_modes: HashMap<NamespacePath, ExportNameMode>,
}

impl GlobalSymbolRegistry {
    /// Returns back provided arguments if failed to insert (i.e. namespace already exists)
    pub fn insert_module(
        &self,
        namespace: NamespacePath,
        data: SymbolNamespaceData,
    ) -> Option<(NamespacePath, SymbolNamespaceData)> {
        let mut inner = self
            .inner
            .write()
            .expect("GlobalSymbolRegistry write lock poisoned");

        if inner.namespaces.contains_key(&namespace) {
            return Some((namespace, data));
        }

        inner.namespaces.insert(namespace, data);

        None
    }

    pub fn set_export_name_mode(&self, namespace: NamespacePath, mode: ExportNameMode) {
        self.inner
            .write()
            .expect("GlobalSymbolRegistry write lock poisoned")
            .export_name_modes
            .insert(namespace, mode);
    }

    pub fn export_name_mode(&self, namespace: &NamespacePath) -> ExportNameMode {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");

        for segment_count in (0..=namespace.segments().len()).rev() {
            let prefix = NamespacePath::new(namespace.segments()[..segment_count].to_vec());
            if let Some(mode) = inner.export_name_modes.get(&prefix) {
                return *mode;
            }
        }

        ExportNameMode::Namespaced
    }

    pub fn resolve(&self, name: &QualifiedName) -> Option<CXSymbol> {
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
