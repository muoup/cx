use std::{
    collections::{HashMap, HashSet},
    sync::RwLock,
};

use cx_namespace::module::{NamespacePath, QualifiedName};

use crate::{
    ast::{global_var::HIREnumDefinition, modifiers::HIRSymbolNameScheme},
    symbols::{HIRSymbol, HIRSymbolKind, SymbolNamespaceData},
};

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
    namespace_friends: HashSet<(NamespacePath, NamespacePath)>,
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

    pub fn insert_namespace_friend(&self, namespace: NamespacePath, friend: NamespacePath) {
        self.inner
            .write()
            .expect("GlobalSymbolRegistry write lock poisoned")
            .namespace_friends
            .insert((namespace, friend));
    }

    pub fn namespaces_are_friends(
        &self,
        namespace: &NamespacePath,
        friend: &NamespacePath,
    ) -> bool {
        self.inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned")
            .namespace_friends
            .contains(&(namespace.clone(), friend.clone()))
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

    pub fn resolve(&self, name: &QualifiedName, tagged: bool) -> Option<Vec<HIRSymbol>> {
        let inner = self.inner.read().expect("GlobalSymbolRegistry read lock poisoned");
        Some(inner.namespaces.get(&name.namespace)?.get_symbol(name.name.as_str(), tagged)?.to_vec())
    }

    pub fn resolve_unmangled_global(&self, name: &str) -> Vec<(NamespacePath, HIRSymbol)> {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");

        inner
            .namespaces
            .iter()
            .flat_map(|(namespace, data)| {
                data.get_symbol(name, false)
                    .into_iter()
                    .flatten()
                    .filter_map(|symbol| {
                        matches!(
                            &symbol.kind,
                            HIRSymbolKind::AddressableGlobal {
                                symbol_naming: HIRSymbolNameScheme::Unmangled,
                                ..
                            }
                        )
                        .then(|| (namespace.clone(), symbol.clone()))
                    })
            })
            .collect()
    }

    pub fn resolve_aliases(&self, lexical_namespace: &NamespacePath, namespace: &NamespacePath) -> Vec<NamespacePath> {
        let inner = self.inner.read().expect("GlobalSymbolRegistry read lock poisoned");
        inner.namespaces.get(lexical_namespace)
            .map(|data| data.resolve_aliases(namespace).cloned().collect())
            .unwrap_or_default()
    }

    pub fn enum_block(&self, namespace: &NamespacePath, index: usize) -> Option<HIREnumDefinition> {
        let inner = self.inner.read().expect("GlobalSymbolRegistry read lock poisoned");
        inner.namespaces.get(namespace)?.get_enum_block(index).cloned()
    }
}
