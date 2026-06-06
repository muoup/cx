use std::{collections::HashMap, sync::RwLock};

use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::symbols::{CXSymbol, SymbolNamespaceData};

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

        if let Some(existing) = inner.namespaces.get_mut(&namespace) {
            if namespace.is_root() {
                existing.merge_from(data);
                return None;
            }

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

    pub fn resolve_qualified_alias(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> QualifiedName {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");

        inner
            .namespaces
            .get(lexical_namespace)
            .and_then(|data| data.resolve_qualified_alias(name))
            .unwrap_or_else(|| name.clone())
    }

    pub fn get_bucket<'b, 'c>(
        &'b self,
        namespace: &NamespacePath,
    ) -> Option<(impl Sized + use<'b>, &'c SymbolNamespaceData)> {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");

        let data = inner.namespaces.get(namespace)?;

        // This is incredibly unnecessary but I thought it was funny. This is my 11:40 PM attempt at
        // recreating RwLockReadGuard::map that is not current stable.
        //
        // Also not sure how to do a true opaque type for locks which we don't want the user to touch,
        // but impl Sized kinda rocks
        unsafe {
            let data = std::mem::transmute(data);

            Some((inner, data))
        }
    }
}
