use std::{
    collections::{HashMap, HashSet},
    sync::RwLock,
};

use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::{
    ast::modifiers::HIRSymbolNameScheme,
    symbols::{HIRSymbol, HIRSymbolKind, SymbolIdentifier, SymbolNamespaceData, SymbolResolution},
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

    pub fn resolve(&self, name: &QualifiedName) -> Option<SymbolResolution> {
        self.resolve_identifier(name, SymbolIdentifier::Standard(name.name.as_string()))
    }

    pub fn resolve_tag(&self, name: &QualifiedName) -> Option<SymbolResolution> {
        self.resolve_identifier(name, SymbolIdentifier::tag(name.name.as_string()))
    }

    fn resolve_identifier(
        &self,
        name: &QualifiedName,
        identifier: SymbolIdentifier,
    ) -> Option<SymbolResolution> {
        let inner = self
            .inner
            .read()
            .expect("GlobalSymbolRegistry read lock poisoned");

        // TODO: Try to avoid cloning here
        inner
            .namespaces
            .get(&name.namespace)?
            .get_symbol(&identifier)
            .cloned()
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
                data.get_symbol(&SymbolIdentifier::standard(name))
                    .into_iter()
                    .flat_map(|resolution| resolution.declarations())
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

    pub fn resolve_aliases(
        &self,
        lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Option<Vec<NamespacePath>> {
        let (_, data) = self.get_bucket(lexical_namespace)?;

        Some(data.resolve_aliases(namespace).cloned().collect::<Vec<_>>())
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
