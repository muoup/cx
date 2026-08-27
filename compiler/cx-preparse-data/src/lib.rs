use std::collections::HashMap;

use cx_util::namespace::{NamespacePath, QualifiedName};
use speedy::{Readable, Writable};

use crate::symbol_data::PreparseModuleSymbols;

pub mod registry;
pub mod symbol_data;

pub type NamespaceAliases = HashMap<NamespacePath, Vec<NamespacePath>>;

#[derive(Debug, Clone, Readable, Writable)]
pub struct Import {
    pub names: Vec<QualifiedName>,
    pub alias: Option<NamespacePath>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct PreparseContents {
    pub module: String,
    pub imports: Vec<Import>,
    pub module_symbols: PreparseModuleSymbols,
    pub namespace_aliases: NamespaceAliases,
}

#[derive(Debug, Clone, Copy, Hash, PartialOrd, Ord, PartialEq, Eq, Readable, Writable)]
pub enum VisibilityMode {
    Private,
    Package,
    Public,
}

impl PreparseContents {
    pub fn new(module: String, namespace: NamespacePath) -> Self {
        let mut namespace_aliases = HashMap::new();
        namespace_aliases.insert(NamespacePath::root(), vec![namespace.clone()]);

        Self {
            module,
            imports: Vec::new(),
            module_symbols: PreparseModuleSymbols::new(namespace),
            namespace_aliases,
        }
    }

    pub fn add_namespace_alias(&mut self, alias: NamespacePath, target: NamespacePath) {
        let targets = self.namespace_aliases.entry(alias).or_default();
        if !targets.contains(&target) {
            targets.push(target);
        }
    }
}
