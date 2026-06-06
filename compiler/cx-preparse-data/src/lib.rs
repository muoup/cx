use std::collections::HashMap;

use cx_util::{module_path::ModulePath, namespace::NamespacePath};
use speedy::{Readable, Writable};

use crate::symbol_data::PreparseModuleSymbols;

pub mod registry;
pub mod symbol_data;

#[derive(Debug, Clone, Readable, Writable)]
pub struct PreparseContents {
    pub module: String,
    pub imports: Vec<ModulePath>,
    pub module_symbols: PreparseModuleSymbols,
    pub root_symbols: PreparseModuleSymbols,
    pub namespace_aliases: HashMap<NamespacePath, NamespacePath>,
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
        namespace_aliases.insert(NamespacePath::root(), namespace.clone());

        Self {
            module,
            imports: Vec::new(),
            module_symbols: PreparseModuleSymbols::new(namespace),
            root_symbols: PreparseModuleSymbols::new(NamespacePath::root()),
            namespace_aliases,
        }
    }
}
