use rustc_hash::FxHashMap;

use cx_hir::ast::types::HIRTagKind;
use cx_namespace::module::{NamespacePath, QualifiedName};

use crate::HMIRDefID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRLookupClass {
    Ordinary,
    Tag,
}

#[derive(Debug, Clone, Default)]
pub struct HMIRSymbolTable {
    imports: Vec<NamespacePath>,
    aliases: FxHashMap<NamespacePath, Vec<NamespacePath>>,
    symbols: FxHashMap<(QualifiedName, HMIRLookupClass), HMIRDefID>,
}

impl From<Option<HIRTagKind>> for HMIRLookupClass {
    fn from(other: Option<HIRTagKind>) -> Self {
        match other {
            Some(_) => Self::Tag,
            None => Self::Ordinary,
        }
    }
}

impl HMIRSymbolTable {
    pub fn new(
        imports: Vec<NamespacePath>,
        aliases: FxHashMap<NamespacePath, Vec<NamespacePath>>,
        symbols: FxHashMap<(QualifiedName, HMIRLookupClass), HMIRDefID>,
    ) -> Self {
        Self {
            imports,
            aliases,
            symbols,
        }
    }

    pub fn imports(&self) -> &[NamespacePath] {
        &self.imports
    }

    pub fn aliases(&self) -> &FxHashMap<NamespacePath, Vec<NamespacePath>> {
        &self.aliases
    }

    pub fn symbol(&self, name: &QualifiedName, class: HMIRLookupClass) -> Option<HMIRDefID> {
        self.symbols.get(&(name.clone(), class)).copied()
    }

    pub fn symbols(&self) -> impl Iterator<Item = (&QualifiedName, HMIRLookupClass, HMIRDefID)> {
        self.symbols
            .iter()
            .map(|((name, class), def)| (name, *class, *def))
    }
}
