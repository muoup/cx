use std::collections::HashMap;

use cx_mir::mir::data::MIRTypeId;
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct SymbolId(pub u64);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SymbolKind {
    Value,
    Function,
    Type,
    TypeConstructor,
    GenericParam,
}

#[derive(Clone, Debug)]
pub enum SemanticSymbol {
    Type(MIRTypeId),
    GenericTypeParam { resolved_type: MIRTypeId },
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub id: SymbolId,
    pub name: CXIdent,
    pub kind: SymbolKind,
    pub semantic: SemanticSymbol,
}

pub struct SymbolStore {
    next_id: u64,
    symbols: HashMap<SymbolId, Symbol>,
}

impl SymbolStore {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            symbols: HashMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        name: CXIdent,
        kind: SymbolKind,
        semantic: SemanticSymbol,
    ) -> SymbolId {
        let id = SymbolId(self.next_id);
        self.next_id += 1;
        self.symbols.insert(
            id,
            Symbol {
                id,
                name,
                kind,
                semantic,
            },
        );
        id
    }

    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(&id)
    }
}

pub struct TemplateScope {
    bindings: ScopedMap<SymbolId>,
}

impl TemplateScope {
    pub fn new() -> Self {
        Self {
            bindings: ScopedMap::new_with_starting_scope(),
        }
    }

    pub fn push_scope(&mut self) {
        self.bindings.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.bindings.pop_scope();
    }

    pub fn insert(&mut self, name: String, symbol: SymbolId) {
        self.bindings.insert(name, symbol);
    }

    pub fn get(&self, name: &str) -> Option<SymbolId> {
        self.bindings.get(name).copied()
    }
}
