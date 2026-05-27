use std::collections::HashMap;
use std::sync::RwLock;

use cx_ast::symbols::{GlobalSymbolRegistry, SymbolId, UntypedSymbol};
use cx_util::CXResult;

use crate::mir::{
    data::{MIRFunctionPrototype, MIRType},
    expression::MIRPureExpression,
    program::MIRGlobalVariable,
};

#[derive(Clone, Debug)]
pub enum MIRSymbol {
    Type(MIRType),
    Function(MIRFunctionPrototype),
    Global(MIRGlobalVariable),
    PureValue(MIRPureExpression),
}

#[derive(Debug, Default)]
pub struct GlobalMIRSymbolResolver {
    cache: RwLock<HashMap<SymbolId, MIRSymbol>>,
}

impl GlobalMIRSymbolResolver {
    pub fn get_cached(&self, symbol: SymbolId) -> Option<MIRSymbol> {
        self.cache
            .read()
            .expect("GlobalMIRSymbolResolver read lock poisoned")
            .get(&symbol)
            .cloned()
    }

    pub fn insert_cached(&self, symbol: SymbolId, mir_symbol: MIRSymbol) {
        self.cache
            .write()
            .expect("GlobalMIRSymbolResolver write lock poisoned")
            .insert(symbol, mir_symbol);
    }

    pub fn resolve_or_realize<F>(
        &self,
        registry: &GlobalSymbolRegistry,
        symbol: SymbolId,
        realize: F,
    ) -> CXResult<Option<MIRSymbol>>
    where
        F: FnOnce(UntypedSymbol) -> CXResult<MIRSymbol>,
    {
        if let Some(symbol) = self.get_cached(symbol) {
            return Ok(Some(symbol));
        }

        let Some(untyped) = registry.symbol(symbol) else {
            return Ok(None);
        };

        let realized = realize(untyped)?;
        self.insert_cached(symbol, realized.clone());
        Ok(Some(realized))
    }
}
