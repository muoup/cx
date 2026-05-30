use std::collections::{HashMap, HashSet};

use cx_ast::registry::GlobalSymbolRegistry;
use cx_util::{CXResult, namespace::QualifiedName};

use crate::{
    mir::{
        data::{MIRFunctionPrototype, MIRType, MIRTypeId},
        expression::{MIRExpression, MIRPureExpression},
    },
    symbol::{MIRSymbol, resolution::resolve_symbol},
};

//
// Module-local symbol definitions
//
pub struct MIRSymbolRegistry<'a> {
    global_registry: &'a GlobalSymbolRegistry,
    cache: HashMap<QualifiedName, MIRSymbol>,

    // These two fields differ in one import way: when a recursive type is being defined,
    // it is in a state of "valid typeid but not validly mapped to a type yet", so it will
    // be visible in valid_typeid but not in typeid_defs
    valid_typeid: HashSet<MIRTypeId>,
    typeid_defs: HashMap<MIRTypeId, MIRType>,
}

impl<'a> MIRSymbolRegistry<'a> {
    pub fn new(global_registry: &'a GlobalSymbolRegistry) -> Self {
        Self {
            global_registry,

            cache: HashMap::new(),
            valid_typeid: HashSet::new(),
            typeid_defs: HashMap::new(),
        }
    }

    pub fn get_symbol(&mut self, name: &QualifiedName) -> CXResult<Option<MIRSymbol>> {
        if let Some(symbol) = self.cache.get(name) {
            return Ok(Some(symbol.clone()));
        }

        let Some(untyped_symbol) = self.global_registry.resolve(name) else {
            return Ok(None);
        };

        resolve_symbol(self, &untyped_symbol).map(Option::Some)
    }

    pub fn resolve_type_id(&self, id: &MIRTypeId) -> Option<&MIRType> {
        self.typeid_defs.get(id)
    }

    pub fn insert_value(&mut self, name: QualifiedName, expr: MIRExpression) {
        self.cache.insert(name, MIRSymbol::Value(expr));
    }

    pub fn insert_pure_value(&mut self, name: QualifiedName, expr: MIRPureExpression) {
        self.cache.insert(name, MIRSymbol::PureValue(expr));
    }

    pub fn insert_function_symbol(&mut self, name: QualifiedName, prototype: MIRFunctionPrototype) {
        self.cache.insert(
            name,
            MIRSymbol::PureValue(MIRPureExpression::FunctionReference(Box::new(prototype))),
        );
    }
}
