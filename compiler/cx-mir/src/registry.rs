use std::collections::{HashMap, HashSet};

use cx_ast::registry::GlobalSymbolRegistry;
use cx_util::{namespace::QualifiedName, CXResult};

use crate::{
    mir::{
        data::{MIRFunctionPrototype, MIRType, MIRTypeId},
        expression::{MIRExpression, MIRPureExpression},
    },
    symbol::{resolution::resolve_symbol, MIRSymbol},
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
        if let Some(preresolved_symbol) = self.get_preresolved_symbol(name) {
            return Ok(Some(preresolved_symbol.clone()));
        }

        let Some(untyped_symbol) = self.global_registry.resolve(name) else {
            return Ok(None);
        };

        let symbol = resolve_symbol(self, &untyped_symbol)?;
        self.insert_symbol(name.clone(), symbol.clone());
        Ok(Some(symbol))
    }

    pub fn get_preresolved_symbol(&self, name: &QualifiedName) -> Option<&MIRSymbol> {
        self.cache.get(name)
    }

    pub fn resolve_type_id(&self, id: &MIRTypeId) -> Option<&MIRType> {
        self.typeid_defs.get(id)
    }

    pub fn insert_symbol(&mut self, name: QualifiedName, symbol: MIRSymbol) {
        self.cache.insert(name, symbol);
    }

    pub fn insert_type_symbol(&mut self, name: QualifiedName, id: MIRTypeId) {
        self.insert_symbol(name, MIRSymbol::Type(id));
    }

    pub fn insert_value(&mut self, name: QualifiedName, expr: MIRExpression) {
        self.insert_symbol(name, MIRSymbol::Value(expr));
    }

    pub fn insert_pure_value(&mut self, name: QualifiedName, expr: MIRPureExpression) {
        self.insert_symbol(name, MIRSymbol::PureValue(expr));
    }

    pub fn insert_function_symbol(&mut self, name: QualifiedName, prototype: MIRFunctionPrototype) {
        self.insert_symbol(
            name,
            MIRSymbol::PureValue(MIRPureExpression::FunctionReference(Box::new(prototype))),
        );
    }
}

//
// After the evaluation and completion of the MIRUnit, this struct contains all necessary context to interpret
// the complete meaning of its contents. For instance, prototypes are not necessary to provide here as a map as
// they are either tacked onto the function definition nodes or in the types applied to the AST nodes, however
// mapping type ids is required as later steps need to be able to interpret type definitions.
//
#[derive(Debug, Clone)]
pub struct MIRDecomposedRegistry {
    pub typeid_map: HashMap<MIRTypeId, MIRType>,
}

impl MIRDecomposedRegistry {
    pub fn decompose_registry(registry: MIRSymbolRegistry) -> Self {
        Self {
            typeid_map: registry.typeid_defs,
        }
    }

    pub fn resolve_type_id(&self, id: &MIRTypeId) -> Option<&MIRType> {
        self.typeid_map.get(id)
    }
}
