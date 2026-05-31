use std::collections::{HashMap, HashSet};

use cx_ast::registry::GlobalSymbolRegistry;
use cx_util::{CXResult, namespace::QualifiedName, scoped_map::ScopedMap};

use crate::{
    mir::{
        data::{MIRFunctionPrototype, MIRType, MIRTypeId, MIRTypeKind},
        expression::{MIRExpression, MIRPureExpression}, r#type::MIRBitfieldAccess,
    },
    symbol::{MIRSymbol, resolution::resolve_symbol}, type_context::MIRTypeContext,
};

//
// Module-local symbol definitions
//
pub struct MIRSymbolRegistry<'a> {
    global_registry: &'a GlobalSymbolRegistry,
    global_cache: HashMap<QualifiedName, MIRSymbol>,
    local_symbols: ScopedMap<QualifiedName, MIRSymbol>,

    // These two fields differ in one import way: when a recursive type is being defined,
    // it is in a state of "valid typeid but not validly mapped to a type yet", so it will
    // be visible in valid_typeid but not in typeid_defs
    valid_typeid: HashSet<MIRTypeId>,
    typeid_defs: HashMap<MIRTypeId, MIRType>,
    next_typeid: u64,
}

impl MIRTypeContext for MIRSymbolRegistry<'_> {
    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType {
        self.typeid_defs.get(&id)
            .unwrap_or_else(|| panic!("Invalid MIRTypeId {} in AST!", id))
    }
}

impl<'a> MIRSymbolRegistry<'a> {
    pub fn new(global_registry: &'a GlobalSymbolRegistry) -> Self {
        Self {
            global_registry,
            global_cache: HashMap::new(),
            local_symbols: ScopedMap::new(),

            valid_typeid: HashSet::new(),
            typeid_defs: HashMap::new(),
            next_typeid: 0
        }
    }

    pub fn get_symbol(&mut self, name: &QualifiedName) -> CXResult<Option<MIRSymbol>> {
        if let Some(preresolved_symbol) = self.get_preresolved_symbol(name) {
            return Ok(Some(preresolved_symbol.clone()));
        }

        if let Some(local_symbol) = self.local_symbols.get(name) {
            return Ok(Some(local_symbol.clone()));
        }
        
        let Some(untyped_symbol) = self.global_registry.resolve(name) else {
            return Ok(None);
        };

        let symbol = resolve_symbol(self, &untyped_symbol)?;
        self.insert_symbol(name.clone(), symbol.clone());
        Ok(Some(symbol))
    }

    pub fn get_preresolved_symbol(&self, name: &QualifiedName) -> Option<&MIRSymbol> {
        self.global_cache.get(name)
    }

    pub fn generate_type_id(&mut self, ty: MIRType) -> MIRTypeId {
        self.typeid_defs.insert(MIRTypeId(self.next_typeid), ty);

        self.next_typeid += 1;
        MIRTypeId(self.next_typeid - 1)
    }

    pub fn insert_symbol(&mut self, name: QualifiedName, symbol: MIRSymbol) {
        self.global_cache.insert(name, symbol);
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

    pub fn pointer_to(&mut self, ty: MIRType) -> MIRType {
        let id = self.generate_type_id(ty);

        MIRTypeKind::PointerTo { inner_type: id }.into()
    }

    pub fn mem_ref_to(&mut self, ty: MIRType) -> MIRType {
        let id = self.generate_type_id(ty);

        MIRTypeKind::MemoryReference { inner_type: id, bitfield: None }.into()
    }

    pub fn bitfield_mem_ref_to(&mut self, ty: MIRType, bitfield: MIRBitfieldAccess) -> MIRType {
        let id = self.generate_type_id(ty);
        
        MIRTypeKind::MemoryReference { inner_type: id, bitfield: Some(bitfield) }.into()
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

impl MIRTypeContext for MIRDecomposedRegistry {
    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType {
        self.typeid_map.get(&id)
            .unwrap_or_else(|| panic!("Invalid id {id} in MIRDecomposedRegistry!"))
    }
}

impl MIRDecomposedRegistry {
    pub fn decompose_registry(registry: MIRSymbolRegistry) -> Self {
        Self {
            typeid_map: registry.typeid_defs,
        }
    }
}
