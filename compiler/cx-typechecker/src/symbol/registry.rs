use std::collections::HashMap;

use cx_hir::{ast::expression::HIRExpression, registry::GlobalSymbolRegistry};
use cx_log::{CXRawResult, CXResult};
use cx_target::ArchitectureConfig;
use cx_thir::{
    EnvironmentNamespace,
    intrinsic_types::INTRINSIC_TYPES,
    registry::THIRDecomposedRegistry,
    symbol::MIRSymbol,
    thir::{
        data::THIRFnPrototype,
        expression::{THIRExpression, THIRPureExpression},
        r#type::{THIRType, THIRTypeID, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};
use cx_util::{identifier::CXIdent, namespace::QualifiedName, scoped_map::ScopedMap};

/// Module-local symbol definitions
pub struct MIRSymbolRegistry<'a> {
    architecture: ArchitectureConfig,
    global_registry: &'a GlobalSymbolRegistry,
    global_cache: HashMap<QualifiedName, MIRSymbol>,
    tag_cache: HashMap<QualifiedName, MIRSymbol>,
    local_symbols: ScopedMap<QualifiedName, MIRSymbol>,

    typeid_defs: HashMap<THIRTypeID, THIRType>,
    next_typeid: usize,
}

impl THIRTypeContext for MIRSymbolRegistry<'_> {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn resolve_type_id(&self, id: THIRTypeID) -> &THIRType {
        self.typeid_defs
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid MIRTypeId {} in AST!", id))
    }

    fn try_resolve_type_id(&self, id: THIRTypeID) -> Option<&THIRType> {
        self.typeid_defs.get(&id)
    }

    fn type_id_lookup_identifier(&self, id: THIRTypeID) -> Option<&QualifiedName> {
        self.global_cache
            .iter()
            .find_map(|(name, symbol)| (symbol.as_type_id() == Some(id)).then_some(name))
            .or_else(|| {
                self.try_resolve_type_id(id)
                    .and_then(|ty| ty.lookup_identifier())
            })
    }
}

impl<'a> MIRSymbolRegistry<'a> {
    pub fn new(
        global_registry: &'a GlobalSymbolRegistry,
        architecture: ArchitectureConfig,
    ) -> Self {
        let mut registry = Self {
            architecture,
            global_registry,
            global_cache: HashMap::new(),
            tag_cache: HashMap::new(),
            local_symbols: ScopedMap::new_with_starting_scope(),

            typeid_defs: HashMap::new(),
            next_typeid: 0,
        };

        for (name, ty_kind) in INTRINSIC_TYPES {
            let ty_kind = match *name {
                "usize" => THIRTypeKind::Integer {
                    signed: false,
                    _type: registry.pointer_integer_type(),
                },
                "isize" => THIRTypeKind::Integer {
                    signed: true,
                    _type: registry.pointer_integer_type(),
                },
                _ => ty_kind.clone(),
            };
            let name = QualifiedName::new_raw(CXIdent::new(*name));
            let mut ty: THIRType = ty_kind.into();
            ty.lookup_identifier = Some(name.clone());
            let id = registry.generate_type_id(ty);

            registry.insert_type_symbol(name, id);
        }

        registry
    }

    pub fn decompose(self) -> THIRDecomposedRegistry {
        let intrinsic_types = INTRINSIC_TYPES
            .iter()
            .filter_map(|(name, _)| {
                self.global_cache
                    .get(&QualifiedName::new_raw(CXIdent::new(*name)))
                    .and_then(MIRSymbol::as_type_id)
                    .map(|id| ((*name).to_owned(), id))
            })
            .collect();
        THIRDecomposedRegistry::new(
            self.architecture,
            self.typeid_defs,
            intrinsic_types,
            self.next_typeid,
        )
    }

    pub fn get_global_registry(&self) -> &GlobalSymbolRegistry {
        self.global_registry
    }

    pub fn get_preresolved_symbol(&self, name: &QualifiedName) -> Option<&MIRSymbol> {
        self.global_cache.get(name)
    }

    pub fn get_preresolved_tag(&self, name: &QualifiedName) -> Option<&MIRSymbol> {
        self.tag_cache.get(name)
    }

    pub fn generate_type_id(&mut self, ty: THIRType) -> THIRTypeID {
        let id = self.reserve_type_id();
        self.overwrite_type_id(id, ty);

        id
    }

    pub fn undo_type_id(&mut self, id: THIRTypeID) -> Option<THIRType> {
        self.typeid_defs.remove(&id)
    }

    pub fn reserve_type_id(&mut self) -> THIRTypeID {
        let id = THIRTypeID::new(self.next_typeid);
        self.next_typeid += 1;
        id
    }

    pub fn overwrite_type_id(&mut self, id: THIRTypeID, ty: THIRType) {
        self.typeid_defs.insert(id, ty);
    }

    pub fn try_resolve_type_id(&self, id: THIRTypeID) -> Option<&THIRType> {
        self.typeid_defs.get(&id)
    }

    pub fn insert_local_type(&mut self, name: String, _type: THIRType) -> CXResult<THIRTypeID> {
        let type_id = self.generate_type_id(_type);

        self.local_symbols.insert(
            QualifiedName::new_raw(CXIdent::new(name)),
            MIRSymbol::Type(type_id),
        );

        Ok(type_id)
    }

    pub fn insert_local_type_id(&mut self, name: String, type_id: THIRTypeID) -> CXRawResult<()> {
        self.local_symbols.insert(
            QualifiedName::new_raw(CXIdent::new(name)),
            MIRSymbol::Type(type_id),
        );

        Ok(())
    }

    pub fn push_local_scope(&mut self) {
        self.local_symbols.push_scope();
    }

    pub fn pop_local_scope(&mut self) {
        self.local_symbols.pop_scope();
    }

    pub fn get_local_symbol(&self, name: &QualifiedName) -> Option<&MIRSymbol> {
        self.local_symbols.get(name)
    }

    pub fn get_local_symbol_at_shadow_depth(
        &self,
        name: &QualifiedName,
        depth: usize,
    ) -> Option<&MIRSymbol> {
        self.local_symbols.get_at_shadow_depth(name, depth)
    }

    pub fn get_local_symbol_avoiding_staged_expansions(
        &self,
        name: &QualifiedName,
        active_expansions: &[u64],
    ) -> Option<&MIRSymbol> {
        let mut depth = 0;
        loop {
            let symbol = self.get_local_symbol_at_shadow_depth(name, depth)?;
            match symbol {
                MIRSymbol::StagedExpression { id, .. } if active_expansions.contains(id) => {
                    depth += 1;
                }
                symbol => return Some(symbol),
            }
        }
    }

    pub fn insert_symbol(&mut self, name: QualifiedName, symbol: MIRSymbol) {
        self.global_cache.insert(name, symbol);
    }

    pub fn insert_value(&mut self, name: QualifiedName, expr: THIRExpression) {
        self.insert_symbol(name, MIRSymbol::Expression(expr));
    }

    pub fn insert_type_symbol(&mut self, name: QualifiedName, id: THIRTypeID) {
        self.insert_symbol(name, MIRSymbol::Type(id));
    }

    pub fn insert_tag_type_symbol(&mut self, name: QualifiedName, id: THIRTypeID) {
        self.tag_cache.insert(name, MIRSymbol::Type(id));
    }

    pub fn insert_local_value(&mut self, name: QualifiedName, expr: THIRExpression) {
        self.local_symbols.insert(name, MIRSymbol::Expression(expr));
    }

    pub fn insert_local_staged_expression(
        &mut self,
        id: u64,
        name: QualifiedName,
        namespace: EnvironmentNamespace,
        expr: HIRExpression,
        expected_type: THIRType,
    ) {
        self.local_symbols.insert(
            name,
            MIRSymbol::StagedExpression {
                id,
                namespace,
                expr: Box::new(expr),
                expected_type,
            },
        );
    }

    pub fn insert_local_staged_expression_function(
        &mut self,
        name: QualifiedName,
        local_id: cx_thir::thir::expression::THIRLocalID,
        params: Vec<THIRType>,
        return_type: THIRType,
    ) {
        self.local_symbols.insert(
            name,
            MIRSymbol::StagedExpressionFunction {
                local_id,
                params,
                return_type,
            },
        );
    }

    pub fn insert_pure_value(&mut self, name: QualifiedName, expr: THIRPureExpression) {
        self.insert_symbol(name, MIRSymbol::Expression(expr.as_value()));
    }

    pub fn insert_function_symbol(&mut self, name: QualifiedName, prototype: THIRFnPrototype) {
        self.insert_symbol(name, MIRSymbol::FunctionReference(prototype));
    }

    pub fn pointer_to(&mut self, ty: THIRType) -> THIRType {
        let inner_type = self.generate_type_id(ty);
        THIRTypeKind::PointerTo { inner_type }.into()
    }

    pub fn mem_ref_to(&mut self, ty: THIRType) -> THIRType {
        let inner_type = self.generate_type_id(ty);
        THIRTypeKind::MemoryReference {
            inner_type,
            bitfield: None,
        }
        .into()
    }

    pub fn contains(&self, id: THIRTypeID) -> bool {
        self.typeid_defs.contains_key(&id)
    }
}
