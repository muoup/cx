use std::{borrow::Cow, collections::HashMap};

use cx_ast::{registry::GlobalSymbolRegistry, symbols::SymbolNamespaceData};
use cx_mir::{
    intrinsic_types::INTRINSIC_TYPES,
    mir::{
        data::MIRFunctionPrototype,
        expression::{MIRExpression, MIRExpressionKind, MIRPureExpression},
        r#type::{MIRType, MIRTypeId, MIRTypeKind},
    },
    registry::MIRDecomposedRegistry,
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};
use cx_util::{
    CXResult,
    identifier::CXIdent,
    namespace::{NamespacePath, QualifiedName},
    scoped_map::ScopedMap,
};


/// Module-local symbol definitions
pub struct MIRSymbolRegistry<'a> {
    global_registry: &'a GlobalSymbolRegistry,
    global_cache: HashMap<QualifiedName, MIRSymbol>,
    local_symbols: ScopedMap<QualifiedName, MIRSymbol>,
    namespace_aliases: HashMap<NamespacePath, NamespacePath>,

    typeid_defs: HashMap<MIRTypeId, MIRType>,
    next_typeid: u64,
}

impl MIRTypeContext for MIRSymbolRegistry<'_> {
    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType {
        self.typeid_defs
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid MIRTypeId {} in AST!", id))
    }
}

impl<'a> MIRSymbolRegistry<'a> {
    pub fn new(
        global_registry: &'a GlobalSymbolRegistry,
        namespace_aliases: HashMap<NamespacePath, NamespacePath>,
    ) -> Self {
        let mut registry = Self {
            global_registry,
            global_cache: HashMap::new(),
            local_symbols: ScopedMap::new_with_starting_scope(),

            namespace_aliases,

            typeid_defs: HashMap::new(),
            next_typeid: 0,
        };

        for (name, ty_kind) in INTRINSIC_TYPES {
            let ty: MIRType = ty_kind.clone().into();
            let id = registry.generate_type_id(ty);

            registry.insert_type_symbol(QualifiedName::new_raw(CXIdent::new(*name)), id);
        }

        registry
    }

    pub fn decompose(self) -> MIRDecomposedRegistry {
        MIRDecomposedRegistry {
            typeid_map: self.typeid_defs,
        }
    }

    pub fn get_global_registry(&self) -> &GlobalSymbolRegistry {
        self.global_registry
    }

    pub fn resolve_namespace_alias<'b>(&'b self, namespace: &'b NamespacePath) -> Cow<'b, NamespacePath> {
        if let Some(alias) = self.namespace_aliases.get(namespace) {
            Cow::Borrowed(alias)
        } else {
            Cow::Borrowed(namespace)
        }
    }

    pub fn resolve_qualified_alias<'b>(&self, name: &'b QualifiedName) -> Cow<'b, QualifiedName> {
        let mut name = Cow::Borrowed(name);

        if let Some(alias) = self.namespace_aliases.get(&name.namespace) {
            name = Cow::Owned(QualifiedName {
                namespace: alias.clone(),
                name: name.name.clone(),
            });
        }

        name
    }

    pub fn get_namespace_data(
        &mut self,
        name: &NamespacePath,
    ) -> Option<(impl Sized, &SymbolNamespaceData)> {
        let resolved_namespace = self.resolve_namespace_alias(name);
        
        self.global_registry.get_bucket(&resolved_namespace)
    }

    pub fn get_preresolved_symbol(&self, name: &QualifiedName) -> Option<&MIRSymbol> {
        self.global_cache.get(name)
    }

    pub fn map_namespace_alias(&mut self, alias: NamespacePath, target: NamespacePath) {
        self.namespace_aliases.insert(alias, target);
    }

    pub fn generate_type_id(&mut self, ty: MIRType) -> MIRTypeId {
        let id = self.reserve_type_id();
        self.overwrite_type_id(id, ty);

        id
    }

    pub fn undo_type_id(&mut self, id: MIRTypeId) -> Option<MIRType> {
        self.typeid_defs.remove(&id)
    }

    pub fn reserve_type_id(&mut self) -> MIRTypeId {
        let id = MIRTypeId(self.next_typeid);
        self.next_typeid += 1;
        id
    }

    pub fn overwrite_type_id(&mut self, id: MIRTypeId, ty: MIRType) {
        self.typeid_defs.insert(id, ty);
    }

    pub fn insert_local_type(&mut self, name: String, _type: MIRType) -> CXResult<MIRTypeId> {
        let type_id = self.generate_type_id(_type);

        self.local_symbols.insert(
            QualifiedName::new_raw(CXIdent::new(name)),
            MIRSymbol::Type(type_id),
        );

        Ok(type_id)
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

    pub fn insert_symbol(&mut self, name: QualifiedName, symbol: MIRSymbol) {
        self.global_cache.insert(name, symbol);
    }

    pub fn insert_type_symbol(&mut self, name: QualifiedName, id: MIRTypeId) {
        self.insert_symbol(name, MIRSymbol::Type(id));
    }

    pub fn insert_value(&mut self, name: QualifiedName, expr: MIRExpression) {
        self.insert_symbol(name, MIRSymbol::Expression(expr));
    }

    pub fn insert_pure_value(&mut self, name: QualifiedName, expr: MIRPureExpression) {
        self.insert_symbol(name, MIRSymbol::Expression(expr.as_value()));
    }

    pub fn insert_function_symbol(&mut self, name: QualifiedName, prototype: MIRFunctionPrototype) {
        self.insert_symbol(
            name,
            MIRSymbol::Expression(MIRExpression {
                token_range: None,
                _type: MIRTypeKind::Function {
                    signature: Box::new(prototype.signature().clone()),
                }
                .into(),
                kind: MIRExpressionKind::FunctionReference {
                    name: prototype.name.clone(),
                },
            }),
        );
    }

    pub fn pointer_to(&mut self, ty: MIRType) -> MIRType {
        let id = self.generate_type_id(ty);

        MIRTypeKind::PointerTo { inner_type: id }.into()
    }

    pub fn mem_ref_to(&mut self, ty: MIRType) -> MIRType {
        let id = self.generate_type_id(ty);

        MIRTypeKind::MemoryReference {
            inner_type: id,
            bitfield: None,
        }
        .into()
    }

    pub fn contains(&self, id: MIRTypeId) -> bool {
        self.typeid_defs.contains_key(&id)
    }
}
