use std::collections::{HashMap, HashSet};

use cx_ast::{
    ast::modifiers::{CX_CONST, CXTypeQualifiers},
    registry::GlobalSymbolRegistry,
};
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName, scoped_map::ScopedMap};

use crate::{
    intrinsic_types::INTRINSIC_TYPES,
    mir::{
        data::{MIRFunctionPrototype, MIRType, MIRTypeId, MIRTypeKind, TemplateInfo},
        expression::{MIRExpression, MIRExpressionKind, MIRPureExpression, SymbolValueOrigin},
        name_mangling::type_mangle,
        r#type::MIRBitfieldAccess,
    },
    symbol::{MIRSymbol, resolution::resolve_symbol},
    type_context::MIRTypeContext,
};

//
// Module-local symbol definitions
//
pub struct MIRSymbolRegistry<'a> {
    pub global_registry: &'a GlobalSymbolRegistry,
    global_cache: HashMap<QualifiedName, MIRSymbol>,
    local_symbols: ScopedMap<QualifiedName, MIRSymbol>,
    
    pub realized_types: HashMap<String, MIRType>,
    pub named_type_ids: HashMap<String, MIRTypeId>,
    currently_defining_types: HashSet<MIRTypeId>,
    definition_stack: Vec<MIRTypeId>,

    // These two fields differ in one import way: when a recursive type is being defined,
    // it is in a state of "valid typeid but not validly mapped to a type yet", so it will
    // be visible in valid_typeid but not in typeid_defs
    valid_typeid: HashSet<MIRTypeId>,
    typeid_defs: HashMap<MIRTypeId, MIRType>,
    
    next_typeid: u64,
}

pub struct TemplateBindingFrame {
    bindings: Vec<(String, Option<MIRType>)>,
}

#[derive(Clone)]
pub enum ResolvedValueSymbol {
    Value {
        value: MIRExpression,
        origin: Option<SymbolValueOrigin>,
    },
    PureValue(MIRPureExpression),
}

impl MIRTypeContext for MIRSymbolRegistry<'_> {
    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType {
        self.typeid_defs
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid MIRTypeId {} in AST!", id))
    }
}

impl<'a> MIRSymbolRegistry<'a> {
    pub fn new(global_registry: &'a GlobalSymbolRegistry) -> Self {
        let mut registry = Self {
            global_registry,
            global_cache: HashMap::new(),
            local_symbols: ScopedMap::new_with_starting_scope(),
            realized_types: HashMap::new(),
            named_type_ids: HashMap::new(),
            currently_defining_types: HashSet::new(),
            definition_stack: Vec::new(),

            valid_typeid: HashSet::new(),
            typeid_defs: HashMap::new(),
            next_typeid: 0,
        };

        for (name, ty_kind) in INTRINSIC_TYPES {
            let ty: MIRType = ty_kind.clone().into();
            let id = registry.generate_type_id(ty.clone());
            registry.valid_typeid.insert(id);
            registry.named_type_ids.insert((*name).to_string(), id);
            registry
                .realized_types
                .insert((*name).to_string(), ty.clone());
            registry.insert_type_symbol(QualifiedName::new_raw(CXIdent::new(*name)), id);
        }

        registry
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

    pub fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType {
        self.typeid_defs
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid MIRTypeId {} in AST!", id))
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

    pub fn ptr_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.ptr_inner().map(|id| self.resolve_type_id(id))
    }

    pub fn mem_ref_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.mem_ref_inner().map(|id| self.resolve_type_id(id))
    }

    pub fn is_c_str(&self, ty: &MIRType) -> bool {
        <Self as MIRTypeContext>::is_c_str(self, ty)
    }

    pub fn is_cx_str(&self, ty: &MIRType) -> bool {
        <Self as MIRTypeContext>::is_cx_str(self, ty)
    }

    pub fn get_mut(&mut self, id: MIRTypeId) -> Option<&mut MIRType> {
        self.typeid_defs.get_mut(&id)
    }

    pub fn contains(&self, id: MIRTypeId) -> bool {
        self.typeid_defs.contains_key(&id)
    }

    pub fn push_scope(&mut self) {
        self.local_symbols.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.local_symbols.pop_scope();
    }

    pub fn add_type(&mut self, name: String, ty: MIRType) -> Option<MIRType> {
        let old = self.realized_types.insert(name.clone(), ty.clone());
        if let Some(id) = ty
            .get_name()
            .and_then(|name| self.named_type_ids.get(name.as_str()).copied())
        {
            self.typeid_defs.insert(id, ty);
            self.insert_type_symbol(QualifiedName::new_raw(CXIdent::new(name)), id);
        } else {
            let id = self.generate_type_id(ty);
            self.named_type_ids.insert(name.clone(), id);
            self.insert_type_symbol(QualifiedName::new_raw(CXIdent::new(name)), id);
        }
        old
    }

    pub fn bind_template_types(
        &mut self,
        names: &[CXIdent],
        args: &[MIRType],
    ) -> Result<TemplateBindingFrame, String> {
        if names.len() != args.len() {
            return Err(format!(
                "Template argument count mismatch: expected {}, got {}",
                names.len(),
                args.len()
            ));
        }

        self.push_scope();
        let mut bindings = Vec::new();

        for (name, arg) in names.iter().zip(args) {
            bindings.push((
                name.as_string(),
                self.realized_types.insert(name.as_string(), arg.clone()),
            ));
            let id = self.generate_type_id(arg.clone());
            self.local_symbols
                .insert(QualifiedName::new_raw(name.clone()), MIRSymbol::Type(id));
        }

        Ok(TemplateBindingFrame { bindings })
    }

    pub fn restore_template_types(&mut self, frame: TemplateBindingFrame) {
        for (name, previous) in frame.bindings {
            match previous {
                Some(previous) => {
                    self.realized_types.insert(name, previous);
                }
                None => {
                    self.realized_types.remove(&name);
                }
            }
        }
        self.pop_scope();
    }

    pub fn get_or_create_named_type_id(&mut self, name: &str) -> MIRTypeId {
        if let Some(id) = self.named_type_ids.get(name) {
            return *id;
        }

        let id = self.generate_type_id(MIRType {
            strong_identifier: Some(QualifiedName::new_raw(CXIdent::new(name))),
            kind: MIRTypeKind::Undefined,
            ..Default::default()
        });
        self.valid_typeid.insert(id);
        self.named_type_ids.insert(name.to_string(), id);
        self.insert_type_symbol(QualifiedName::new_raw(CXIdent::new(name)), id);
        id
    }

    pub fn register_identifier(&mut self, name: CXIdent, id: MIRTypeId) {
        self.named_type_ids.insert(name.as_string(), id);
        self.insert_type_symbol(QualifiedName::new_raw(name), id);
    }

    pub fn get_named_type_id(&self, name: &str) -> Option<MIRTypeId> {
        self.named_type_ids.get(name).copied()
    }

    pub fn mark_type_defining(&mut self, id: MIRTypeId) {
        self.currently_defining_types.insert(id);
        self.definition_stack.push(id);
    }

    pub fn finish_type_definition(
        &mut self,
        id: MIRTypeId,
        definition: MIRType,
    ) -> Option<MIRType> {
        self.currently_defining_types.remove(&id);
        self.remove_from_definition_stack(id);
        if let Some(name) = definition.get_name() {
            self.realized_types
                .insert(name.as_string(), definition.clone());
            self.insert_type_symbol(QualifiedName::new_raw(name.clone()), id);
        }
        self.typeid_defs.insert(id, definition)
    }

    pub fn is_type_defining(&self, id: MIRTypeId) -> bool {
        self.currently_defining_types.contains(&id)
    }

    pub fn abort_type_definition(&mut self, id: MIRTypeId) {
        self.currently_defining_types.remove(&id);
        self.remove_from_definition_stack(id);
    }

    pub fn has_complete_named_type_definition(&self, id: MIRTypeId) -> bool {
        self.typeid_defs
            .get(&id)
            .map(|definition| !matches!(definition.kind, MIRTypeKind::Undefined))
            .unwrap_or(false)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.typeid_defs.get(&id)
    }

    pub fn intern_type(&mut self, ty: MIRType) -> MIRTypeId {
        if let Some(name) = ty.get_name()
            && let Some(id) = self.named_type_ids.get(name.as_str()).copied()
        {
            self.typeid_defs.entry(id).or_insert(ty);
            return id;
        }

        let id = self.generate_type_id(ty.clone());
        if let Some(name) = ty.get_name() {
            self.named_type_ids.insert(name.as_string(), id);
            self.insert_type_symbol(QualifiedName::new_raw(name.clone()), id);
        }
        id
    }

    pub fn update_named_type_metadata(
        &mut self,
        id: MIRTypeId,
        new_name: QualifiedName,
        template_info: Option<Box<TemplateInfo>>,
    ) {
        let Some(existing) = self.typeid_defs.get_mut(&id) else {
            return;
        };

        existing.strong_identifier = Some(new_name.clone());
        existing
            .debug_name
            .get_or_insert_with(|| new_name.name.clone());
        existing.template_info = template_info;
        self.named_type_ids.insert(new_name.as_flat_name(), id);
        self.insert_type_symbol(new_name, id);
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
        self.realized_types.get(name).cloned().or_else(|| {
            self.get_named_type_id(name)
                .and_then(|id| self.typeid_defs.get(&id).cloned())
        })
    }

    pub fn resolve_value_symbol(&self, name: &str) -> Option<ResolvedValueSymbol> {
        let symbol = self
            .local_symbols
            .iter()
            .find(|(key, _)| key.name.as_str() == name)
            .map(|(_, symbol)| symbol)
            .or_else(|| {
                self.global_cache
                    .iter()
                    .find(|(key, _)| key.name.as_str() == name)
                    .map(|(_, symbol)| symbol)
            })?;

        match symbol {
            MIRSymbol::PureValue(value) => Some(ResolvedValueSymbol::PureValue(value.clone())),
            MIRSymbol::Value(value) => {
                let origin = match value.kind {
                    MIRExpressionKind::Variable { location, .. } => Some(location),
                    _ => None,
                };
                Some(ResolvedValueSymbol::Value {
                    value: value.clone(),
                    origin,
                })
            }
            _ => None,
        }
    }

    pub fn resolve_pure_symbol(&self, name: &str) -> Option<MIRPureExpression> {
        match self.resolve_value_symbol(name) {
            Some(ResolvedValueSymbol::PureValue(value)) => Some(value),
            _ => None,
        }
    }

    pub fn resolve_symbol_as_value(&self, name: &str) -> Option<MIRExpression> {
        match self.resolve_value_symbol(name) {
            Some(ResolvedValueSymbol::PureValue(value)) => Some(value.as_value()),
            Some(ResolvedValueSymbol::Value { value, .. }) => Some(value),
            None => None,
        }
    }

    pub fn type_eq(&self, type1: &MIRType, type2: &MIRType) -> bool {
        type1.contextual_eq(type2, self)
    }

    pub fn apply_qualifiers(mut ty: MIRType, specifiers: CXTypeQualifiers) -> MIRType {
        ty.specifiers = specifiers;
        ty
    }

    pub fn is_const_qualified(ty: &MIRType) -> bool {
        ty.get_specifier(CX_CONST)
    }

    pub fn is_copyable(&self, ty: &MIRType) -> bool {
        !self.is_nocopy(ty)
    }

    pub fn is_nocopy(&self, ty: &MIRType) -> bool {
        ty.is_nocopy()
    }

    pub fn aggregate_fields(&self, ty: &MIRType) -> Option<Vec<(String, MIRType)>> {
        ty.aggregate_fields(self)
    }

    pub fn mangle(&self, ty: &MIRType) -> String {
        type_mangle(self, ty)
    }

    fn remove_from_definition_stack(&mut self, id: MIRTypeId) {
        if self.definition_stack.last().copied() == Some(id) {
            self.definition_stack.pop();
        } else if let Some(index) = self
            .definition_stack
            .iter()
            .rposition(|stack_id| *stack_id == id)
        {
            self.definition_stack.remove(index);
        }
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

    pub fn bitfield_mem_ref_to(&mut self, ty: MIRType, bitfield: MIRBitfieldAccess) -> MIRType {
        let id = self.generate_type_id(ty);

        MIRTypeKind::MemoryReference {
            inner_type: id,
            bitfield: Some(bitfield),
        }
        .into()
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
        self.typeid_map
            .get(&id)
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
