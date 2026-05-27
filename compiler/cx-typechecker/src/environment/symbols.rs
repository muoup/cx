use std::collections::{HashMap, HashSet};

use cx_ast::data::{CX_CONST, CXTypeQualifiers};
use cx_mir::CXTypeMap;
use cx_mir::intrinsic_types::INTRINSIC_TYPES;
use cx_mir::mir::data::{
    MIRFunctionPrototype, MIRType, MIRTypeContext, MIRTypeId, MIRTypeKind, TemplateInfo,
};
use cx_mir::mir::expression::{MIRExpression, MIRPureExpression};
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;

pub(crate) mod completion;
pub(crate) mod templates;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct SymbolId(u64);

#[derive(Clone, Debug)]
#[allow(dead_code)]
enum SymbolDefinition {
    Type(MIRType),
    Value(MIRExpression),
    PureValue(MIRPureExpression),
}

impl SymbolDefinition {
    fn as_value(&self) -> Option<MIRExpression> {
        match self {
            Self::Value(value) => Some(value.clone()),
            Self::PureValue(value) => Some(value.as_value()),
            Self::Type(_) => None,
        }
    }

    fn as_pure(&self) -> Option<MIRPureExpression> {
        match self {
            Self::PureValue(value) => Some(value.clone()),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolValueOrigin {
    Local,
    Global,
    Contract,
}

#[derive(Clone, Debug, Default)]
struct SymbolMetadata {
    value_origin: Option<SymbolValueOrigin>,
}

#[derive(Clone, Debug)]
struct Symbol {
    name: CXIdent,
    definition: SymbolDefinition,
    metadata: SymbolMetadata,
}

struct SymbolStore {
    next_id: u64,
    symbols: HashMap<SymbolId, Symbol>,
}

impl SymbolStore {
    fn new() -> Self {
        Self {
            next_id: 1,
            symbols: HashMap::new(),
        }
    }

    fn insert(
        &mut self,
        name: CXIdent,
        definition: SymbolDefinition,
        metadata: SymbolMetadata,
    ) -> SymbolId {
        let id = SymbolId(self.next_id);
        self.next_id += 1;
        self.symbols.insert(
            id,
            Symbol {
                name,
                definition,
                metadata,
            },
        );
        id
    }

    fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.get(&id)
    }
}

struct SymbolScope {
    bindings: ScopedMap<String, SymbolId>,
}

impl SymbolScope {
    fn new() -> Self {
        Self {
            bindings: ScopedMap::new_with_starting_scope(),
        }
    }

    fn push_scope(&mut self) {
        self.bindings.push_scope();
    }

    fn pop_scope(&mut self) {
        self.bindings.pop_scope();
    }

    fn insert(&mut self, name: String, symbol: SymbolId) {
        self.bindings.insert(name, symbol);
    }

    fn get(&self, name: &str) -> Option<SymbolId> {
        self.bindings.get(name).copied()
    }
}

pub struct SymbolRegistry {
    pub context: MIRTypeContext,
    pub realized_types: CXTypeMap,
    pub named_type_ids: HashMap<String, MIRTypeId>,
    currently_defining_types: HashSet<MIRTypeId>,
    definition_stack: Vec<MIRTypeId>,
    next_type_id: u64,
    symbols: SymbolStore,
    scope: SymbolScope,
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

impl SymbolRegistry {
    pub fn with_intrinsics() -> Self {
        let mut realized_types = HashMap::new();
        let mut context = MIRTypeContext::default();
        let mut named_type_ids = HashMap::new();
        let mut symbols = SymbolStore::new();
        let mut scope = SymbolScope::new();
        let mut next_type_id = 1u64;

        for (name, ty_kind) in INTRINSIC_TYPES {
            let ty: MIRType = ty_kind.clone().into();
            let id = MIRTypeId(next_type_id);
            next_type_id += 1;

            context.insert(id, ty.clone());
            context.register_identifier(CXIdent::new(*name), id);
            named_type_ids.insert((*name).to_string(), id);
            realized_types.insert((*name).to_string(), ty);
            let symbol_id = symbols.insert(
                CXIdent::new(*name),
                SymbolDefinition::Type(context.get(id).cloned().unwrap()),
                SymbolMetadata::default(),
            );
            scope.insert((*name).to_string(), symbol_id);
        }

        Self {
            context,
            realized_types,
            named_type_ids,
            currently_defining_types: HashSet::new(),
            definition_stack: Vec::new(),
            next_type_id,
            symbols,
            scope,
        }
    }

    pub fn push_scope(&mut self) {
        self.scope.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.scope.pop_scope();
    }

    pub fn add_type(&mut self, name: String, ty: MIRType) -> Option<MIRType> {
        let old = self.realized_types.remove(&name);
        self.realized_types.insert(name.clone(), ty.clone());
        let symbol_id = self.symbols.insert(
            CXIdent::new(name.as_str()),
            SymbolDefinition::Type(ty),
            SymbolMetadata::default(),
        );
        self.scope.insert(name.clone(), symbol_id);
        old
    }

    pub fn bind_template_types(
        &mut self,
        names: &[String],
        args: &[MIRType],
    ) -> Result<TemplateBindingFrame, String> {
        if names.len() != args.len() {
            return Err(format!(
                "Template argument count mismatch: expected {}, got {}",
                names.len(),
                args.len()
            ));
        }

        self.scope.push_scope();
        let mut overwritten = Vec::new();

        for (name, arg_type) in names.iter().zip(args.iter()) {
            let symbol_id = self.symbols.insert(
                CXIdent::new(name.as_str()),
                SymbolDefinition::Type(arg_type.clone()),
                SymbolMetadata::default(),
            );
            self.scope.insert(name.clone(), symbol_id);
            overwritten.push((
                name.clone(),
                self.realized_types.insert(name.clone(), arg_type.clone()),
            ));
        }

        Ok(TemplateBindingFrame {
            bindings: overwritten,
        })
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
        self.scope.pop_scope();
    }

    pub fn get_or_create_named_type_id(&mut self, name: &str) -> MIRTypeId {
        if let Some(id) = self.named_type_ids.get(name) {
            return *id;
        }

        let next_available = self.context.types.len() as u64 + 1;
        let id = MIRTypeId(self.next_type_id.max(next_available));
        self.next_type_id = id.0 + 1;
        self.named_type_ids.insert(name.to_string(), id);
        self.context.insert(
            id,
            MIRType {
                strong_identifier: Some(CXIdent::new(name)),
                kind: MIRTypeKind::Undefined,
                ..Default::default()
            },
        );
        let ty = self.context.get(id).cloned().unwrap();
        let symbol_id = self.symbols.insert(
            CXIdent::new(name),
            SymbolDefinition::Type(ty),
            SymbolMetadata::default(),
        );
        self.scope.insert(name.to_string(), symbol_id);
        id
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
            let symbol_id = self.symbols.insert(
                name.clone(),
                SymbolDefinition::Type(definition.clone()),
                SymbolMetadata::default(),
            );
            self.scope.insert(name.as_string(), symbol_id);
        }
        self.context.insert(id, definition)
    }

    pub fn is_type_defining(&self, id: MIRTypeId) -> bool {
        self.currently_defining_types.contains(&id)
    }

    pub fn abort_type_definition(&mut self, id: MIRTypeId) {
        self.currently_defining_types.remove(&id);
        self.remove_from_definition_stack(id);
    }

    pub fn has_complete_named_type_definition(&self, id: MIRTypeId) -> bool {
        self.context
            .get(id)
            .map(|definition| !matches!(definition.kind, MIRTypeKind::Undefined))
            .unwrap_or(false)
    }

    pub fn get_named_type_definition(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.context.get(id)
    }

    pub fn intern_type(&mut self, ty: MIRType) -> MIRTypeId {
        if let Some(name) = ty.get_name()
            && let Some(id) = self.named_type_ids.get(name.as_str()).copied()
        {
            self.context.register_identifier(name.clone(), id);
            if self.context.get(id).is_none() {
                self.context.insert(id, ty);
            }

            return id;
        }

        let id = self.context.intern(ty.clone());
        if let Some(name) = ty.get_name() {
            self.context.register_identifier(name.clone(), id);
        }
        id
    }

    pub fn update_named_type_metadata(
        &mut self,
        id: MIRTypeId,
        new_name: CXIdent,
        template_info: Option<Box<TemplateInfo>>,
    ) {
        let Some(existing) = self.context.get_mut(id) else {
            return;
        };

        existing.strong_identifier = Some(new_name.clone());
        existing.debug_name.get_or_insert_with(|| new_name.clone());
        existing.template_info = template_info.clone();
        self.context.register_identifier(new_name, id);
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
        if let Some(symbol_id) = self.scope.get(name)
            && let Some(symbol) = self.symbols.get(symbol_id)
            && symbol.name.as_str() == name
        {
            match &symbol.definition {
                SymbolDefinition::Type(ty) => return Some(ty.clone()),
                _ => return None,
            }
        }

        if let Some(id) = self.get_named_type_id(name)
            && let Some(definition) = self.get_named_type_definition(id)
            && definition
                .get_name()
                .map(|ident| ident.as_str() == name)
                .unwrap_or(false)
        {
            return Some(definition.clone());
        }

        self.realized_types.get(name).cloned()
    }

    pub fn insert_value(
        &mut self,
        name: CXIdent,
        expr: MIRExpression,
        origin: Option<SymbolValueOrigin>,
    ) {
        let symbol_id = self.symbols.insert(
            name.clone(),
            SymbolDefinition::Value(expr),
            SymbolMetadata {
                value_origin: origin,
            },
        );
        self.scope.insert(name.as_string(), symbol_id);
    }

    pub fn insert_pure_value(&mut self, name: CXIdent, expr: MIRPureExpression) {
        let symbol_id = self.symbols.insert(
            name.clone(),
            SymbolDefinition::PureValue(expr),
            SymbolMetadata::default(),
        );
        self.scope.insert(name.as_string(), symbol_id);
    }

    pub fn insert_function_symbol(&mut self, name: CXIdent, prototype: MIRFunctionPrototype) {
        self.insert_pure_value(
            name,
            MIRPureExpression::FunctionReference(Box::new(prototype)),
        );
    }

    pub fn resolve_value_symbol(&self, name: &str) -> Option<ResolvedValueSymbol> {
        let symbol_id = self.scope.get(name)?;
        let symbol = self.symbols.get(symbol_id)?;

        if let Some(value) = symbol.definition.as_pure() {
            return Some(ResolvedValueSymbol::PureValue(value));
        }

        symbol
            .definition
            .as_value()
            .map(|value| ResolvedValueSymbol::Value {
                value,
                origin: symbol.metadata.value_origin,
            })
    }

    pub fn resolve_pure_symbol(&self, name: &str) -> Option<MIRPureExpression> {
        let symbol_id = self.scope.get(name)?;
        let symbol = self.symbols.get(symbol_id)?;
        symbol.definition.as_pure()
    }

    pub fn resolve_symbol_as_value(&self, name: &str) -> Option<MIRExpression> {
        let symbol_id = self.scope.get(name)?;
        let symbol = self.symbols.get(symbol_id)?;
        symbol.definition.as_value()
    }

    pub fn is_copyable(&self, ty: &MIRType) -> bool {
        !self.is_nocopy(ty)
    }

    pub fn is_nocopy(&self, ty: &MIRType) -> bool {
        ty.struct_attributes()
            .map(|attributes| attributes.nocopy || attributes.nodrop)
            .unwrap_or(false)
    }

    pub fn is_nodrop(&self, ty: &MIRType) -> bool {
        ty.struct_attributes()
            .map(|attributes| attributes.nodrop)
            .unwrap_or(false)
    }

    pub fn type_eq(&self, type1: &MIRType, type2: &MIRType) -> bool {
        self.context.type_eq(type1, type2)
    }

    pub fn apply_qualifiers(mut ty: MIRType, specifiers: CXTypeQualifiers) -> MIRType {
        ty.specifiers = specifiers;
        ty
    }

    pub fn is_const_qualified(ty: &MIRType) -> bool {
        ty.get_specifier(CX_CONST)
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
}
