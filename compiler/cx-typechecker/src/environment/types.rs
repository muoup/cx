use std::collections::{HashMap, HashSet};

use cx_ast::data::{CX_CONST, CXTypeQualifiers};
use cx_mir::CXTypeMap;
use cx_mir::intrinsic_types::INTRINSIC_TYPES;
use cx_mir::mir::data::{MIRType, MIRTypeContext, MIRTypeId, MIRTypeKind, TemplateInfo};
use cx_util::identifier::CXIdent;

use crate::environment::symbols::{SemanticSymbol, SymbolKind, SymbolStore, TemplateScope};

pub struct TypeRegistry {
    pub context: MIRTypeContext,
    pub realized_types: CXTypeMap,
    pub named_type_ids: HashMap<String, MIRTypeId>,
    currently_defining_types: HashSet<MIRTypeId>,
    definition_stack: Vec<MIRTypeId>,
    next_type_id: u64,
    symbols: SymbolStore,
    template_scope: TemplateScope,
}

pub struct TemplateBindingFrame {
    bindings: Vec<(String, Option<MIRType>)>,
}

impl TypeRegistry {
    pub fn with_intrinsics() -> Self {
        let mut realized_types = HashMap::new();
        let mut context = MIRTypeContext::default();
        let mut named_type_ids = HashMap::new();
        let mut symbols = SymbolStore::new();
        let mut next_type_id = 1u64;

        for (name, ty_kind) in INTRINSIC_TYPES {
            let ty: MIRType = ty_kind.clone().into();
            let id = MIRTypeId(next_type_id);
            next_type_id += 1;

            context.insert(id, ty.clone());
            context.register_identifier(CXIdent::new(*name), id);
            named_type_ids.insert((*name).to_string(), id);
            realized_types.insert((*name).to_string(), ty);
            symbols.insert(
                CXIdent::new(*name),
                SymbolKind::Type,
                SemanticSymbol::Type(id),
            );
        }

        Self {
            context,
            realized_types,
            named_type_ids,
            currently_defining_types: HashSet::new(),
            definition_stack: Vec::new(),
            next_type_id,
            symbols,
            template_scope: TemplateScope::new(),
        }
    }

    pub fn add_type(&mut self, name: String, ty: MIRType) -> Option<MIRType> {
        let old = self.realized_types.remove(&name);
        self.realized_types.insert(name.clone(), ty.clone());
        if let Some(id) = self.named_type_ids.get(&name).copied() {
            self.symbols.insert(
                CXIdent::new(name.as_str()),
                SymbolKind::Type,
                SemanticSymbol::Type(id),
            );
        }
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

        self.template_scope.push_scope();
        let mut overwritten = Vec::new();

        for (name, arg_type) in names.iter().zip(args.iter()) {
            let type_id = self.intern_type(arg_type.clone());
            let symbol_id = self.symbols.insert(
                CXIdent::new(name.as_str()),
                SymbolKind::GenericParam,
                SemanticSymbol::GenericTypeParam {
                    resolved_type: type_id,
                },
            );
            self.template_scope.insert(name.clone(), symbol_id);
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
        self.template_scope.pop_scope();
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
        self.symbols.insert(
            CXIdent::new(name),
            SymbolKind::Type,
            SemanticSymbol::Type(id),
        );
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
        existing.template_info = template_info.clone();
        self.context.register_identifier(new_name, id);
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
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
        type1 == type2
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
