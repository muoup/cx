use cx_ast::{ast::VisibilityMode, data::CXTypeQualifiers};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::mir::{
    data::{MIRFunctionSignature, MIRTemplateInput, TemplateInfo},
    name_mangling::type_mangle,
};

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct MIRTypeContext {
    pub type_identifiers: Vec<(CXIdent, MIRTypeId)>,
    pub types: Vec<MIRType>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Readable, Writable)]
pub struct MIRTypeId(pub u64);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct MIRType {
    pub visibility: VisibilityMode,
    pub specifiers: CXTypeQualifiers,
    pub move_attributes: MIRMoveAttributes,
    pub strong_identifier: Option<CXIdent>,

    pub template_info: Option<Box<TemplateInfo>>,
    pub kind: MIRTypeKind,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub struct MIRMoveAttributes {
    pub nocopy: bool,
    pub nodrop: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum MIRTypeKind {
    Unit,
    Integer {
        _type: MIRIntegerType,
        signed: bool,
    },
    Float {
        _type: MIRFloatType,
    },
    Structured {
        fields: Vec<(String, MIRTypeId)>,
    },
    Union {
        variants: Vec<(String, MIRTypeId)>,
    },
    TaggedUnion {
        variants: Vec<(String, MIRTypeId)>,
    },
    PointerTo {
        inner_type: MIRTypeId,
    },
    MemoryReference {
        inner_type: MIRTypeId,
    },
    Array {
        length: usize,
        inner_type: MIRTypeId,
    },
    Function {
        signature: Box<MIRFunctionSignature>,
    },
    Opaque {
        size: usize,
    },
    Undefined,
    Str,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Readable, Writable)]
pub enum MIRIntegerType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Readable, Writable)]
pub enum MIRFloatType {
    F32,
    F64,
}

impl MIRIntegerType {
    pub const fn rank(&self) -> u8 {
        match self {
            MIRIntegerType::I1 => 0,
            MIRIntegerType::I8 => 1,
            MIRIntegerType::I16 => 2,
            MIRIntegerType::I32 => 3,
            MIRIntegerType::I64 => 4,
            MIRIntegerType::I128 => 5,
        }
    }
    
    pub const fn bytes(&self) -> usize {
        match self {
            MIRIntegerType::I1 => 1,
            MIRIntegerType::I8 => 1,
            MIRIntegerType::I16 => 2,
            MIRIntegerType::I32 => 4,
            MIRIntegerType::I64 => 8,
            MIRIntegerType::I128 => 16,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            1 => Some(MIRIntegerType::I8),
            2 => Some(MIRIntegerType::I16),
            4 => Some(MIRIntegerType::I32),
            8 => Some(MIRIntegerType::I64),
            16 => Some(MIRIntegerType::I128),
            _ => None,
        }
    }
}

impl MIRFloatType {
    pub const fn bytes(&self) -> usize {
        match self {
            MIRFloatType::F32 => 4,
            MIRFloatType::F64 => 8,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            4 => Some(MIRFloatType::F32),
            8 => Some(MIRFloatType::F64),
            _ => None,
        }
    }
}

impl MIRTypeContext {
    fn index_of(id: MIRTypeId) -> Option<usize> {
        id.0.checked_sub(1).map(|idx| idx as usize)
    }

    fn undefined_type() -> MIRType {
        MIRType {
            kind: MIRTypeKind::Undefined,
            ..Default::default()
        }
    }

    pub fn insert(&mut self, id: MIRTypeId, ty: MIRType) -> Option<MIRType> {
        let idx = Self::index_of(id)?;

        if idx == self.types.len() {
            self.types.push(ty);
            return None;
        }

        if self.types.len() < idx {
            self.types.resize_with(idx, Self::undefined_type);
            self.types.push(ty);
            return None;
        }

        Some(std::mem::replace(&mut self.types[idx], ty))
    }

    pub fn intern(&mut self, ty: MIRType) -> MIRTypeId {
        if let Some((idx, _)) = self
            .types
            .iter()
            .enumerate()
            .find(|(_, existing)| *existing == &ty)
        {
            return MIRTypeId(idx as u64 + 1);
        }

        self.types.push(ty);
        MIRTypeId(self.types.len() as u64)
    }

    pub fn get(&self, id: MIRTypeId) -> Option<&MIRType> {
        self.types.get(Self::index_of(id)?)
    }

    pub fn get_mut(&mut self, id: MIRTypeId) -> Option<&mut MIRType> {
        self.types.get_mut(Self::index_of(id)?)
    }

    pub fn contains(&self, id: MIRTypeId) -> bool {
        self.get(id)
            .map(|ty| !matches!(ty.kind, MIRTypeKind::Undefined))
            .unwrap_or(false)
    }

    pub fn register_identifier(&mut self, name: CXIdent, id: MIRTypeId) {
        if let Some((_, existing_id)) = self
            .type_identifiers
            .iter_mut()
            .find(|(existing_name, _)| existing_name == &name)
        {
            *existing_id = id;
            return;
        }

        self.type_identifiers.push((name, id));
    }

    pub fn identifier_id(&self, name: &str) -> Option<MIRTypeId> {
        self.type_identifiers
            .iter()
            .find(|(existing_name, _)| existing_name.as_str() == name)
            .map(|(_, id)| *id)
    }

    pub fn type_id(&self, ty: &MIRType) -> Option<MIRTypeId> {
        self.types
            .iter()
            .enumerate()
            .find(|(_, existing)| *existing == ty)
            .map(|(idx, _)| MIRTypeId(idx as u64 + 1))
    }

    pub fn pointer_to(&mut self, inner_type: MIRType) -> MIRType {
        let inner_id = self.intern(inner_type);
        MIRType {
            kind: MIRTypeKind::PointerTo {
                inner_type: inner_id,
            },
            ..Default::default()
        }
    }

    pub fn mem_ref_to(&mut self, inner_type: MIRType) -> MIRType {
        let inner_id = self.intern(inner_type);
        MIRType {
            kind: MIRTypeKind::MemoryReference {
                inner_type: inner_id,
            },
            ..Default::default()
        }
    }

    pub fn mem_ref_inner_id(&self, ty: &MIRType) -> Option<MIRTypeId> {
        let MIRTypeKind::MemoryReference { inner_type } = &ty.kind else {
            return None;
        };

        Some(*inner_type)
    }

    pub fn array_inner_id(&self, ty: &MIRType) -> Option<MIRTypeId> {
        let MIRTypeKind::Array { inner_type, .. } = &ty.kind else {
            return None;
        };

        Some(*inner_type)
    }

    pub fn ptr_inner_id(&self, ty: &MIRType) -> Option<MIRTypeId> {
        let MIRTypeKind::PointerTo { inner_type } = &ty.kind else {
            return None;
        };

        Some(*inner_type)
    }

    pub fn mem_ref_inner<'a>(&'a self, ty: &MIRType) -> Option<&'a MIRType> {
        self.get(self.mem_ref_inner_id(ty)?)
    }

    pub fn array_inner<'a>(&'a self, ty: &MIRType) -> Option<&'a MIRType> {
        self.get(self.array_inner_id(ty)?)
    }

    pub fn ptr_inner<'a>(&'a self, ty: &MIRType) -> Option<&'a MIRType> {
        self.get(self.ptr_inner_id(ty)?)
    }

    pub fn memory_resident_type<'a>(&'a self, ty: &'a MIRType) -> &'a MIRType {
        self.mem_ref_inner(ty)
            .or_else(|| self.ptr_inner(ty))
            .unwrap_or(ty)
    }

    pub fn is_mutable_memory_reference(&self, ty: &MIRType) -> bool {
        let Some(inner_type) = self.mem_ref_inner(ty) else {
            return false;
        };

        !inner_type.get_specifier(cx_ast::data::CX_CONST)
    }

    pub fn aggregate_fields<'a>(&'a self, ty: &'a MIRType) -> Option<&'a Vec<(String, MIRTypeId)>> {
        match &ty.kind {
            MIRTypeKind::Structured { fields } => Some(fields),
            MIRTypeKind::Union { variants } | MIRTypeKind::TaggedUnion { variants } => {
                Some(variants)
            }
            _ => None,
        }
    }

    pub fn mangle(&self, ty: &MIRType) -> String {
        type_mangle(self, ty)
    }

    pub fn type_size(&self, ty: &MIRType) -> usize {
        match &ty.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes(),
            MIRTypeKind::Float { _type } => _type.bytes(),
            MIRTypeKind::Unit => 0,
            MIRTypeKind::Opaque { size } => *size,
            MIRTypeKind::MemoryReference { .. } | MIRTypeKind::PointerTo { .. } => {
                std::mem::size_of::<usize>()
            }
            MIRTypeKind::Structured { fields } => {
                let mut offset = 0usize;

                for (_, field_id) in fields {
                    let field_type = self
                        .get(*field_id)
                        .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
                    let align = self.type_alignment(field_type);
                    offset = offset.div_ceil(align) * align;
                    offset += self.type_size(field_type);
                }

                offset
            }
            MIRTypeKind::Union { variants } => variants
                .iter()
                .map(|(_, field_id)| {
                    let field_type = self
                        .get(*field_id)
                        .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
                    self.type_size(field_type)
                })
                .max()
                .unwrap_or(0),
            MIRTypeKind::Array { length: size, inner_type } => {
                let inner_type = self
                    .get(*inner_type)
                    .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
                size * self.padded_size(inner_type)
            }
            MIRTypeKind::TaggedUnion { variants } => {
                variants
                    .iter()
                    .map(|(_, field_id)| {
                        let field_type = self
                            .get(*field_id)
                            .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
                        self.type_size(field_type)
                    })
                    .max()
                    .unwrap_or(0)
                    + 1
            }
            MIRTypeKind::Undefined => unreachable!("Incomplete type has no type_size"),
            MIRTypeKind::Str => unreachable!("str is unsized and has no type_size"),
            MIRTypeKind::Function { .. } => unreachable!("Function has no type_size"),
        }
    }

    pub fn padded_size(&self, ty: &MIRType) -> usize {
        let size = self.type_size(ty);
        let align = self.type_alignment(ty);
        size.div_ceil(align) * align
    }

    pub fn type_alignment(&self, ty: &MIRType) -> usize {
        match &ty.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes().min(8),
            MIRTypeKind::Float { _type } => _type.bytes().min(8),
            MIRTypeKind::Unit => 1,
            MIRTypeKind::Opaque { size } => (*size).min(8),
            MIRTypeKind::MemoryReference { .. } | MIRTypeKind::PointerTo { .. } => {
                std::mem::size_of::<usize>()
            }
            MIRTypeKind::Structured { fields } => fields
                .iter()
                .map(|(_, field_id)| {
                    let field_type = self
                        .get(*field_id)
                        .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
                    self.type_alignment(field_type)
                })
                .max()
                .unwrap_or(8),
            MIRTypeKind::Union { variants } | MIRTypeKind::TaggedUnion { variants } => variants
                .iter()
                .map(|(_, field_id)| {
                    let field_type = self
                        .get(*field_id)
                        .unwrap_or_else(|| panic!("Unknown type id {}", field_id.0));
                    self.type_alignment(field_type)
                })
                .max()
                .unwrap_or(8),
            MIRTypeKind::Array { inner_type, .. } => {
                let inner_type = self
                    .get(*inner_type)
                    .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
                self.type_alignment(inner_type)
            }
            MIRTypeKind::Undefined => unreachable!("Incomplete type has no type_alignment"),
            MIRTypeKind::Str => unreachable!("str is unsized and has no type_alignment"),
            MIRTypeKind::Function { .. } => unreachable!("Function has no type_alignment"),
        }
    }
}

impl Default for MIRType {
    fn default() -> Self {
        MIRType {
            visibility: VisibilityMode::Private,
            specifiers: CXTypeQualifiers::default(),
            move_attributes: MIRMoveAttributes::default(),
            strong_identifier: None,
            template_info: None,
            kind: MIRTypeKind::Unit,
        }
    }
}

impl MIRType {
    pub fn unit() -> Self {
        Self::default()
    }

    pub fn bool() -> Self {
        MIRType {
            kind: MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            },
            ..Default::default()
        }
    }

    pub fn with_name(mut self, name: CXIdent) -> MIRType {
        self.strong_identifier = Some(name);
        self
    }

    pub fn set_visibility_mode(&mut self, visibility: VisibilityMode) -> &mut Self {
        self.visibility = visibility;
        self
    }

    pub fn add_specifier(mut self, specifier: CXTypeQualifiers) -> Self {
        self.specifiers |= specifier;
        self
    }

    pub fn with_specifier(&self, specifier: CXTypeQualifiers) -> Self {
        self.clone().add_specifier(specifier)
    }

    pub fn remove_specifier(&mut self, specifier: CXTypeQualifiers) -> &mut Self {
        self.specifiers &= !specifier;
        self
    }

    pub fn without_specifier(&self, specifier: CXTypeQualifiers) -> Self {
        let mut clone = self.clone();
        clone.remove_specifier(specifier);
        clone
    }

    pub fn get_specifier(&self, specifier: CXTypeQualifiers) -> bool {
        self.specifiers & specifier == specifier
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, MIRTypeKind::PointerTo { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Array { .. })
    }

    pub fn is_memory_resident(&self) -> bool {
        matches!(
            self.kind,
            MIRTypeKind::Structured { .. }
                | MIRTypeKind::Union { .. }
                | MIRTypeKind::TaggedUnion { .. }
                | MIRTypeKind::Array { .. }
        )
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Opaque { .. })
    }

    pub fn is_tagged_union(&self) -> bool {
        matches!(self.kind, MIRTypeKind::TaggedUnion { .. })
    }
    
    pub fn is_c_union(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Union { .. })
    }
    
    pub fn is_union(&self) -> bool {
        self.is_tagged_union() || self.is_c_union()
    }

    pub fn is_str(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Str)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Function { .. })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Integer { .. })
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Float { .. })
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Unit)
    }

    pub fn is_structure(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Structured { .. })
    }

    pub fn struct_attributes(&self) -> Option<MIRMoveAttributes> {
        match self.kind {
            MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. } => Some(self.move_attributes),
            _ => None,
        }
    }

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, MIRTypeKind::MemoryReference { .. })
    }

    pub fn get_name(&self) -> Option<&CXIdent> {
        self.strong_identifier.as_ref()
    }

    pub fn get_base_identifier(&self) -> Option<&CXIdent> {
        self.template_info
            .as_ref()
            .map(|info| &info.base_name)
            .or(self.strong_identifier.as_ref())
    }

    pub fn get_template_data(&self) -> Option<&TemplateInfo> {
        self.template_info.as_deref()
    }

    pub fn function_signature(&self) -> Option<&MIRFunctionSignature> {
        match &self.kind {
            MIRTypeKind::Function { signature } => Some(signature),
            _ => None,
        }
    }

    pub fn add_template_info(&mut self, new_name: CXIdent, template_input: MIRTemplateInput) {
        let old_name = self
            .strong_identifier
            .clone()
            .unwrap_or_else(|| new_name.clone());
        self.strong_identifier = Some(new_name);
        self.template_info = Some(Box::new(TemplateInfo {
            base_name: old_name,
            template_input,
        }));
    }

    pub fn was_template_instantiated(&self) -> bool {
        self.template_info.is_some()
    }

    pub fn set_name(&mut self, new_name: CXIdent) {
        self.strong_identifier = Some(new_name);
    }

    pub fn named_struct(
        name: CXIdent,
        _type_id: MIRTypeId,
        template_info: Option<Box<TemplateInfo>>,
        attributes: MIRMoveAttributes,
    ) -> Self {
        MIRType {
            strong_identifier: Some(name),
            template_info,
            move_attributes: attributes,
            kind: MIRTypeKind::Structured { fields: vec![] },
            ..Default::default()
        }
    }

    pub fn named_union(name: CXIdent, _type_id: MIRTypeId) -> Self {
        MIRType {
            strong_identifier: Some(name),
            kind: MIRTypeKind::Union { variants: vec![] },
            ..Default::default()
        }
    }

    pub fn named_tagged_union(
        name: CXIdent,
        _type_id: MIRTypeId,
        template_info: Option<Box<TemplateInfo>>,
        attributes: MIRMoveAttributes,
    ) -> Self {
        MIRType {
            strong_identifier: Some(name),
            template_info,
            move_attributes: attributes,
            kind: MIRTypeKind::TaggedUnion { variants: vec![] },
            ..Default::default()
        }
    }

    pub fn named_type_id(&self, definitions: &MIRTypeContext) -> Option<MIRTypeId> {
        self.get_name()
            .and_then(|name| definitions.identifier_id(name.as_str()))
    }

    pub fn aggregate_fields(&self, definitions: &MIRTypeContext) -> Option<Vec<(String, MIRType)>> {
        definitions.aggregate_fields(self).map(|fields| {
            fields
                .iter()
                .map(|(name, id)| {
                    (
                        name.clone(),
                        definitions
                            .get(*id)
                            .unwrap_or_else(|| panic!("Unknown type id {}", id.0))
                            .clone(),
                    )
                })
                .collect()
        })
    }

    pub fn is_named_aggregate_complete(&self, definitions: &MIRTypeContext) -> bool {
        self.named_type_id(definitions)
            .map(|id| definitions.contains(id))
            .unwrap_or(true)
    }

    pub fn rewrite_named_type_metadata(
        &mut self,
        _target_id: MIRTypeId,
        new_name: &CXIdent,
        template_info: &Option<Box<TemplateInfo>>,
    ) {
        self.strong_identifier = Some(new_name.clone());
        self.template_info = template_info.clone();
    }

    pub fn type_size(&self, definitions: &MIRTypeContext) -> usize {
        definitions.type_size(self)
    }

    pub fn padded_size(&self, definitions: &MIRTypeContext) -> usize {
        definitions.padded_size(self)
    }

    pub fn type_alignment(&self, definitions: &MIRTypeContext) -> usize {
        definitions.type_alignment(self)
    }

    pub fn internal_function() -> Self {
        MIRType::from(MIRTypeKind::Function {
            signature: Box::new(MIRFunctionSignature {
                return_type: MIRType::unit(),
                params: vec![],
                var_args: false,
                contract: crate::mir::expression::MIRFunctionContract::default(),
            }),
        })
        .with_name(CXIdent::from("__internal_function"))
    }
}

impl From<MIRTypeKind> for MIRType {
    fn from(kind: MIRTypeKind) -> Self {
        MIRType {
            kind,
            ..Default::default()
        }
    }
}