use std::collections::HashSet;

use cx_ast::ast::modifiers::{CXTypeQualifiers, VisibilityMode};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};
use speedy::{Readable, Writable};

use crate::{
    mir::data::{MIRFunctionSignature, TemplateInfo},
    registry::MIRSymbolRegistry, type_context::MIRTypeContext,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Readable, Writable)]
pub struct MIRTypeId(pub u64);

#[derive(Debug, Clone)]
pub struct MIRType {
    pub visibility: VisibilityMode,
    pub specifiers: CXTypeQualifiers,
    pub move_attributes: MIRMoveAttributes,
    pub strong_identifier: Option<QualifiedName>,
    pub debug_name: Option<CXIdent>,

    pub template_info: Option<Box<TemplateInfo>>,
    pub kind: MIRTypeKind,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub struct MIRMoveAttributes {
    pub nocopy: bool,
    pub nodrop: bool,
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum MIRField {
    Standard {
        name: String,
        type_id: MIRTypeId,
    },
    Bitfield {
        name: Option<String>,
        integer_type_id: MIRTypeId,
        width: usize,
    },
}

impl MIRField {
    pub fn standard(name: String, type_id: MIRTypeId) -> Self {
        Self::Standard { name, type_id }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            MIRField::Standard { name, .. } => Some(name.as_str()),
            MIRField::Bitfield { name, .. } => name.as_deref(),
        }
    }

    pub fn ty(&self) -> MIRTypeId {
        match self {
            MIRField::Standard { type_id, .. } => *type_id,
            MIRField::Bitfield {
                integer_type_id, ..
            } => *integer_type_id,
        }
    }

    pub fn standard_parts(&self) -> Option<(&String, MIRTypeId)> {
        match self {
            MIRField::Standard { name, type_id } => Some((name, *type_id)),
            MIRField::Bitfield { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub struct MIRBitfieldAccess {
    pub storage_type: MIRTypeId,
    pub bit_offset: usize,
    pub bit_width: usize,
    pub signed: bool,
}

#[derive(Debug, Clone)]
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
        fields: Vec<MIRField>,
    },
    Union {
        variants: Vec<MIRField>,
    },
    TaggedUnion {
        variants: Vec<MIRField>,
    },
    PointerTo {
        inner_type: MIRTypeId,
    },
    MemoryReference {
        inner_type: MIRTypeId,
        bitfield: Option<MIRBitfieldAccess>,
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

#[derive(Default)]
pub(crate) struct TypeComparisonState {
    compared_ids: HashSet<TypeIdPair>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct TypeIdPair {
    left: MIRTypeId,
    right: MIRTypeId,
}

impl TypeIdPair {
    fn new(left: MIRTypeId, right: MIRTypeId) -> Self {
        if left <= right {
            Self { left, right }
        } else {
            Self {
                left: right,
                right: left,
            }
        }
    }
}

impl MIRTypeId {
    pub fn contextual_eq(&self, other: &Self, definitions: &MIRSymbolRegistry) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        if self == other {
            return true;
        }

        let pair = TypeIdPair::new(*self, *other);
        if state.compared_ids.contains(&pair) {
            return true;
        }

        let left = definitions.resolve_type_id(*self);
        let right = definitions.resolve_type_id(*other);

        state.compared_ids.insert(pair);
        left.contextual_eq_with_state(right, definitions, state)
    }
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

// pub fn bitfield_ref_to(
//         &mut self,
//         inner_type: MIRType,
//         storage_type: MIRType,
//         bit_offset: usize,
//         bit_width: usize,
//         signed: bool,
//     ) -> MIRType {
//         let inner_id = self.intern(inner_type);
//         let storage_id = self.intern(storage_type);
//         MIRType {
//             kind: MIRTypeKind::MemoryReference {
//                 inner_type: inner_id,
//                 bitfield: Some(MIRBitfieldAccess {
//                     storage_type: storage_id,
//                     bit_offset,
//                     bit_width,
//                     signed,
//                 }),
//             },
//             ..Default::default()
//         }
//     }

impl Default for MIRType {
    fn default() -> Self {
        MIRType {
            visibility: VisibilityMode::Private,
            specifiers: CXTypeQualifiers::default(),
            move_attributes: MIRMoveAttributes::default(),
            strong_identifier: None,
            debug_name: None,
            template_info: None,
            kind: MIRTypeKind::Unit,
        }
    }
}

impl MIRType {
    pub fn contextual_eq(&self, other: &Self, definitions: &impl MIRTypeContext) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        if self.specifiers != other.specifiers || self.move_attributes != other.move_attributes {
            return false;
        }

        match (&self.strong_identifier, &other.strong_identifier) {
            (Some(left), Some(right)) => {
                return left == right
                    && match (&self.template_info, &other.template_info) {
                        (Some(left), Some(right)) => left.template_input.contextual_eq_with_state(
                            &right.template_input,
                            definitions,
                            state,
                        ),
                        (None, None) => true,
                        (Some(_), None) | (None, Some(_)) => false,
                    };
            }
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }

        self.kind
            .contextual_eq_with_state(&other.kind, definitions, state)
    }

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

    pub fn internal_function() -> Self {
        MIRType::from(MIRTypeKind::Function {
            signature: Box::new(MIRFunctionSignature::default()),
        })
        .with_name(CXIdent::from("__internal_function"))
    }

    pub fn padded_size(&self, _definitions: &MIRSymbolRegistry) -> usize {
        match &self.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes() as usize,
            MIRTypeKind::Float { _type } => _type.bytes() as usize,
            MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. } => 8,
            MIRTypeKind::Unit => 0,
            MIRTypeKind::Array { length, .. } => *length,
            MIRTypeKind::Opaque { size } => *size,
            _ => 0,
        }
    }

    pub fn with_name(mut self, name: CXIdent) -> MIRType {
        self.strong_identifier = Some(QualifiedName::new_raw(name));
        self
    }

    pub fn with_qualified_name(mut self, name: QualifiedName) -> MIRType {
        self.strong_identifier = Some(name);
        self
    }

    pub fn with_debug_name(mut self, name: CXIdent) -> MIRType {
        self.debug_name = Some(name);
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

    pub fn without_specifiers(mut self) -> Self {
        self.specifiers = 0;
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

    pub fn is_nodrop(&self) -> bool {
        self.struct_attributes().map(|a| a.nodrop).unwrap_or(false)
    }

    pub fn is_nocopy(&self) -> bool {
        self.struct_attributes().map(|a| a.nocopy).unwrap_or(false)
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
        self.strong_identifier.as_ref().map(|name| &name.name)
    }

    pub fn strong_identifier(&self) -> Option<&QualifiedName> {
        self.strong_identifier.as_ref()
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }

    pub fn get_base_identifier(&self) -> Option<&CXIdent> {
        self.template_info
            .as_ref()
            .map(|info| &info.base_name)
            .or_else(|| self.get_name())
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

    pub fn was_template_instantiated(&self) -> bool {
        self.template_info.is_some()
    }

    pub fn set_name(&mut self, new_name: CXIdent) {
        self.strong_identifier = Some(QualifiedName::new_raw(new_name));
    }

    pub fn set_qualified_name(&mut self, new_name: QualifiedName) {
        self.strong_identifier = Some(new_name);
    }

    pub fn set_debug_name(&mut self, new_name: CXIdent) {
        self.debug_name = Some(new_name);
    }

    pub fn named_struct(
        name: CXIdent,
        _type_id: MIRTypeId,
        template_info: Option<Box<TemplateInfo>>,
        attributes: MIRMoveAttributes,
    ) -> Self {
        MIRType {
            strong_identifier: Some(QualifiedName::new_raw(name)),
            debug_name: None,
            template_info,
            move_attributes: attributes,
            kind: MIRTypeKind::Structured { fields: vec![] },
            ..Default::default()
        }
    }

    pub fn named_union(name: CXIdent, _type_id: MIRTypeId) -> Self {
        MIRType {
            strong_identifier: Some(QualifiedName::new_raw(name)),
            debug_name: None,
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
            strong_identifier: Some(QualifiedName::new_raw(name)),
            debug_name: None,
            template_info,
            move_attributes: attributes,
            kind: MIRTypeKind::TaggedUnion { variants: vec![] },
            ..Default::default()
        }
    }

    pub fn ptr_inner(&self) -> Option<MIRTypeId> {
        match &self.kind {
            MIRTypeKind::PointerTo { inner_type, .. } => Some(*inner_type),

            _ => None,
        }
    }

    pub fn mem_ref_inner(&self) -> Option<MIRTypeId> {
        match &self.kind {
            MIRTypeKind::MemoryReference { inner_type, .. } => Some(*inner_type),

            _ => None,
        }
    }

    pub fn array_inner(&self) -> Option<MIRTypeId> {
        match &self.kind {
            MIRTypeKind::Array { inner_type, .. } => Some(*inner_type),

            _ => None,
        }
    }

    pub fn aggregate_fields(
        &self,
        definitions: &impl MIRTypeContext,
    ) -> Option<Vec<(String, MIRType)>> {
        let fields = match &self.kind {
            MIRTypeKind::Structured { fields, .. } => fields,
            MIRTypeKind::TaggedUnion { variants, .. } | MIRTypeKind::Union { variants, .. } => {
                variants
            }

            _ => return None,
        };

        Some(
            fields
                .iter()
                .map(|f| {
                    Some((
                        f.name()?.to_string(),
                        definitions.resolve_type_id(f.ty()).clone(),
                    ))
                })
                .collect::<Option<_>>()?,
        )
    }

    pub fn rewrite_named_type_metadata(
        &mut self,
        _target_id: MIRTypeId,
        new_name: &CXIdent,
        template_info: &Option<Box<TemplateInfo>>,
    ) {
        self.strong_identifier = Some(QualifiedName::new_raw(new_name.clone()));
        self.debug_name.get_or_insert_with(|| new_name.clone());
        self.template_info = template_info.clone();
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

impl MIRTypeKind {
    pub fn contextual_eq(&self, other: &Self, definitions: &MIRSymbolRegistry) -> bool {
        let mut state = TypeComparisonState::default();
        self.contextual_eq_with_state(other, definitions, &mut state)
    }

    pub(crate) fn contextual_eq_with_state(
        &self,
        other: &Self,
        definitions: &impl MIRTypeContext,
        state: &mut TypeComparisonState,
    ) -> bool {
        match (self, other) {
            (MIRTypeKind::Unit, MIRTypeKind::Unit)
            | (MIRTypeKind::Undefined, MIRTypeKind::Undefined)
            | (MIRTypeKind::Str, MIRTypeKind::Str) => true,
            (
                MIRTypeKind::Integer {
                    _type: left_type,
                    signed: left_signed,
                },
                MIRTypeKind::Integer {
                    _type: right_type,
                    signed: right_signed,
                },
            ) => left_type == right_type && left_signed == right_signed,
            (MIRTypeKind::Float { _type: left_type }, MIRTypeKind::Float { _type: right_type }) => {
                left_type == right_type
            }
            (
                MIRTypeKind::Structured { fields: left },
                MIRTypeKind::Structured { fields: right },
            )
            | (MIRTypeKind::Union { variants: left }, MIRTypeKind::Union { variants: right })
            | (
                MIRTypeKind::TaggedUnion { variants: left },
                MIRTypeKind::TaggedUnion { variants: right },
            ) => named_type_fields_contextual_eq(left, right, definitions, state),
            (
                MIRTypeKind::PointerTo { inner_type: left },
                MIRTypeKind::PointerTo { inner_type: right },
            ) => left.contextual_eq_with_state(right, definitions, state),
            (
                MIRTypeKind::MemoryReference {
                    inner_type: left,
                    bitfield: left_bitfield,
                },
                MIRTypeKind::MemoryReference {
                    inner_type: right,
                    bitfield: right_bitfield,
                },
            ) => {
                left_bitfield == right_bitfield
                    && left.contextual_eq_with_state(right, definitions, state)
            }
            (
                MIRTypeKind::Array {
                    length: left_len,
                    inner_type: left_inner,
                },
                MIRTypeKind::Array {
                    length: right_len,
                    inner_type: right_inner,
                },
            ) => {
                left_len == right_len
                    && left_inner.contextual_eq_with_state(right_inner, definitions, state)
            }
            (
                MIRTypeKind::Function { signature: left },
                MIRTypeKind::Function { signature: right },
            ) => left.contextual_eq_with_state(right, definitions, state),
            (MIRTypeKind::Opaque { size: left }, MIRTypeKind::Opaque { size: right }) => {
                left == right
            }
            _ => false,
        }
    }
}

fn named_type_fields_contextual_eq(
    left: &[MIRField],
    right: &[MIRField],
    definitions: &impl MIRTypeContext,
    state: &mut TypeComparisonState,
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right.iter())
            .all(|(left, right)| match (left, right) {
                (
                    MIRField::Standard {
                        name: left_name,
                        type_id: left_id,
                    },
                    MIRField::Standard {
                        name: right_name,
                        type_id: right_id,
                    },
                ) => {
                    left_name == right_name
                        && left_id.contextual_eq_with_state(right_id, definitions, state)
                }
                (
                    MIRField::Bitfield {
                        name: left_name,
                        integer_type_id: left_id,
                        width: left_width,
                    },
                    MIRField::Bitfield {
                        name: right_name,
                        integer_type_id: right_id,
                        width: right_width,
                    },
                ) => {
                    left_name == right_name
                        && left_width == right_width
                        && left_id.contextual_eq_with_state(right_id, definitions, state)
                }
                _ => false,
            })
}
