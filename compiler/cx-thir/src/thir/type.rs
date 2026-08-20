use cx_hir::ast::modifiers::{HIRTypeQualifiers, VisibilityMode};
use cx_util::{dense_id, identifier::CXIdent, namespace::QualifiedName};
use speedy::{Readable, Writable};

use crate::{
    thir::contextual_eq::{TypeComparisonState, TypeContextEqual},
    thir::data::{THIRFnSignature, TemplateInfo},
    type_context::THIRTypeContext,
};

dense_id!(THIRTypeID);

#[derive(Debug, Clone)]
pub struct THIRType {
    pub visibility: VisibilityMode,
    pub specifiers: HIRTypeQualifiers,

    pub attributes: THIRTypeAttributes,

    pub strong_identifier: Option<String>,
    pub lookup_identifier: Option<QualifiedName>,
    pub template_info: Option<Box<TemplateInfo>>,

    pub kind: THIRTypeKind,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub struct THIRTypeAttributes {
    pub semantics: THIRMoveSemantics,
    pub minimum_alignment: Option<usize>,
    pub unsafe_move: bool,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub enum THIRMoveSemantics {
    #[default]
    POD,
    Nocopy,
    Nodrop,
}

impl THIRMoveSemantics {
    pub fn is_nodrop(&self) -> bool {
        matches!(self, THIRMoveSemantics::Nodrop)
    }

    pub fn is_nocopy(&self) -> bool {
        self.is_nodrop() || matches!(self, THIRMoveSemantics::Nocopy)
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum THIRField {
    Standard {
        name: String,
        type_id: THIRTypeID,
    },
    Bitfield {
        name: Option<String>,
        integer_type_id: THIRTypeID,
        width: usize,
    },
}

impl THIRField {
    pub fn standard(name: String, type_id: THIRTypeID) -> Self {
        Self::Standard { name, type_id }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            THIRField::Standard { name, .. } => Some(name.as_str()),
            THIRField::Bitfield { name, .. } => name.as_deref(),
        }
    }

    pub fn ty(&self) -> THIRTypeID {
        match self {
            THIRField::Standard { type_id, .. } => *type_id,
            THIRField::Bitfield {
                integer_type_id, ..
            } => *integer_type_id,
        }
    }

    pub fn standard_parts(&self) -> Option<(&String, THIRTypeID)> {
        match self {
            THIRField::Standard { name, type_id } => Some((name, *type_id)),
            THIRField::Bitfield { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub struct THIRBitfieldAccess {
    pub storage_type: THIRTypeID,
    pub bit_offset: usize,
    pub bit_width: usize,
    pub signed: bool,
}

#[derive(Debug, Clone)]
pub enum THIRTypeKind {
    Void,
    Unreachable,
    Integer {
        _type: THIRIntType,
        signed: bool,
    },
    Float {
        _type: THIRFloatType,
    },
    Structured {
        fields: Vec<THIRField>,
    },
    Union {
        variants: Vec<THIRField>,
    },
    TaggedUnion {
        variants: Vec<THIRField>,
    },
    PointerTo {
        inner_type: THIRTypeID,
    },
    MemoryReference {
        inner_type: THIRTypeID,
        bitfield: Option<THIRBitfieldAccess>,
    },
    Array {
        length: usize,
        inner_type: THIRTypeID,
    },
    Function {
        signature: Box<THIRFnSignature>,
    },
    Opaque {
        size: usize,
        alignment: usize,
    },
    Undefined,
    Str,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Readable, Writable)]
pub enum THIRIntType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Readable, Writable)]
pub enum THIRFloatType {
    F32,
    F64,
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRTypeID {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        if self == other {
            return true;
        }

        if !state.compare_type_ids_once(*self, *other) {
            return true;
        }

        let left = definitions.resolve_type_id(*self);
        let right = definitions.resolve_type_id(*other);

        left.compare(right, definitions, state)
    }
}

impl THIRIntType {
    pub const fn rank(&self) -> u8 {
        match self {
            THIRIntType::I1 => 0,
            THIRIntType::I8 => 1,
            THIRIntType::I16 => 2,
            THIRIntType::I32 => 3,
            THIRIntType::I64 => 4,
            THIRIntType::I128 => 5,
        }
    }

    pub const fn bytes(&self) -> usize {
        match self {
            THIRIntType::I1 => 1,
            THIRIntType::I8 => 1,
            THIRIntType::I16 => 2,
            THIRIntType::I32 => 4,
            THIRIntType::I64 => 8,
            THIRIntType::I128 => 16,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            1 => Some(THIRIntType::I8),
            2 => Some(THIRIntType::I16),
            4 => Some(THIRIntType::I32),
            8 => Some(THIRIntType::I64),
            16 => Some(THIRIntType::I128),
            _ => None,
        }
    }
}

impl THIRFloatType {
    pub const fn bytes(&self) -> usize {
        match self {
            THIRFloatType::F32 => 4,
            THIRFloatType::F64 => 8,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            4 => Some(THIRFloatType::F32),
            8 => Some(THIRFloatType::F64),
            _ => None,
        }
    }
}

impl Default for THIRType {
    fn default() -> Self {
        THIRType {
            visibility: VisibilityMode::Private,
            specifiers: HIRTypeQualifiers::default(),
            attributes: THIRTypeAttributes::default(),
            strong_identifier: None,
            lookup_identifier: None,
            template_info: None,
            kind: THIRTypeKind::Void,
        }
    }
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRType {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        if self.specifiers != other.specifiers || self.attributes != other.attributes {
            return false;
        }

        match (&self.strong_identifier, &other.strong_identifier) {
            (Some(left), Some(right)) => {
                return left == right
                    && match (&self.template_info, &other.template_info) {
                        (Some(left), Some(right)) => {
                            left.template_input
                                .compare(&right.template_input, definitions, state)
                        }
                        (None, None) => true,
                        (Some(_), None) | (None, Some(_)) => false,
                    };
            }
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }

        self.kind.compare(&other.kind, definitions, state)
    }
}

impl THIRType {
    pub fn unit() -> Self {
        Self::default()
    }

    pub fn bool() -> Self {
        THIRType {
            kind: THIRTypeKind::Integer {
                _type: THIRIntType::I1,
                signed: false,
            },
            ..Default::default()
        }
    }

    pub fn internal_function() -> Self {
        THIRType::from(THIRTypeKind::Function {
            signature: Box::new(THIRFnSignature::default()),
        })
        .with_strong_identifier(CXIdent::from("__internal_function"))
    }

    pub fn with_strong_identifier(mut self, name: CXIdent) -> THIRType {
        self.strong_identifier = Some(name.as_string());
        self
    }

    pub fn set_visibility_mode(&mut self, visibility: VisibilityMode) -> &mut Self {
        self.visibility = visibility;
        self
    }

    pub fn add_specifier(mut self, specifier: HIRTypeQualifiers) -> Self {
        self.specifiers |= specifier;
        self
    }

    pub fn with_specifier(&self, specifier: HIRTypeQualifiers) -> Self {
        self.clone().add_specifier(specifier)
    }

    pub fn remove_specifier(&mut self, specifier: HIRTypeQualifiers) -> &mut Self {
        self.specifiers &= !specifier;
        self
    }

    pub fn without_specifiers(mut self) -> Self {
        self.specifiers = 0;
        self
    }

    pub fn without_specifier(&self, specifier: HIRTypeQualifiers) -> Self {
        let mut clone = self.clone();
        clone.remove_specifier(specifier);
        clone
    }

    pub fn get_specifier(&self, specifier: HIRTypeQualifiers) -> bool {
        self.specifiers & specifier == specifier
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, THIRTypeKind::PointerTo { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Array { .. })
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Opaque { .. })
    }

    pub fn is_tagged_union(&self) -> bool {
        matches!(self.kind, THIRTypeKind::TaggedUnion { .. })
    }

    pub fn is_c_union(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Union { .. })
    }

    pub fn is_union(&self) -> bool {
        self.is_tagged_union() || self.is_c_union()
    }

    pub fn is_str(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Str)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Function { .. })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Integer { .. })
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Float { .. })
    }

    pub fn is_void(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Void)
    }

    pub fn is_unreachable(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Unreachable)
    }

    pub fn is_structure(&self) -> bool {
        matches!(self.kind, THIRTypeKind::Structured { .. })
    }

    pub fn is_nodrop(&self) -> bool {
        self.struct_attributes()
            .map(|a| a.semantics.is_nodrop())
            .unwrap_or(false)
    }

    pub fn is_nocopy(&self) -> bool {
        self.struct_attributes()
            .map(|a| a.semantics.is_nocopy())
            .unwrap_or(false)
    }

    pub fn is_unsafe_move(&self) -> bool {
        self.struct_attributes()
            .map(|a| a.unsafe_move)
            .unwrap_or(false)
    }

    pub fn struct_attributes(&self) -> Option<THIRTypeAttributes> {
        match self.kind {
            THIRTypeKind::Structured { .. }
            | THIRTypeKind::Union { .. }
            | THIRTypeKind::TaggedUnion { .. } => Some(self.attributes),
            _ => None,
        }
    }

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, THIRTypeKind::MemoryReference { .. })
    }

    pub fn strong_identifier(&self) -> Option<&str> {
        self.strong_identifier.as_deref()
    }

    pub fn lookup_identifier(&self) -> Option<&QualifiedName> {
        self.lookup_identifier.as_ref()
    }

    pub fn member_lookup_identifier(&self) -> Option<&QualifiedName> {
        self.lookup_identifier.as_ref().or_else(|| {
            self.template_info
                .as_ref()
                .and_then(|info| info.base_name.as_ref())
        })
    }

    pub fn get_base_identifier(&self) -> Option<&QualifiedName> {
        self.member_lookup_identifier()
    }

    pub fn get_template_data(&self) -> Option<&TemplateInfo> {
        self.template_info.as_deref()
    }

    pub fn function_signature(&self) -> Option<&THIRFnSignature> {
        match &self.kind {
            THIRTypeKind::Function { signature } => Some(signature),
            _ => None,
        }
    }

    pub fn was_template_instantiated(&self) -> bool {
        self.template_info.is_some()
    }

    pub fn set_name(&mut self, new_name: CXIdent) {
        self.strong_identifier = Some(new_name.as_string());
    }

    pub fn set_qualified_name(&mut self, new_name: QualifiedName) {
        self.lookup_identifier = Some(new_name.clone());
        self.strong_identifier = Some(new_name.as_flat_name());
    }

    pub fn named_struct(
        name: CXIdent,
        _type_id: THIRTypeID,
        template_info: Option<Box<TemplateInfo>>,
        attributes: THIRTypeAttributes,
    ) -> Self {
        THIRType {
            strong_identifier: Some(name.as_string()),
            template_info,
            attributes,
            kind: THIRTypeKind::Structured { fields: vec![] },
            ..Default::default()
        }
    }

    pub fn named_union(name: CXIdent, _type_id: THIRTypeID) -> Self {
        THIRType {
            strong_identifier: Some(name.as_string()),
            kind: THIRTypeKind::Union { variants: vec![] },
            ..Default::default()
        }
    }

    pub fn named_tagged_union(
        name: CXIdent,
        _type_id: THIRTypeID,
        template_info: Option<Box<TemplateInfo>>,
        attributes: THIRTypeAttributes,
    ) -> Self {
        THIRType {
            strong_identifier: Some(name.as_string()),
            template_info,
            attributes,
            kind: THIRTypeKind::TaggedUnion { variants: vec![] },
            ..Default::default()
        }
    }

    pub fn ptr_inner(&self) -> Option<THIRTypeID> {
        match &self.kind {
            THIRTypeKind::PointerTo { inner_type, .. } => Some(*inner_type),

            _ => None,
        }
    }

    pub fn mem_ref_inner(&self) -> Option<THIRTypeID> {
        match &self.kind {
            THIRTypeKind::MemoryReference { inner_type, .. } => Some(*inner_type),

            _ => None,
        }
    }

    pub fn array_inner(&self) -> Option<THIRTypeID> {
        match &self.kind {
            THIRTypeKind::Array { inner_type, .. } => Some(*inner_type),

            _ => None,
        }
    }

    pub fn aggregate_fields(
        &self,
        definitions: &impl THIRTypeContext,
    ) -> Option<Vec<(String, THIRType)>> {
        let fields = match &self.kind {
            THIRTypeKind::Structured { fields, .. } => fields,
            THIRTypeKind::TaggedUnion { variants, .. } | THIRTypeKind::Union { variants, .. } => {
                variants
            }

            _ => return None,
        };

        fields
            .iter()
            .map(|f| {
                Some((
                    f.name()?.to_string(),
                    definitions.resolve_type_id(f.ty()).clone(),
                ))
            })
            .collect::<Option<_>>()
    }

    pub fn rewrite_named_type_metadata(
        &mut self,
        _target_id: THIRTypeID,
        new_name: &CXIdent,
        template_info: &Option<Box<TemplateInfo>>,
    ) {
        self.strong_identifier = Some(new_name.as_string());
        self.template_info = template_info.clone();
    }
}

impl From<THIRTypeKind> for THIRType {
    fn from(kind: THIRTypeKind) -> Self {
        THIRType {
            kind,
            ..Default::default()
        }
    }
}

impl<Context: THIRTypeContext + ?Sized> TypeContextEqual<Context> for THIRTypeKind {
    fn compare(
        &self,
        other: &Self,
        definitions: &Context,
        state: &mut TypeComparisonState,
    ) -> bool {
        match (self, other) {
            (THIRTypeKind::Void, THIRTypeKind::Void)
            | (THIRTypeKind::Unreachable, THIRTypeKind::Unreachable)
            | (THIRTypeKind::Undefined, THIRTypeKind::Undefined)
            | (THIRTypeKind::Str, THIRTypeKind::Str) => true,
            (
                THIRTypeKind::Integer {
                    _type: left_type,
                    signed: left_signed,
                },
                THIRTypeKind::Integer {
                    _type: right_type,
                    signed: right_signed,
                },
            ) => left_type == right_type && left_signed == right_signed,
            (
                THIRTypeKind::Float { _type: left_type },
                THIRTypeKind::Float { _type: right_type },
            ) => left_type == right_type,
            (
                THIRTypeKind::Structured { fields: left },
                THIRTypeKind::Structured { fields: right },
            )
            | (THIRTypeKind::Union { variants: left }, THIRTypeKind::Union { variants: right })
            | (
                THIRTypeKind::TaggedUnion { variants: left },
                THIRTypeKind::TaggedUnion { variants: right },
            ) => compare_named_type_fields(left, right, definitions, state),
            (
                THIRTypeKind::PointerTo { inner_type: left },
                THIRTypeKind::PointerTo { inner_type: right },
            ) => left.compare(right, definitions, state),
            (
                THIRTypeKind::MemoryReference {
                    inner_type: left,
                    bitfield: left_bitfield,
                },
                THIRTypeKind::MemoryReference {
                    inner_type: right,
                    bitfield: right_bitfield,
                },
            ) => left_bitfield == right_bitfield && left.compare(right, definitions, state),
            (
                THIRTypeKind::Array {
                    length: left_len,
                    inner_type: left_inner,
                },
                THIRTypeKind::Array {
                    length: right_len,
                    inner_type: right_inner,
                },
            ) => left_len == right_len && left_inner.compare(right_inner, definitions, state),
            (
                THIRTypeKind::Function { signature: left },
                THIRTypeKind::Function { signature: right },
            ) => left.compare(right, definitions, state),
            (
                THIRTypeKind::Opaque {
                    size: left_size,
                    alignment: left_alignment,
                },
                THIRTypeKind::Opaque {
                    size: right_size,
                    alignment: right_alignment,
                },
            ) => left_size == right_size && left_alignment == right_alignment,
            _ => false,
        }
    }
}

fn compare_named_type_fields<Context: THIRTypeContext + ?Sized>(
    left: &[THIRField],
    right: &[THIRField],
    definitions: &Context,
    state: &mut TypeComparisonState,
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right.iter())
            .all(|(left, right)| match (left, right) {
                (
                    THIRField::Standard {
                        name: left_name,
                        type_id: left_id,
                    },
                    THIRField::Standard {
                        name: right_name,
                        type_id: right_id,
                    },
                ) => left_name == right_name && left_id.compare(right_id, definitions, state),
                (
                    THIRField::Bitfield {
                        name: left_name,
                        integer_type_id: left_id,
                        width: left_width,
                    },
                    THIRField::Bitfield {
                        name: right_name,
                        integer_type_id: right_id,
                        width: right_width,
                    },
                ) => {
                    left_name == right_name
                        && left_width == right_width
                        && left_id.compare(right_id, definitions, state)
                }
                _ => false,
            })
}
