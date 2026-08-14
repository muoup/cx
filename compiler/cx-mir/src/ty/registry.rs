use std::collections::{HashMap, HashSet};

use cx_target::ArchitectureConfig;

use super::{
    MIRBitfieldAccess, MIRField, MIRFloatType, MIRFunctionType, MIRIntType, MIRLayoutError,
    MIRTypeDefinition, MIRTypeID, MIRTypeKind, MIRTypeLayout,
};

#[derive(Debug, Clone)]
pub struct MIRTypeRegistry {
    pub(super) architecture: ArchitectureConfig,
    pub(super) definitions: Vec<Option<MIRTypeDefinition>>,
    pub(super) layouts: Vec<Option<MIRTypeLayout>>,

    interner: HashMap<MIRTypeDefinition, MIRTypeID>,
    debug_names: Vec<Option<String>>,
    next_id: usize,
}

impl MIRTypeRegistry {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            definitions: Vec::new(),
            interner: HashMap::new(),
            debug_names: Vec::new(),
            layouts: Vec::new(),
            next_id: 0,
        }
    }

    pub fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    pub fn unit(&self) -> MIRTypeID {
        self.find(&MIRTypeDefinition::new(MIRTypeKind::Void))
            .expect("MIR unit type was not imported")
    }

    pub fn len(&self) -> usize {
        self.definitions.iter().flatten().count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn definition(&self, id: MIRTypeID) -> Option<&MIRTypeDefinition> {
        self.definitions.get(id.index()).and_then(Option::as_ref)
    }

    pub fn kind(&self, id: MIRTypeID) -> Option<&MIRTypeKind> {
        self.definition(id).map(|definition| &definition.kind)
    }

    pub fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(id.index()).and_then(Option::as_deref)
    }

    pub fn set_debug_name(&mut self, id: MIRTypeID, name: impl Into<String>) {
        self.ensure_capacity(id.index());
        self.debug_names[id.index()] = Some(name.into());
    }

    pub fn intern(&mut self, definition: MIRTypeDefinition) -> MIRTypeID {
        if let Some(id) = self.interner.get(&definition).copied() {
            return id;
        }

        let id = MIRTypeID::new(self.next_id);
        self.next_id += 1;
        self.ensure_capacity(id.index());
        self.definitions[id.index()] = Some(definition.clone());
        self.interner.insert(definition, id);
        id
    }

    pub fn reserve_id_space(&mut self, end: usize) {
        self.next_id = self.next_id.max(end);
        let end = end as usize;
        if self.definitions.len() < end {
            self.definitions.resize_with(end, || None);
            self.debug_names.resize(end, None);
            self.layouts.resize(end, None);
        }
    }

    pub fn find(&self, definition: &MIRTypeDefinition) -> Option<MIRTypeID> {
        self.interner.get(definition).copied()
    }

    pub fn define(
        &mut self,
        id: MIRTypeID,
        definition: MIRTypeDefinition,
    ) -> Result<(), MIRLayoutError> {
        self.ensure_capacity(id.index());
        self.next_id = self.next_id.max(id.index() + 1);
        let slot = &mut self.definitions[id.index()];
        if slot.is_some() {
            return Err(MIRLayoutError::DuplicateType(id));
        }
        *slot = Some(definition.clone());
        self.layouts[id.index()] = None;
        self.interner.entry(definition).or_insert(id);
        Ok(())
    }

    fn ensure_capacity(&mut self, index: usize) {
        if self.definitions.len() <= index {
            let len = index + 1;
            self.definitions.resize_with(len, || None);
            self.debug_names.resize(len, None);
            self.layouts.resize(len, None);
        }
    }

    pub fn integer_type(&self, ty: MIRIntType, signed: bool) -> Option<MIRTypeID> {
        self.find(&MIRTypeDefinition::new(MIRTypeKind::Integer { ty, signed }))
    }

    pub fn float_type(&self, ty: MIRFloatType) -> Option<MIRTypeID> {
        self.find(&MIRTypeDefinition::new(MIRTypeKind::Float { ty }))
    }

    pub fn bool_type(&self) -> Option<MIRTypeID> {
        self.integer_type(MIRIntType::I1, false)
    }

    pub fn pointer_integer_type(&self) -> MIRIntType {
        MIRIntType::from_bytes(self.architecture.pointer_size() as u8)
            .expect("ArchitectureConfig guarantees a supported pointer size")
    }

    pub fn same_type(&self, left: MIRTypeID, right: MIRTypeID) -> bool {
        if left == right {
            return true;
        }

        self.same_type_inner(left, right, &mut HashSet::new())
    }

    fn same_type_inner(
        &self,
        left: MIRTypeID,
        right: MIRTypeID,
        compared: &mut HashSet<(MIRTypeID, MIRTypeID)>,
    ) -> bool {
        if !compared.insert((left, right)) {
            return true;
        }

        let (Some(left), Some(right)) = (self.definition(left), self.definition(right)) else {
            return false;
        };
        left.minimum_alignment == right.minimum_alignment
            && same_kind(&left.kind, &right.kind, |left, right| {
                self.same_type_inner(left, right, compared)
            })
    }
}

impl Default for MIRTypeRegistry {
    fn default() -> Self {
        Self::new(ArchitectureConfig::default())
    }
}

fn same_kind(
    left: &MIRTypeKind,
    right: &MIRTypeKind,
    mut same_id: impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    match (left, right) {
        (MIRTypeKind::Void, MIRTypeKind::Void)
        | (MIRTypeKind::Undefined, MIRTypeKind::Undefined)
        | (MIRTypeKind::Str, MIRTypeKind::Str) => true,
        (
            MIRTypeKind::Integer {
                ty: left_ty,
                signed: left_signed,
            },
            MIRTypeKind::Integer {
                ty: right_ty,
                signed: right_signed,
            },
        ) => left_ty == right_ty && left_signed == right_signed,
        (MIRTypeKind::Float { ty: left }, MIRTypeKind::Float { ty: right }) => left == right,
        (MIRTypeKind::Structured { fields: left }, MIRTypeKind::Structured { fields: right })
        | (MIRTypeKind::Union { variants: left }, MIRTypeKind::Union { variants: right })
        | (
            MIRTypeKind::TaggedUnion { variants: left },
            MIRTypeKind::TaggedUnion { variants: right },
        ) => same_fields(left, right, &mut same_id),
        (MIRTypeKind::PointerTo { inner: left }, MIRTypeKind::PointerTo { inner: right }) => {
            same_id(*left, *right)
        }
        (
            MIRTypeKind::MemoryReference {
                inner: left_inner,
                bitfield: left_bitfield,
            },
            MIRTypeKind::MemoryReference {
                inner: right_inner,
                bitfield: right_bitfield,
            },
        ) => {
            same_id(*left_inner, *right_inner)
                && same_bitfield(
                    left_bitfield.as_ref(),
                    right_bitfield.as_ref(),
                    &mut same_id,
                )
        }
        (
            MIRTypeKind::Array {
                length: left_length,
                inner: left_inner,
            },
            MIRTypeKind::Array {
                length: right_length,
                inner: right_inner,
            },
        ) => left_length == right_length && same_id(*left_inner, *right_inner),
        (MIRTypeKind::Function { signature: left }, MIRTypeKind::Function { signature: right }) => {
            same_function_type(left, right, &mut same_id)
        }
        (
            MIRTypeKind::Opaque {
                size: left_size,
                alignment: left_alignment,
            },
            MIRTypeKind::Opaque {
                size: right_size,
                alignment: right_alignment,
            },
        ) => left_size == right_size && left_alignment == right_alignment,
        _ => false,
    }
}

fn same_bitfield(
    left: Option<&MIRBitfieldAccess>,
    right: Option<&MIRBitfieldAccess>,
    same_id: &mut impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    match (left, right) {
        (None, None) => true,
        (Some(left), Some(right)) => {
            same_id(left.storage_type, right.storage_type)
                && left.bit_offset == right.bit_offset
                && left.bit_width == right.bit_width
                && left.signed == right.signed
        }
        _ => false,
    }
}

fn same_function_type(
    left: &MIRFunctionType,
    right: &MIRFunctionType,
    same_id: &mut impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    left.variadic == right.variadic
        && same_id(left.return_type, right.return_type)
        && left.params.len() == right.params.len()
        && left
            .params
            .iter()
            .zip(&right.params)
            .all(|(left, right)| same_id(*left, *right))
}

fn same_fields(
    left: &[MIRField],
    right: &[MIRField],
    same_id: &mut impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| match (left, right) {
                (
                    MIRField::Standard {
                        name: left_name,
                        type_id: left,
                    },
                    MIRField::Standard {
                        name: right_name,
                        type_id: right,
                    },
                ) => left_name == right_name && same_id(*left, *right),
                (
                    MIRField::Bitfield {
                        name: left_name,
                        integer_type_id: left_type,
                        width: left_width,
                    },
                    MIRField::Bitfield {
                        name: right_name,
                        integer_type_id: right_type,
                        width: right_width,
                    },
                ) => {
                    left_name == right_name
                        && left_width == right_width
                        && same_id(*left_type, *right_type)
                }
                _ => false,
            })
}
