use crate::ty::{MIRField, interface::MTRegistry};

use super::MIRTypeID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MIRTypeLayout {
    size: usize,
    alignment: usize,
}

impl MIRTypeLayout {
    pub fn new(size: usize, alignment: usize) -> Self {
        Self { size, alignment }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn alignment(&self) -> usize {
        self.alignment
    }

    pub fn apply_minimum(self, minimum: MIRTypeLayout) -> MIRTypeLayout {
        MIRTypeLayout {
            size: self.size.max(minimum.size),
            alignment: self.size.max(minimum.alignment),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRFieldLayout {
    Standard {
        offset: usize,
        ty: MIRTypeID,
    },
    Bitfield {
        offset: usize,
        bit_offset: usize,
        bit_width: usize,
        storage_type: MIRTypeID,
    },
}

// TODO: Reimplement layout calculation against the current MIR type registry.
pub fn calculate_type_layout<Registry: MTRegistry>(
    _registry: &Registry,
    _ty: MIRTypeID,
) -> MIRTypeLayout {
    todo!()
}

pub fn calculate_field_layout<Registry: MTRegistry>(
    _registry: &Registry,
    _ty: &MIRField,
) -> MIRTypeLayout {
    todo!()
}
