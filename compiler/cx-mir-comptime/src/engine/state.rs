use std::collections::HashMap;

use cx_mir::{MIRRegister, MIRTarget};

use crate::{interpretable::InterpretedFunction, value::MIRComptimeValue};

#[derive(Debug, Clone)]
pub(super) enum PathSeg {
    Field(usize),
    Index(i128),
    Variant(usize),
}

impl PathSeg {
    pub(super) fn key(&self) -> usize {
        match self {
            Self::Field(key) | Self::Variant(key) => *key,
            Self::Index(index) => *index as usize,
        }
    }
}

pub(super) struct Frame<'ctx> {
    pub(super) id: usize,
    pub(super) code: InterpretedFunction<'ctx>,
    pub(super) registers: HashMap<MIRRegister, MIRComptimeValue>,
    pub(super) cells: HashMap<MIRTarget, MIRComptimeValue>,
    pub(super) derived: HashMap<MIRTarget, (MIRTarget, Vec<PathSeg>)>,
}

impl<'ctx> Frame<'ctx> {
    pub(super) fn new(code: InterpretedFunction<'ctx>, id: usize) -> Self {
        Self {
            id,
            code,
            registers: HashMap::new(),
            cells: HashMap::new(),
            derived: HashMap::new(),
        }
    }
}
