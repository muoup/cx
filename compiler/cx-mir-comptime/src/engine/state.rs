use std::collections::HashMap;

use cx_mir::{MIRPlace, MIRRegister};

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
    pub(super) code: InterpretedFunction<'ctx>,
    pub(super) registers: HashMap<MIRRegister, MIRComptimeValue>,
    pub(super) cells: HashMap<MIRPlace, MIRComptimeValue>,
    pub(super) derived: HashMap<MIRPlace, (MIRPlace, Vec<PathSeg>)>,
}

impl<'ctx> Frame<'ctx> {
    pub(super) fn new(code: InterpretedFunction<'ctx>) -> Self {
        Self {
            code,
            registers: HashMap::new(),
            cells: HashMap::new(),
            derived: HashMap::new(),
        }
    }
}
