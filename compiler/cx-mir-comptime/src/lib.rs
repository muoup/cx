use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRFunction, MIRTarget, MIRValue};

// TODO: The previous comptime interpreter still targets the removed MIR instruction hierarchy.
/*
mod engine;
mod interpretable;
pub mod log;
mod value;
pub mod context;
*/

#[derive(Debug, Clone)]
pub enum MIRComptimeValue {
    Reference { frame: usize, target: MIRTarget },
    Constant(MIRConstant),
    Staged(MIRStagedValue),
}

impl MIRComptimeValue {
    pub fn constant(self) -> Option<MIRConstant> {
        match self {
            Self::Constant(value) => Some(value),
            Self::Reference { .. } | Self::Staged(_) => None,
        }
    }
}

impl From<MIRConstant> for MIRComptimeValue {
    fn from(value: MIRConstant) -> Self {
        Self::Constant(value)
    }
}

#[derive(Debug, Clone)]
pub enum MIRStagedBinding {
    Value(MIRValue),
    Comptime(MIRComptimeValue),
}

#[derive(Debug, Clone, Default)]
pub struct MIRStagedValue;

#[derive(Debug, Clone, Copy)]
pub struct EngineLimits {
    pub max_steps: u64,
    pub max_call_depth: usize,
}

impl Default for EngineLimits {
    fn default() -> Self {
        Self {
            max_steps: 1_000_000,
            max_call_depth: 128,
        }
    }
}

pub struct MIRComptimeEngine;

pub fn evaluate_comptime_function<C>(
    _context: &C,
    _func: &MIRFunction,
    _args: &[MIRComptimeValue],
) -> CXResult<MIRComptimeValue> {
    todo!("comptime evaluation was removed with the MIR representation migration")
}
