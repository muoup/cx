mod execution;
mod memory;
mod ops;
mod state;

use std::collections::{HashMap, HashSet};

use crate::{
    context::ComptimeContext, interpretable::InterpretedFunction, value::MIRComptimeValue,
};
use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRGlobalID};

use self::state::Frame;

const DEFAULT_STEP_BUDGET: u64 = 1_000_000;
const DEFAULT_MAX_DEPTH: usize = 128;

#[derive(Debug, Clone, Copy)]
pub struct EngineLimits {
    pub max_steps: u64,
    pub max_call_depth: usize,
}

impl Default for EngineLimits {
    fn default() -> Self {
        Self {
            max_steps: DEFAULT_STEP_BUDGET,
            max_call_depth: DEFAULT_MAX_DEPTH,
        }
    }
}

pub struct MIRComptimeEngine<'ctx, Context: ComptimeContext> {
    context: &'ctx Context,

    limits: EngineLimits,
    frames: Vec<Frame<'ctx>>,
    globals: HashMap<MIRGlobalID, MIRConstant>,
    evaluating_globals: HashSet<MIRGlobalID>,
    steps: u64,
}

impl<'ctx, Context: ComptimeContext> MIRComptimeEngine<'ctx, Context> {
    pub fn new(resolver: &'ctx Context) -> Self {
        Self::with_limits(resolver, EngineLimits::default())
    }

    pub fn with_limits(context: &'ctx Context, limits: EngineLimits) -> Self {
        Self {
            context,
            limits,

            frames: Vec::new(),
            globals: HashMap::new(),
            evaluating_globals: HashSet::new(),
            steps: 0,
        }
    }

    pub fn run(
        &mut self,
        entry: InterpretedFunction<'ctx>,
        args: &[MIRComptimeValue],
    ) -> CXResult<MIRComptimeValue> {
        execution::run(self, entry, args)
    }
}
