mod execution;
mod memory;
mod ops;
mod state;

use std::collections::{HashMap, HashSet};

use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRGlobalID};

use crate::{
    context::ComptimeResolver, interpretable::InterpretedFunction, value::MIRComptimeValue,
};

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

pub struct MIRComptimeEngine<'ctx> {
    resolver: &'ctx dyn ComptimeResolver,
    limits: EngineLimits,
    frames: Vec<Frame<'ctx>>,
    globals: HashMap<MIRGlobalID, MIRConstant>,
    evaluating_globals: HashSet<MIRGlobalID>,
    steps: u64,
}

impl<'ctx> MIRComptimeEngine<'ctx> {
    pub fn new(resolver: &'ctx dyn ComptimeResolver) -> Self {
        Self::with_limits(resolver, EngineLimits::default())
    }

    pub fn with_limits(resolver: &'ctx dyn ComptimeResolver, limits: EngineLimits) -> Self {
        Self {
            resolver,
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
        args: &[MIRConstant],
    ) -> CXResult<MIRConstant> {
        let args = args
            .iter()
            .cloned()
            .map(MIRComptimeValue::Constant)
            .collect::<Vec<_>>();
        self.run_values(entry, &args)?.constant().ok_or_else(|| {
            cx_log::error::CXErr::new(
                cx_log::error::message::CXStdErrMessage::error(
                    "COMPTIME ERROR",
                    "expected a concrete compile-time value",
                ),
                cx_log::error::context::CXInternalContext::error(
                    "staged value escaped a constant evaluation",
                ),
            )
        })
    }

    pub fn run_values(
        &mut self,
        entry: InterpretedFunction<'ctx>,
        args: &[MIRComptimeValue],
    ) -> CXResult<MIRComptimeValue> {
        execution::run(self, entry, args)
    }
}
