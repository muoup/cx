mod engine;
mod interpretable;
pub mod log;
mod value;

pub mod context;

pub use context::ComptimeContext;
pub use engine::{EngineLimits, MIRComptimeEngine};
pub use interpretable::{ComptimeInterpretable, InterpretedFunction};
pub use value::{MIRComptimeValue, MIRStagedBinding, MIRStagedValue};

use cx_log::CXResult;
use cx_mir::MIRFunction;

pub fn evaluate_comptime_function(
    context: &impl ComptimeContext,
    func: &MIRFunction,
    args: &[MIRComptimeValue],
) -> CXResult<MIRComptimeValue> {
    let entry = InterpretedFunction::new(func);
    let mut engine = MIRComptimeEngine::new(context);

    engine.run(entry, args)
}
