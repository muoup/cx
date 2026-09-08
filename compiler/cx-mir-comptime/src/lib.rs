mod engine;
mod interpretable;
pub mod log;
mod value;

pub mod context;

pub use context::ComptimeResolver;
pub use engine::{EngineLimits, MIRComptimeEngine};
pub use interpretable::{ComptimeInterpretable, InterpretedFunction};
pub use value::{MIRComptimeValue, MIRStagedBinding, MIRStagedValue};

use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRUnit};

pub fn evaluate_unit_globals(unit: &MIRUnit) -> CXResult<Vec<(cx_mir::MIRGlobalID, MIRConstant)>> {
    use cx_mir::{MIRGlobalKind, MIRGlobalState};

    let mut engine = MIRComptimeEngine::new(unit);
    let mut evaluated = Vec::new();

    for global in unit.globals_in_order() {
        let MIRGlobalKind::Variable { state, .. } = &global.kind else {
            continue;
        };
        let MIRGlobalState::Initializer(function_id) = state else {
            continue;
        };
        let Some(function) = unit.function(*function_id) else {
            continue;
        };
        let Some(entry) = InterpretedFunction::new(function) else {
            continue;
        };

        let constant = engine.run(entry, &[])?;
        evaluated.push((global.id, constant));
    }

    Ok(evaluated)
}
