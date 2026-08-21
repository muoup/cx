use cx_mir::{MIRGlobalKind, MIRGlobalState, MIRUnit};

use crate::{MIRComptimeEngine, MIRComptimeError};

pub fn materialize_globals(unit: &mut MIRUnit) -> Result<(), MIRComptimeError> {
    let global_order = unit.global_order().to_owned();
    for global_id in global_order {
        let Some(function) = unit.global(global_id).and_then(|global| {
            let MIRGlobalKind::Variable { state, .. } = &global.kind else {
                return None;
            };
            match state {
                MIRGlobalState::Initializer(function) => Some(*function),
                _ => None,
            }
        }) else {
            continue;
        };

        let value = MIRComptimeEngine::new(&*unit).evaluate(function, &[])?;
        unit.materialize_global(global_id, value)
            .map_err(|message| {
                MIRComptimeError::new(
                    message,
                    cx_mir::MIRDiagnosticLocation::Internal(format!("global {global_id}")),
                )
            })?;
    }
    Ok(())
}
