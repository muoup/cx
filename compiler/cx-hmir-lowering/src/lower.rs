pub(crate) mod constructor;
pub(crate) mod function;
pub(crate) mod global;
pub(crate) mod mir;
pub(crate) mod types;

use crate::{
    env::{
        HMIREnvironment,
        generation::{Generation, Specialization},
    },
    symbol,
};
use cx_hmir::{HMIRDefID, HMIRDefKind, HMIRFunctionStage, HMIRUnit};
use cx_log::CXResult;

pub fn lower_unit<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let mut defs = unit.defs().map(|(id, _)| *id).collect::<Vec<_>>();
    defs.sort();

    for id in defs {
        symbol::validate(env, id)?;
    }

    while let Some(id) = env.generation_mut().next() {
        match id {
            Generation::Definition(id) => generate(id, env, None)?,
            Generation::Specialization(specialization) => {
                generate(specialization.def(), env, Some(&specialization))?
            }
            Generation::Constructor(constructor) => {
                let function = constructor::lower(env, &constructor)?;
                env.module_mut()
                    .define_function(constructor.function(), function);
            }
        }
    }

    Ok(())
}

fn generate(
    id: HMIRDefID,
    env: &mut HMIREnvironment,
    specialization: Option<&Specialization>,
) -> CXResult<()> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("symbol owner is available");
    let source = unit.resolve_def(id).expect("symbol exists");
    let selected = symbol::select(source, None)?;
    let kind = source.declarations()[selected].kind();

    match kind {
        Some(HMIRDefKind::Function(function)) => {
            if let Some(specialization) = specialization {
                return function::lower(env, id, function, specialization.signature(), Some(specialization));
            }
            if function.signature().stage() != HMIRFunctionStage::Runtime
                || symbol::needs_specialization(&function)
                || function.def().is_none()
            {
                return Ok(());
            }
            let signature = symbol::signature(env, id, None)?;
            function::lower(env, id, function, &signature, None)
        }
        Some(HMIRDefKind::Global(_)) => global::lower(env, id),
        Some(HMIRDefKind::ComptimeGlobal(_)) | None => Ok(()),
    }
}
