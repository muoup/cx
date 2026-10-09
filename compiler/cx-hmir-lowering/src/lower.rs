pub(crate) mod context;

use cx_hmir::{
    HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRUnit,
    unit::function::HMIRFnDefinition,
};
use cx_log::CXResult;
use crate::env::HMIREnvironment;


pub fn lower_unit(unit: &HMIRUnit, env: &mut HMIREnvironment) -> CXResult<()> {
    for (_, def) in unit.defs() {
        match def.kind() {
            HMIRDefKind::Function(func) => {
                let Some(def) = func.def() else {
                    continue;
                };

                if func.signature().stage() == HMIRFunctionStage::Comptime
                    || func
                        .signature()
                        .params()
                        .iter()
                        .any(|param| param.comptime())
                {
                    continue;
                }

                lower_function(func, def, env)?;
            }

            HMIRDefKind::Global(global) => {
                lower_global(global, env)?;
            }

            _ => {}
        }
    }

    Ok(())
}

pub fn lower_function(
    function: &HMIRFunction,
    def: &HMIRFnDefinition,
    env: &mut HMIREnvironment,
) -> CXResult<()> {
    todo!()
}

pub fn lower_global(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<()> {
    todo!()
}
