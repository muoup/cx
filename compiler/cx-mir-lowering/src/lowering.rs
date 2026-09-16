use std::collections::HashMap;

use cx_lmir::compiler_functions::COMPILER_FUNCTIONS;
use cx_lmir::{LMIRUnit, LinkageType};
use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::registry::MIRTypeRegistry;
use cx_mir::{MIRFunctionMode, MIRUnit};

use crate::context::LMIRGlobalContext;

mod functions;
mod globals;
mod instructions;
mod typing;

pub(crate) fn lower_unit(unit: &MIRUnit, types: &MIRTypeRegistry) -> CXResult<LMIRUnit> {
    let mut global = LMIRGlobalContext::new(unit);

    for function in unit.functions() {
        if function.mode() == MIRFunctionMode::Comptime {
            continue;
        }

        let mut prototype = typing::convert_prototype(function.prototype(), types);
        if function.definition().is_none() {
            prototype.linkage = LinkageType::External;
        }

        global
            .prototypes_mut()
            .insert(prototype.name.to_string(), prototype);
    }

    for compiler_function in COMPILER_FUNCTIONS {
        if !global
            .prototypes()
            .contains_key(&compiler_function.symbol_name())
        {
            let prototype = (compiler_function.prototype_factory)(types.architecture());

            global
                .prototypes_mut()
                .insert(prototype.name.to_string(), prototype);
        }
    }

    for mir_global in unit.globals() {
        let lowered = globals::lower_global(unit, mir_global, types, &HashMap::new());

        global.add_global(lowered, Some(mir_global.id));
    }

    for mir_fn in unit.functions() {
        if mir_fn.mode() == MIRFunctionMode::Comptime {
            continue;
        }

        let Some(body) = mir_fn.definition() else {
            continue;
        };

        let function = functions::lower_function(&mut global, mir_fn.prototype(), body)?;
        global.prototypes_mut().insert(
            mir_fn.prototype().symbol_name.to_string(),
            function.prototype.clone(),
        );
    }

    Ok(LMIRUnit {
        architecture: *types.architecture(),
        fn_map: prototypes,
        fn_defs: functions,
        global_vars: lowered_globals,
    })
}
