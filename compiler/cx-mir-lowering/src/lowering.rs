use std::collections::HashMap;

use cx_lmir::compiler_functions::COMPILER_FUNCTIONS;
use cx_lmir::{LMIRUnit, LinkageType};
use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::MIRUnit;

use crate::context::LMIRGlobalContext;

mod functions;
mod globals;
mod instructions;
mod preservation;
mod typing;

pub(crate) fn lower_unit(unit: &MIRUnit) -> CXResult<LMIRUnit> {
    let mut global = LMIRGlobalContext::new(unit);

    for (_, function) in unit.functions() {
        let mut prototype = typing::convert_prototype(function.prototype(), unit.types());

        if function.body().is_none() {
            prototype.linkage = LinkageType::External;
        }

        global
            .prototypes_mut()
            .insert(prototype.name.to_string(), prototype);
    }

    for compiler_function in COMPILER_FUNCTIONS {
        if global
            .prototypes()
            .contains_key(&compiler_function.symbol_name())
        {
            continue;
        }

        let prototype = (compiler_function.prototype_factory)(*types.architecture());

        global
            .prototypes_mut()
            .insert(prototype.name.to_string(), prototype);
    }

    for (id, mir_global) in unit.globals() {
        let lowered = globals::lower_global(unit, mir_global, unit.types(), &HashMap::new());

        global.add_global(lowered, Some(id));
    }

    for (_, mir_fn) in unit.functions() {
        let Some(body) = mir_fn.body() else {
            continue;
        };

        let function = functions::lower_function(&mut global, mir_fn, body)?;

        global.prototypes_mut().insert(
            function.prototype.name.to_string(),
            function.prototype.clone(),
        );
        global.add_function(function);
    }

    Ok(global.finish())
}
