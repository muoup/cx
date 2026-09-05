use std::collections::HashMap;

use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::{LMIRFunctionMap, LMIRUnit, LinkageType};
use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{MIRFunctionMode, MIRTypeRegistryBuilder, MIRUnit};

mod functions;
mod globals;
mod instructions;
mod memory;
mod output;
mod typing;

pub(crate) fn lower_unit(mir: &MIRUnit, types: &MIRTypeRegistryBuilder) -> CXResult<LMIRUnit> {
    let mut prototypes = LMIRFunctionMap::new();

    for function in mir.functions() {
        if function.mode() == MIRFunctionMode::Comptime {
            continue;
        }
        let mut prototype = typing::convert_prototype(function.prototype(), types);
        if function.definition().is_none() {
            prototype.linkage = LinkageType::External;
        }
        prototypes.insert(prototype.name.to_string(), prototype);
    }

    prototypes
        .entry(ASSERTION.symbol_name())
        .or_insert_with(|| globals::assertion_prototype(types));

    let mut globals = mir.globals().collect::<Vec<_>>();
    globals.sort_by_key(|global| global.id.index());

    let mut global_indices = HashMap::new();
    for global in &globals {
        let index = global_indices.len() as u32;
        global_indices.insert(global.id, index);
    }

    let mut lowered_globals = globals
        .iter()
        .map(|global| globals::lower_global(mir, global, types, &global_indices))
        .collect::<Vec<_>>();

    let mut functions = Vec::new();
    for function in mir.functions() {
        if function.mode() == MIRFunctionMode::Comptime || function.definition().is_none() {
            continue;
        }
        functions.push(functions::lower_function(
            mir,
            function,
            types,
            &prototypes,
            &global_indices,
            &mut lowered_globals,
        )?);
    }

    Ok(LMIRUnit {
        architecture: *types.architecture(),
        fn_map: prototypes,
        fn_defs: functions,
        global_vars: lowered_globals,
    })
}
