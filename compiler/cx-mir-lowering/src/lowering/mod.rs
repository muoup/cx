use std::collections::HashMap;

use cx_lmir::{
    LMIRFunctionMap, LMIRGlobalState as LoweredGlobalState, LMIRGlobalType, LMIRGlobalValue,
    LMIRUnit, LinkageType,
};
use cx_log::CXResult;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{MIRGlobalState, MIRTypeRegistryBuilder, MIRUnit};

mod functions;
mod globals;
mod instructions;
mod memory;
mod output;
mod typing;

pub(crate) fn lower_unit(mir: &MIRUnit, types: &MIRTypeRegistryBuilder) -> CXResult<LMIRUnit> {
    let mut prototypes = LMIRFunctionMap::new();
    
    for function in mir.functions() {
        let prototype = typing::convert_prototype(function.prototype(), types);
        prototypes.insert(prototype.name.to_string(), prototype);
    }
    
    prototypes
        .entry(cx_lmir::compiler_functions::ASSERTION.symbol_name())
        .or_insert_with(|| globals::assertion_prototype(types));

    let mut global_indices = HashMap::new();
    for global in mir.globals() {
        let index = global_indices.len() as u32;
        global_indices.insert(global.id, index);
    }

    let mut lowered_globals = mir
        .globals()
        .map(|global| {
            let linkage = if matches!(global.state, MIRGlobalState::External) {
                LinkageType::External
            } else {
                typing::convert_linkage(global.linkage)
            };
            let lowered_type = typing::convert_type(global.ty, types);
            let lowered = match &global.state {
                MIRGlobalState::External => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::External,
                },
                MIRGlobalState::ZeroInitialized => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::ZeroInitialized,
                },
                MIRGlobalState::Initialized(cx_mir::MIRConstant::Unit) => {
                    LMIRGlobalType::Variable {
                        _type: lowered_type,
                        state: LoweredGlobalState::ZeroInitialized,
                    }
                }
                MIRGlobalState::Initialized(constant) => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::Initialized(globals::lower_global_initializer(
                        mir,
                        constant,
                        &global_indices,
                    )),
                },
            };
            LMIRGlobalValue {
                name: global.name.clone(),
                _type: lowered,
                linkage,
            }
        })
        .collect::<Vec<_>>();

    let mut functions = Vec::new();
    for function in mir.functions() {
        if function.definition().is_none() {
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
