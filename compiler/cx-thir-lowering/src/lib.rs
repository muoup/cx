use cx_log::CXResult;
use cx_mir::{MIRFunctionMode, MIRUnit};
use cx_thir::THIRUnit;

pub mod builder;

pub(crate) mod lowering;

pub use builder::MIRBuilder;

use crate::lowering::{globals, lower_comptime_function, lower_function};

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);

    let mut fn_pairs = vec![];
    let mut comptime_pairs = vec![];
    let mut global_pairs = vec![];
    let mut global_requests = vec![];

    for function in &thir.functions {
        let prototype =
            builder.lower_prototype(&function.prototype, MIRFunctionMode::Runtime)?;
        let id = builder.module_mut().declare_function(prototype);

        fn_pairs.push((function, id));
    }

    for comptime_fn in &thir.comptime_functions {
        let prototype = builder.lower_comptime_prototype(&comptime_fn.prototype)?;
        let id = builder.module_mut().declare_function(prototype);

        comptime_pairs.push((comptime_fn, id));
    }

    for global in &thir.global_variables {
        let id = globals::predeclare_global(&mut builder, global)?;
        global_pairs.push((global, id));
    }

    for (global, id) in global_pairs.into_iter() {
        if let Some(request) = globals::lower_global(&mut builder, id, global)? {
            global_requests.push(request);
        }
    }

    for request in global_requests.into_iter() {
        globals::fulfill_init_request(&mut builder, request)?;
    }

    for (comptime_fn, id) in comptime_pairs.into_iter() {
        lower_comptime_function(&mut builder, id, comptime_fn)?;
    }

    for (function, id) in fn_pairs.into_iter() {
        lower_function(&mut builder, id, function)?;
    }

    let mut unit = builder.finish();

    let evaluated = cx_mir_comptime::evaluate_unit_globals(&unit)?;
    for (global_id, constant) in evaluated {
        unit.materialize_global(global_id, constant)
            .map_err(|error| {
                cx_log::error::CXError::new(
                    cx_log::error::message::CXStdErrMessage::error(
                        "COMPTIME ERROR",
                        error,
                    ),
                    cx_log::error::context::CXInternalContext::error(
                        "failed to materialize a global initializer",
                    ),
                )
            })?;
    }

    Ok(unit)
}
