use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;

pub(crate) mod log;
pub(crate) mod lowering;

use crate::{
    builder::MIRBuilder,
    lowering::{
        comptime::lower_comptime_function, globals, lower_function,
        types::{lower_comptime_prototype, lower_prototype},
    },
};

pub fn generate_mir<'thir>(thir: &'thir THIRUnit) -> CXResult<MIRUnit<'thir>> {
    let mut builder = MIRBuilder::new(thir);

    let mut fn_pairs = vec![];
    let mut comptime_pairs = vec![];
    let mut global_pairs = vec![];
    let mut global_requests = vec![];

    for global in &thir.global_variables {
        builder.module_mut().reserve_global(global.name.as_str());
    }

    for comptime_fn in &thir.comptime_functions {
        let prototype = lower_comptime_prototype(&mut builder, comptime_fn)?;
        let id = builder.module_mut().declare_comptime_function(prototype);

        comptime_pairs.push((comptime_fn, id));
    }

    for (comptime_fn, id) in comptime_pairs {
        lower_comptime_function(&mut builder, id, comptime_fn)?;
    }

    for function in &thir.functions {
        let prototype = lower_prototype(&mut builder, &function.prototype)?;
        let id = builder.module_mut().declare_function(prototype);
        fn_pairs.push((function, id));
    }

    for global in &thir.global_variables {
        let id = globals::predeclare_global(&mut builder, global)?;
        global_pairs.push((global, id));
    }

    for (global, id) in global_pairs.into_iter() {
        if let Some(request) = globals::lower_global(id, global) {
            global_requests.push(request);
        }
    }

    for request in global_requests.into_iter() {
        globals::execute_request(&mut builder, &request)?;
    }

    for (function, id) in fn_pairs.into_iter() {
        lower_function(&mut builder, id, function)?;
    }

    Ok(builder.finish())
}
