use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
pub(crate) mod lowering;

pub use builder::MIRBuilder;

use crate::lowering::{globals, lower_function};

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);

    let mut fn_pairs = vec![];
    let mut global_pairs = vec![];
    let mut global_requests = vec![];

    for function in &thir.functions {
        let prototype = builder.convert_prototype(&function.prototype)?;
        let id = builder.module_mut().declare_function(prototype);

        fn_pairs.push((function, id)); 
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

    for (function, id) in fn_pairs.into_iter() {
        lower_function(&mut builder, id, function)?;
    }

    Ok(builder.finish())
}
