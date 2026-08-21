use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;
pub(crate) mod lowering;

pub use builder::MIRBuilder;

pub fn generate_mir(thir: &THIRUnit) -> CXResult<MIRUnit> {
    let mut builder = MIRBuilder::new(thir);

    let mut fn_pairs = vec![];
    let mut global_pairs = vec![];
    let mut global_requests = vec![];

    for function in &thir.functions {
        let prototype = builder.convert_prototype(&function.prototype)?;
        let id = builder.module().declare_function(prototype);

        fn_pairs.push((function, id)); 
    }

    for global in &thir.global_variables {
        let id = globals::predeclare_global(builder, global)?;
        global_pairs.push((global, id));
    }

    for (global, id) in global_pairs.into_iter() {
        if let Some(request) = globals::lower_global(builder, id, global)? {
            global_requests.push(request);
        }
    }

    for request in global_requests.into_iter() {
        fulfill_init_request(builder, request)?;
    }

    for (function, id) in fn_pairs.into_iter() {
        lower_function(builder, id, function)?;
    }

    Ok(builder.finish())
}
