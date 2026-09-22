use cx_log::CXResult;
use cx_mir::MIRUnit;
use cx_thir::THIRUnit;

pub mod builder;

pub(crate) mod log;
pub(crate) mod lowering;

use crate::{
    builder::MIRBuilder,
    lowering::{
        globals, lower_function,
    },
};

pub fn generate_mir<'thir>(thir: &'thir THIRUnit) -> CXResult<MIRUnit<'thir>> {
    let mut builder = MIRBuilder::new(thir);

    let mut fn_pairs = vec![];
    let mut global_pairs = vec![];

    for function in &thir.functions {
        let prototype = lowering::types::lower_prototype(&mut builder, &function.prototype)?;
        let id = builder.module_mut().declare_function(prototype);

        fn_pairs.push((function, id));
    }

    for global in &thir.global_variables {
        let id = globals::predeclare_global(&mut builder, global)?;
        global_pairs.push((global, id));
    }

    for (global, id) in global_pairs {
        globals::lower_global(&mut builder, id, global)?;
    }

    for (function, id) in fn_pairs.into_iter() {
        lower_function(&mut builder, id, function)?;
    }

    Ok(builder.finish())
}
