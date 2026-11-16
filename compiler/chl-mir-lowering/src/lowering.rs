use chl_lmir::compiler_functions::COMPILER_FUNCTIONS;
use chl_lmir::{LMIRUnit, LinkageType};
use chl_log::CHLResult;
use chl_mir::ty::interface::MTRegistry;
use chl_mir::MIRUnit;

use crate::context::GlobalContext;

mod calls;
mod functions;
mod globals;
mod instructions;
pub(crate) mod lifts;
mod intrinsics;
pub(crate) mod memory;
pub(crate) mod typing;
pub(crate) mod values;

pub(crate) fn lower_unit(unit: &MIRUnit) -> CHLResult<LMIRUnit> {
    let mut context = GlobalContext::new(unit);
    for (_, function) in unit.functions() {
        let mut prototype = typing::convert_prototype(function.prototype(), unit.types());
        if function.body().is_none() {
            prototype.linkage = LinkageType::External;
        }
        context
            .prototypes
            .insert(prototype.name.to_string(), prototype);
    }
    for builtin in COMPILER_FUNCTIONS {
        context
            .prototypes
            .entry(builtin.symbol_name())
            .or_insert_with(|| (builtin.prototype_factory)(*unit.types().architecture()));
    }
    globals::lower_globals(&mut context);
    for (_, function) in unit.functions() {
        if let Some(body) = function.body() {
            let lowered = functions::lower_function(&mut context, function, body)?;
            context.functions.push(lowered);
        }
    }
    Ok(context.finish())
}
