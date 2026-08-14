mod aggregates;
mod calls;
mod control_flow;
mod expression;
mod memory;
mod operators;

use crate::builder::MIRBuilder;
use cx_log::CXResult;
use cx_thir::THIRUnit;

pub(crate) fn lower_unit(builder: &mut MIRBuilder<'_>, thir: &THIRUnit) -> CXResult<()> {
    for (index, function) in thir.functions.iter().enumerate() {
        lower_function(builder, index, function)?;
    }
    Ok(())
}

fn lower_function(
    builder: &mut MIRBuilder<'_>,
    index: usize,
    function: &cx_thir::thir::data::THIRFunction,
) -> CXResult<()> {
    builder.start_function(index, function);
    expression::lower_expression(builder, &function.body)?;
    builder.finish_function();
    Ok(())
}
