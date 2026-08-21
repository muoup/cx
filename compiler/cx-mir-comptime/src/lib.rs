mod engine;
mod error;
mod value;

pub mod context;

use crate::{context::MIRContext, value::MIRComptimeValue};
use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRFunctionMode, MIRValue};
use cx_thir::thir::expression::THIRExpression;

/// If while lowering a THIRFunction, a comptime expression is encountered, this function will handle the different contexts of what
/// and how this expression should be handled, acting as a mostly drop-in replacement for the standard lowering format.
pub fn lower_comptime_expression<T: MIRContext>(
    context: &mut T,
    expr: &THIRExpression,
) -> CXResult<MIRValue> {
    let comptime_value = match context.current_function().mode() {
        // In a comptime context, if for instance a comptime function is invoked, we DONT want to eagerly evaluate this, as
        // eager evaluation requires the function to be pre-generated, and recursive function call chains will fail. Therefore
        // we should just generate normal instructions that defer this evaluation to the comptime engine we are guaranteed to be
        // interpreted by, given that we are in a comptime context.
        //
        // In a runtime / contexpr context however, we must accomodate the future codegen of the function, so having comptime-only
        // instructions will cause an error, as such at some point, direct evaluation into runtime instructions is needed, and here
        // is the cleanest part of the pipeline.
        MIRFunctionMode::Runtime | MIRFunctionMode::Constexpr => {
            evaluate_comptime_expr(context, expr)?
        }
        MIRFunctionMode::Comptime => generate_comptime_instructions(context, expr)?,
    };

    lower_comptime_value(context, comptime_value)
}

pub fn lower_comptime_value<T: MIRContext>(
    context: &mut T,
    expr: MIRComptimeValue,
) -> CXResult<MIRValue> {
    Ok(match expr {
        MIRComptimeValue::Staged { expr, parameters } => {
            if !parameters.is_empty() {
                return context.log_error(
                    expr.token_range.clone(),
                    "Produced staged expression requires parameters to be materialized",
                );
            }

            evaluate_comptime_expr(context, expr)
                .and_then(|val| lower_comptime_value(context, val))?
        }

        MIRComptimeValue::Integer { val, _ty, signed } => {
            MIRValue::Constant(MIRConstant::Integer {
                value: val,
                ty: _ty,
                signed,
            })
        }

        _ => todo!(),
    })
}

pub fn generate_comptime_instructions<'a, T: MIRContext>(
    context: &mut T,
    expr: &'a THIRExpression,
) -> CXResult<MIRComptimeValue<'a>> {
    todo!()
}

pub fn evaluate_comptime_expr<'a, T: MIRContext>(
    context: &mut T,
    expr: &'a THIRExpression,
) -> CXResult<MIRComptimeValue<'a>> {
    todo!()
}
