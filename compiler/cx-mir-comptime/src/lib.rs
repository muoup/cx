mod engine;
mod error;
mod interpretable;
mod value;

pub mod context;

pub use context::ComptimeResolver;
pub use engine::{EngineLimits, MIRComptimeEngine};
pub use interpretable::{ComptimeInterpretable, InterpretedFunction};
pub use value::{MIRComptimeValue, MIRStagedBinding, MIRStagedValue};

use crate::{context::MIRContext, error::comptime_error};
use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRFunctionMode, MIRUnit, MIRValue};
use cx_thir::thir::expression::THIRExpression;

/// If while lowering a THIRFunction, a comptime expression is encountered, this function will handle the different contexts of what
/// and how this expression should be handled, acting as a mostly drop-in replacement for the standard lowering format.
pub fn lower_comptime_expression<T: MIRContext>(
    context: &mut T,
    expr: &THIRExpression,
) -> CXResult<MIRValue> {
    match context.current_prototype().signature.mode {
        MIRFunctionMode::Runtime | MIRFunctionMode::Constexpr => {
            let value = evaluate_compite_expr(context, expr)?;
            lower_comptime_value(context, value)
        }
        MIRFunctionMode::Comptime => generate_comptime_instructions(context, expr),
    }
}

pub fn lower_comptime_value<T: MIRContext>(
    _context: &mut T,
    expr: MIRComptimeValue,
) -> CXResult<MIRValue> {
    Ok(match expr {
        MIRComptimeValue::Constant(value) => MIRValue::Constant(value),
        MIRComptimeValue::Staged(_) => {
            return comptime_error(
                expr_token_range(),
                "staged values must be instantiated by the MIR lowering context",
            );
        }
    })
}

/// Lowers a comptime expression verbatim into the active instruction stream. Used inside comptime-mode functions where evaluation
/// must be deferred to the interpreting engine rather than folded away at lowering time.
pub fn generate_comptime_instructions<T: MIRContext>(
    context: &mut T,
    expr: &THIRExpression,
) -> CXResult<MIRValue> {
    context.lower_thir(expr)
}

/// Captures a standalone lowering of a THIR expression into an anonymous comptime function and interprets it into a constant.
pub fn evaluate_compite_expr<T: MIRContext>(
    context: &mut T,
    expr: &THIRExpression,
) -> CXResult<MIRComptimeValue> {
    let function = context.capture_expression(expr)?;
    let resolver = context.comptime_resolver();
    let mut engine = MIRComptimeEngine::new(resolver);
    let entry =
        InterpretedFunction::new(&function).expect("captured comptime functions have definitions");

    let constant = engine.run(entry, &[])?;
    Ok(MIRComptimeValue::Constant(constant))
}

/// Evaluates every pending global initializer in the unit by interpreting its anonymous init function, returning the resolved
/// constants in declaration order for the caller to materialize.
pub fn evaluate_unit_globals(unit: &MIRUnit) -> CXResult<Vec<(cx_mir::MIRGlobalID, MIRConstant)>> {
    use cx_mir::{MIRGlobalKind, MIRGlobalState};

    let mut engine = MIRComptimeEngine::new(unit);
    let mut evaluated = Vec::new();

    for global in unit.globals_in_order() {
        let MIRGlobalKind::Variable { state, .. } = &global.kind else {
            continue;
        };
        let MIRGlobalState::Initializer(function_id) = state else {
            continue;
        };
        let Some(function) = unit.function(*function_id) else {
            continue;
        };
        let Some(entry) = InterpretedFunction::new(function) else {
            continue;
        };

        let constant = engine.run(entry, &[])?;
        evaluated.push((global.id, constant));
    }

    Ok(evaluated)
}

fn expr_token_range() -> cx_tokens::TokenRange {
    cx_tokens::TokenRange::internal()
}
