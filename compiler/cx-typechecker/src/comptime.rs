use cx_log::CXResult;
use cx_mir::mir::expression::MIRExpression;

use crate::{
    comptime::{engine::ComptimeEngine, evaluation::evaluate_expression, value::ComptimeValue},
    environment::TypeEnvironment,
};

pub(crate) mod engine;
pub(crate) mod evaluation;
pub(crate) mod value;

pub fn evaluate_comptime_expression(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<ComptimeValue> {
    evaluate_expression(&mut ComptimeEngine::new(env), expr)
}
