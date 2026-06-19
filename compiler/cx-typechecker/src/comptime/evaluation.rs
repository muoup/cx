use cx_log::CXResult;
use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    r#type::{MIRFloatType, MIRIntegerType},
};

use crate::{
    comptime::{engine::ComptimeEngine, value::ComptimeValue},
    log_comptime_error,
};

pub fn lower_comptime_expression<'a>(
    builder: &mut ComptimeEngine,
    comptime_expr: &'a MIRExpression,
) -> CXResult<ComptimeValue<'a>> {
    match &comptime_expr.kind {
        MIRExpressionKind::IntLiteral(val) => Ok(ComptimeValue::Integer {
            val: *val,
            itype: MIRIntegerType::I32,
        }),

        MIRExpressionKind::FloatLiteral(val) => Ok(ComptimeValue::Float {
            val: *val,
            ftype: MIRFloatType::F32,
        }),

        MIRExpressionKind::BoolLiteral(val) => Ok(ComptimeValue::Integer {
            val: if *val { 1 } else { 0 },
            itype: MIRIntegerType::I1,
        }),

        _ => {
            return log_comptime_error!(
                builder,
                comptime_expr.token_range,
                "Invalid expression used in a comptime context"
            );
        }
    }
}
