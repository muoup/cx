use cx_log::CXResult;
use cx_thir::thir::{
    expression::{
        THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRFloatBinOp, THIRIntBinOp,
        THIRUnOp,
    },
    r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeKind},
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::comptime::{
    engine::ComptimeEngine,
    value::{ComptimeKind, ComptimeValue},
};

pub(crate) fn evaluate_expression(
    engine: &mut ComptimeEngine,
    expr: THIRExpression,
) -> CXResult<ComptimeValue> {
    let token_range = expr.token_range.clone();
    let expr_type = expr._type.clone();

    Ok(match expr.kind {
        THIRExpressionKind::BoolLiteral(value) => ComptimeValue {
            token_range,
            kind: ComptimeKind::Integer {
                val: i64::from(value),
                itype: THIRIntType::I1,
                signed: false,
            },
        },

        THIRExpressionKind::IntLiteral(value) => {
            integer_value_from_type(value, &expr_type, THIRIntType::I32, true, token_range)
        }

        THIRExpressionKind::FloatLiteral(value) => {
            float_value_from_type(value, &expr_type, THIRFloatType::F32, token_range)
        }

        THIRExpressionKind::Unit => ComptimeValue {
            token_range,
            kind: ComptimeKind::Unit,
        },

        THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            evaluate_binary_operation(engine, *lhs, *rhs, op, &expr_type, token_range)?
        }

        THIRExpressionKind::UnaryOperation { operand, op } => {
            evaluate_unary_operation(engine, *operand, op, &expr_type, token_range)?
        }

        THIRExpressionKind::Typechange(operand) => {
            let value = evaluate_expression(engine, *operand)?;
            retag_value(value, &expr_type, token_range)?
        }

        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => evaluate_type_conversion(engine, *operand, conversion, &expr_type, token_range)?,

        THIRExpressionKind::Emit(expr) => ComptimeValue {
            token_range,
            kind: ComptimeKind::Emit(*expr),
        },

        THIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = evaluate_expression(engine, *condition)?;
            let Some(condition) = condition.as_integer() else {
                return engine.log_error(
                    condition.token_range,
                    "Expected integer condition in comptime conditional expression".to_string(),
                );
            };

            if condition != 0 {
                evaluate_expression(engine, *then_branch)?
            } else if let Some(else_branch) = else_branch {
                evaluate_expression(engine, *else_branch)?
            } else {
                return engine.log_error(
                    token_range,
                    "Invalid conditional expression in comptime context".to_string(),
                );
            }
        }

        _ => {
            return engine.log_error(
                token_range,
                "Invalid expression in comptime context".to_string(),
            );
        }
    })
}

fn evaluate_binary_operation(
    engine: &mut ComptimeEngine,
    lhs: THIRExpression,
    rhs: THIRExpression,
    op: THIRBinOp,
    result_type: &THIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    match op {
        THIRBinOp::Integer { itype, op } => {
            let lhs = evaluate_expression(engine, lhs)?;
            let rhs = evaluate_expression(engine, rhs)?;
            let Some(lhs) = lhs.as_integer() else {
                return engine.log_error(
                    lhs.token_range,
                    "Expected integer left-hand operand in comptime expression".to_string(),
                );
            };
            let Some(rhs) = rhs.as_integer() else {
                return engine.log_error(
                    rhs.token_range,
                    "Expected integer right-hand operand in comptime expression".to_string(),
                );
            };

            let value = match op {
                THIRIntBinOp::ADD => lhs + rhs,
                THIRIntBinOp::SUB => lhs - rhs,
                THIRIntBinOp::MUL | THIRIntBinOp::IMUL => lhs * rhs,
                THIRIntBinOp::DIV | THIRIntBinOp::IDIV => lhs / rhs,
                THIRIntBinOp::MOD | THIRIntBinOp::IMOD => lhs % rhs,
                THIRIntBinOp::BAND => lhs & rhs,
                THIRIntBinOp::BOR => lhs | rhs,
                THIRIntBinOp::BXOR => lhs ^ rhs,
                THIRIntBinOp::SHL => lhs << rhs,
                THIRIntBinOp::ASHR | THIRIntBinOp::LSHR => lhs >> rhs,
                THIRIntBinOp::EQ => i64::from(lhs == rhs),
                THIRIntBinOp::NE => i64::from(lhs != rhs),
                THIRIntBinOp::LT | THIRIntBinOp::ILT => i64::from(lhs < rhs),
                THIRIntBinOp::LE | THIRIntBinOp::ILE => i64::from(lhs <= rhs),
                THIRIntBinOp::GT | THIRIntBinOp::IGT => i64::from(lhs > rhs),
                THIRIntBinOp::GE | THIRIntBinOp::IGE => i64::from(lhs >= rhs),
                THIRIntBinOp::LAND => i64::from(lhs != 0 && rhs != 0),
                THIRIntBinOp::LOR => i64::from(lhs != 0 || rhs != 0),
            };

            Ok(integer_value_from_type(
                value,
                result_type,
                itype,
                true,
                token_range,
            ))
        }

        THIRBinOp::Float { ftype, op } => {
            let lhs = evaluate_expression(engine, lhs)?;
            let lhs = expect_float(engine, lhs)?;
            let rhs = evaluate_expression(engine, rhs)?;
            let rhs = expect_float(engine, rhs)?;

            match op {
                THIRFloatBinOp::FADD => Ok(float_value_from_type(
                    FloatWrapper::from(lhs + rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                THIRFloatBinOp::FSUB => Ok(float_value_from_type(
                    FloatWrapper::from(lhs - rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                THIRFloatBinOp::FMUL => Ok(float_value_from_type(
                    FloatWrapper::from(lhs * rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                THIRFloatBinOp::FDIV => Ok(float_value_from_type(
                    FloatWrapper::from(lhs / rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                THIRFloatBinOp::FEQ => Ok(bool_value(lhs == rhs, token_range)),
                THIRFloatBinOp::FNE => Ok(bool_value(lhs != rhs, token_range)),
                THIRFloatBinOp::FLT => Ok(bool_value(lhs < rhs, token_range)),
                THIRFloatBinOp::FLE => Ok(bool_value(lhs <= rhs, token_range)),
                THIRFloatBinOp::FGT => Ok(bool_value(lhs > rhs, token_range)),
                THIRFloatBinOp::FGE => Ok(bool_value(lhs >= rhs, token_range)),
            }
        }

        THIRBinOp::PtrDiff { .. } | THIRBinOp::Pointer { .. } => engine.log_error(
            token_range,
            "Invalid pointer operation in comptime context".to_string(),
        ),
    }
}

fn evaluate_unary_operation(
    engine: &mut ComptimeEngine,
    operand: THIRExpression,
    op: THIRUnOp,
    result_type: &THIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    let value = evaluate_expression(engine, operand)?;

    match op {
        THIRUnOp::NEG | THIRUnOp::INEG => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    "Expected integer operand in comptime negation".to_string(),
                );
            };

            Ok(integer_value_from_type(
                -value,
                result_type,
                THIRIntType::I32,
                true,
                token_range,
            ))
        }
        THIRUnOp::FNEG => {
            let value = expect_float(engine, value)?;
            Ok(float_value_from_type(
                FloatWrapper::from(-value),
                result_type,
                THIRFloatType::F32,
                token_range,
            ))
        }
        THIRUnOp::BNOT => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    "Expected integer operand in comptime bitwise not".to_string(),
                );
            };

            Ok(integer_value_from_type(
                !value,
                result_type,
                THIRIntType::I32,
                true,
                token_range,
            ))
        }
        THIRUnOp::LNOT => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    "Expected integer operand in comptime logical not".to_string(),
                );
            };

            Ok(bool_value(value == 0, token_range))
        }
        THIRUnOp::PreIncrement(_) | THIRUnOp::PostIncrement(_) => engine.log_error(
            token_range,
            "Invalid unary expression in comptime context".to_string(),
        ),
    }
}

fn evaluate_type_conversion(
    engine: &mut ComptimeEngine,
    operand: THIRExpression,
    conversion: THIRCoercion,
    result_type: &THIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    let value = evaluate_expression(engine, operand)?;

    match conversion {
        THIRCoercion::Integral { to_type, .. } => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    "Expected integer operand in comptime integral conversion".to_string(),
                );
            };

            Ok(integer_value_from_type(
                value,
                result_type,
                to_type,
                true,
                token_range,
            ))
        }
        THIRCoercion::FloatCast { to_type } => {
            let value = expect_float(engine, value)?;
            Ok(float_value_from_type(
                FloatWrapper::from(value),
                result_type,
                to_type,
                token_range,
            ))
        }
        THIRCoercion::IntToFloat { to_type, .. } => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    "Expected integer operand in comptime integer-to-float conversion".to_string(),
                );
            };

            Ok(float_value_from_type(
                FloatWrapper::from(value as f64),
                result_type,
                to_type,
                token_range,
            ))
        }
        THIRCoercion::FloatToInt { to_type, .. } => {
            let value = expect_float(engine, value)?;
            Ok(integer_value_from_type(
                value as i64,
                result_type,
                to_type,
                true,
                token_range,
            ))
        }
        THIRCoercion::Typechange | THIRCoercion::ReinterpretBits => {
            retag_value(value, result_type, token_range)
        }
        THIRCoercion::PtrToInt { .. } | THIRCoercion::IntToPtr { .. } | THIRCoercion::GetFnPtr => {
            engine.log_error(
                token_range,
                "Invalid conversion in comptime context".to_string(),
            )
        }
    }
}

fn retag_value(
    value: ComptimeValue,
    result_type: &THIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    Ok(match value.kind {
        ComptimeKind::Integer { val, itype, signed } => {
            integer_value_from_type(val, result_type, itype, signed, token_range)
        }
        ComptimeKind::Float { val, ftype } => {
            float_value_from_type(val, result_type, ftype, token_range)
        }
        ComptimeKind::Unit => ComptimeValue {
            kind: ComptimeKind::Unit,
            token_range,
        },
        ComptimeKind::Emit(expr) => ComptimeValue {
            kind: ComptimeKind::Emit(expr),
            token_range,
        },
    })
}

fn expect_float(engine: &mut ComptimeEngine, value: ComptimeValue) -> CXResult<f64> {
    let ComptimeKind::Float { val, .. } = value.kind else {
        return engine.log_error(
            value.token_range,
            "Expected float operand in comptime expression".to_string(),
        );
    };

    Ok((&val).into())
}

fn integer_value_from_type(
    value: i64,
    ty: &THIRType,
    fallback_type: THIRIntType,
    fallback_signed: bool,
    token_range: TokenRange,
) -> ComptimeValue {
    let (itype, signed) = match &ty.kind {
        THIRTypeKind::Integer { _type, signed } => (*_type, *signed),
        _ => (fallback_type, fallback_signed),
    };

    ComptimeValue {
        token_range,
        kind: ComptimeKind::Integer {
            val: value,
            itype,
            signed,
        },
    }
}

fn float_value_from_type(
    value: FloatWrapper,
    ty: &THIRType,
    fallback_type: THIRFloatType,
    token_range: TokenRange,
) -> ComptimeValue {
    let ftype = match &ty.kind {
        THIRTypeKind::Float { _type } => *_type,
        _ => fallback_type,
    };

    ComptimeValue {
        token_range,
        kind: ComptimeKind::Float { val: value, ftype },
    }
}

fn bool_value(value: bool, token_range: TokenRange) -> ComptimeValue {
    ComptimeValue {
        token_range,
        kind: ComptimeKind::Integer {
            val: i64::from(value),
            itype: THIRIntType::I1,
            signed: false,
        },
    }
}
