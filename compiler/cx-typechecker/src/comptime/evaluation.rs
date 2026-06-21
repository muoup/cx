use cx_log::CXResult;
use cx_mir::mir::{
    expression::{
        MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp,
        MIRUnOp,
    },
    r#type::{MIRFloatType, MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::comptime::{
    engine::ComptimeEngine,
    value::{ComptimeKind, ComptimeValue},
};

pub(crate) fn evaluate_expression(
    engine: &mut ComptimeEngine,
    expr: MIRExpression,
) -> CXResult<ComptimeValue> {
    let token_range = expr.token_range.clone();
    let expr_type = expr._type.clone();

    Ok(match expr.kind {
        MIRExpressionKind::BoolLiteral(value) => ComptimeValue {
            token_range,
            kind: ComptimeKind::Integer {
                val: i64::from(value),
                itype: MIRIntegerType::I1,
                signed: false,
            },
        },

        MIRExpressionKind::IntLiteral(value) => {
            integer_value_from_type(value, &expr_type, MIRIntegerType::I32, true, token_range)
        }

        MIRExpressionKind::FloatLiteral(value) => {
            float_value_from_type(value, &expr_type, MIRFloatType::F32, token_range)
        }

        MIRExpressionKind::Unit => ComptimeValue {
            token_range,
            kind: ComptimeKind::Unit,
        },

        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            evaluate_binary_operation(engine, *lhs, *rhs, op, &expr_type, token_range)?
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            evaluate_unary_operation(engine, *operand, op, &expr_type, token_range)?
        }

        MIRExpressionKind::Typechange(operand) => {
            let value = evaluate_expression(engine, *operand)?;
            retag_value(value, &expr_type, token_range)?
        }

        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => evaluate_type_conversion(engine, *operand, conversion, &expr_type, token_range)?,

        MIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = evaluate_expression(engine, *condition)?;
            let Some(condition) = condition.as_integer() else {
                return engine.log_error(
                    condition.token_range,
                    format!("Expected integer condition in comptime conditional expression"),
                );
            };

            if condition != 0 {
                evaluate_expression(engine, *then_branch)?
            } else if let Some(else_branch) = else_branch {
                evaluate_expression(engine, *else_branch)?
            } else {
                return engine.log_error(
                    token_range,
                    format!("Invalid conditional expression in comptime context"),
                );
            }
        }

        _ => {
            return engine.log_error(
                token_range,
                format!("Invalid expression in comptime context"),
            );
        }
    })
}

fn evaluate_binary_operation(
    engine: &mut ComptimeEngine,
    lhs: MIRExpression,
    rhs: MIRExpression,
    op: MIRBinOp,
    result_type: &MIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    match op {
        MIRBinOp::Integer { itype, op } => {
            let lhs = evaluate_expression(engine, lhs)?;
            let rhs = evaluate_expression(engine, rhs)?;
            let Some(lhs) = lhs.as_integer() else {
                return engine.log_error(
                    lhs.token_range,
                    format!("Expected integer left-hand operand in comptime expression"),
                );
            };
            let Some(rhs) = rhs.as_integer() else {
                return engine.log_error(
                    rhs.token_range,
                    format!("Expected integer right-hand operand in comptime expression"),
                );
            };

            let value = match op {
                MIRIntegerBinOp::ADD => lhs + rhs,
                MIRIntegerBinOp::SUB => lhs - rhs,
                MIRIntegerBinOp::MUL | MIRIntegerBinOp::IMUL => lhs * rhs,
                MIRIntegerBinOp::DIV | MIRIntegerBinOp::IDIV => lhs / rhs,
                MIRIntegerBinOp::MOD | MIRIntegerBinOp::IMOD => lhs % rhs,
                MIRIntegerBinOp::BAND => lhs & rhs,
                MIRIntegerBinOp::BOR => lhs | rhs,
                MIRIntegerBinOp::BXOR => lhs ^ rhs,
                MIRIntegerBinOp::SHL => lhs << rhs,
                MIRIntegerBinOp::ASHR | MIRIntegerBinOp::LSHR => lhs >> rhs,
                MIRIntegerBinOp::EQ => i64::from(lhs == rhs),
                MIRIntegerBinOp::NE => i64::from(lhs != rhs),
                MIRIntegerBinOp::LT | MIRIntegerBinOp::ILT => i64::from(lhs < rhs),
                MIRIntegerBinOp::LE | MIRIntegerBinOp::ILE => i64::from(lhs <= rhs),
                MIRIntegerBinOp::GT | MIRIntegerBinOp::IGT => i64::from(lhs > rhs),
                MIRIntegerBinOp::GE | MIRIntegerBinOp::IGE => i64::from(lhs >= rhs),
                MIRIntegerBinOp::LAND => i64::from(lhs != 0 && rhs != 0),
                MIRIntegerBinOp::LOR => i64::from(lhs != 0 || rhs != 0),
            };

            Ok(integer_value_from_type(
                value,
                result_type,
                itype,
                true,
                token_range,
            ))
        }

        MIRBinOp::Float { ftype, op } => {
            let lhs = evaluate_expression(engine, lhs)?;
            let lhs = expect_float(engine, lhs)?;
            let rhs = evaluate_expression(engine, rhs)?;
            let rhs = expect_float(engine, rhs)?;

            match op {
                MIRFloatBinOp::FADD => Ok(float_value_from_type(
                    FloatWrapper::from(lhs + rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                MIRFloatBinOp::FSUB => Ok(float_value_from_type(
                    FloatWrapper::from(lhs - rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                MIRFloatBinOp::FMUL => Ok(float_value_from_type(
                    FloatWrapper::from(lhs * rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                MIRFloatBinOp::FDIV => Ok(float_value_from_type(
                    FloatWrapper::from(lhs / rhs),
                    result_type,
                    ftype,
                    token_range,
                )),
                MIRFloatBinOp::FEQ => Ok(bool_value(lhs == rhs, token_range)),
                MIRFloatBinOp::FNE => Ok(bool_value(lhs != rhs, token_range)),
                MIRFloatBinOp::FLT => Ok(bool_value(lhs < rhs, token_range)),
                MIRFloatBinOp::FLE => Ok(bool_value(lhs <= rhs, token_range)),
                MIRFloatBinOp::FGT => Ok(bool_value(lhs > rhs, token_range)),
                MIRFloatBinOp::FGE => Ok(bool_value(lhs >= rhs, token_range)),
            }
        }

        MIRBinOp::PtrDiff { .. } | MIRBinOp::Pointer { .. } => engine.log_error(
            token_range,
            format!("Invalid pointer operation in comptime context"),
        ),
    }
}

fn evaluate_unary_operation(
    engine: &mut ComptimeEngine,
    operand: MIRExpression,
    op: MIRUnOp,
    result_type: &MIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    let value = evaluate_expression(engine, operand)?;

    match op {
        MIRUnOp::NEG | MIRUnOp::INEG => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    format!("Expected integer operand in comptime negation"),
                );
            };

            Ok(integer_value_from_type(
                -value,
                result_type,
                MIRIntegerType::I32,
                true,
                token_range,
            ))
        }
        MIRUnOp::FNEG => {
            let value = expect_float(engine, value)?;
            Ok(float_value_from_type(
                FloatWrapper::from(-value),
                result_type,
                MIRFloatType::F32,
                token_range,
            ))
        }
        MIRUnOp::BNOT => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    format!("Expected integer operand in comptime bitwise not"),
                );
            };

            Ok(integer_value_from_type(
                !value,
                result_type,
                MIRIntegerType::I32,
                true,
                token_range,
            ))
        }
        MIRUnOp::LNOT => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    format!("Expected integer operand in comptime logical not"),
                );
            };

            Ok(bool_value(value == 0, token_range))
        }
        MIRUnOp::PreIncrement(_) | MIRUnOp::PostIncrement(_) => engine.log_error(
            token_range,
            format!("Invalid unary expression in comptime context"),
        ),
    }
}

fn evaluate_type_conversion(
    engine: &mut ComptimeEngine,
    operand: MIRExpression,
    conversion: MIRCoercion,
    result_type: &MIRType,
    token_range: TokenRange,
) -> CXResult<ComptimeValue> {
    let value = evaluate_expression(engine, operand)?;

    match conversion {
        MIRCoercion::Integral { to_type, .. } => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    format!("Expected integer operand in comptime integral conversion"),
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
        MIRCoercion::FloatCast { to_type } => {
            let value = expect_float(engine, value)?;
            Ok(float_value_from_type(
                FloatWrapper::from(value),
                result_type,
                to_type,
                token_range,
            ))
        }
        MIRCoercion::IntToFloat { to_type, .. } => {
            let Some(value) = value.as_integer() else {
                return engine.log_error(
                    value.token_range,
                    format!("Expected integer operand in comptime integer-to-float conversion"),
                );
            };

            Ok(float_value_from_type(
                FloatWrapper::from(value as f64),
                result_type,
                to_type,
                token_range,
            ))
        }
        MIRCoercion::FloatToInt { to_type, .. } => {
            let value = expect_float(engine, value)?;
            Ok(integer_value_from_type(
                value as i64,
                result_type,
                to_type,
                true,
                token_range,
            ))
        }
        MIRCoercion::Typechange | MIRCoercion::ReinterpretBits => {
            retag_value(value, result_type, token_range)
        }
        MIRCoercion::PtrToInt { .. } | MIRCoercion::IntToPtr { .. } | MIRCoercion::GetFnPtr => {
            engine.log_error(
                token_range,
                format!("Invalid conversion in comptime context"),
            )
        }
    }
}

fn retag_value(
    value: ComptimeValue,
    result_type: &MIRType,
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
            format!("Expected float operand in comptime expression"),
        );
    };

    Ok((&val).into())
}

fn integer_value_from_type(
    value: i64,
    ty: &MIRType,
    fallback_type: MIRIntegerType,
    fallback_signed: bool,
    token_range: TokenRange,
) -> ComptimeValue {
    let (itype, signed) = match &ty.kind {
        MIRTypeKind::Integer { _type, signed } => (*_type, *signed),
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
    ty: &MIRType,
    fallback_type: MIRFloatType,
    token_range: TokenRange,
) -> ComptimeValue {
    let ftype = match &ty.kind {
        MIRTypeKind::Float { _type } => *_type,
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
            itype: MIRIntegerType::I1,
            signed: false,
        },
    }
}
