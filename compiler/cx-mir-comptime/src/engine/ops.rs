use cx_log::{CXResult, catalogue::mir as catalogue};
use cx_mir::{
    MIRBinaryOp, MIRCoercion, MIRConstant, MIRFloatBinaryOp, MIRIntBinaryOp,
    MIRIntType, MIRPointerBinaryOp, MIRPointerOffsetOp, MIRTypeID, MIRUnaryOp,
    ty::layout::layout_of,
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{ComptimeContext, log::comptime_error};

use super::MIRComptimeEngine;

pub(super) fn evaluate_binop(
    engine: &MIRComptimeEngine<'_, impl ComptimeContext>,
    op: MIRBinaryOp,
    lhs: MIRConstant,
    rhs: MIRConstant,
) -> CXResult<MIRConstant> {
    match op {
        MIRBinaryOp::Integer { ty, signed, op } => {
            integer_binop(ty, signed, op, as_integer(&lhs), as_integer(&rhs))
        }
        MIRBinaryOp::Float { ty, op } => {
            let lhs = as_float(&lhs);
            let rhs = as_float(&rhs);
            let result = match op {
                MIRFloatBinaryOp::Add => lhs + rhs,
                MIRFloatBinaryOp::Sub => lhs - rhs,
                MIRFloatBinaryOp::Mul => lhs * rhs,
                MIRFloatBinaryOp::Div => lhs / rhs,
                MIRFloatBinaryOp::Eq => return Ok(bool_constant(lhs == rhs)),
                MIRFloatBinaryOp::Ne => return Ok(bool_constant(lhs != rhs)),
                MIRFloatBinaryOp::Lt => return Ok(bool_constant(lhs < rhs)),
                MIRFloatBinaryOp::Le => return Ok(bool_constant(lhs <= rhs)),
                MIRFloatBinaryOp::Gt => return Ok(bool_constant(lhs > rhs)),
                MIRFloatBinaryOp::Ge => return Ok(bool_constant(lhs >= rhs)),
            };
            Ok(MIRConstant::Float {
                value: FloatWrapper::from(result),
                ty,
            })
        }
        MIRBinaryOp::PointerOffset { op, pointee } => {
            let (global, base_offset) = match &lhs {
                MIRConstant::Global { global, offset, .. } => (*global, *offset),
                other => {
                    return comptime_error(
                        TokenRange::internal(),
                        (
                            &catalogue::ENTITY_REQUIREMENT,
                            ("pointer arithmetic".into(), "pointer values".into(), Some(format!("{:?}", other))),
                        ),
                    );
                }
            };
            let count = as_integer(&rhs);
            let Ok(count) = i64::try_from(count) else {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::COMPTIME_POINTER_OVERFLOW, ()),
                );
            };
            let stride = match layout_of(engine.context.types(), pointee) {
                Ok(layout) => layout.size as i64,
                Err(_) => {
                    return comptime_error(
                        TokenRange::internal(),
                        (&catalogue::INVALID_LAYOUT, ("pointee".into(), "a valid layout".into(), None)),
                    );
                }
            };
            let Some(delta) = count.checked_mul(stride) else {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::COMPTIME_POINTER_OVERFLOW, ()),
                );
            };
            let offset = match op {
                MIRPointerOffsetOp::Add => base_offset.checked_add(delta),
                MIRPointerOffsetOp::Sub => base_offset.checked_sub(delta),
            };
            let Some(offset) = offset else {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::COMPTIME_POINTER_OVERFLOW, ()),
                );
            };
            Ok(MIRConstant::Global { global, offset, ty: pointee })
        }
        MIRBinaryOp::Pointer(op) => {
            let equal = pointer_constants_equal(&lhs, &rhs)?;
            let result = match op {
                MIRPointerBinaryOp::Eq => equal,
                MIRPointerBinaryOp::Ne => !equal,
                MIRPointerBinaryOp::Lt
                | MIRPointerBinaryOp::Le
                | MIRPointerBinaryOp::Gt
                | MIRPointerBinaryOp::Ge => {
                    return comptime_error(
                        TokenRange::internal(),
                        (&catalogue::COMPTIME_INVALID_OPERATION, "ordered pointer comparison".into()),
                    );
                }
            };
            Ok(bool_constant(result))
        }
    }
}

fn integer_binop(
    ty: MIRIntType,
    signed: bool,
    op: MIRIntBinaryOp,
    lhs: i128,
    rhs: i128,
) -> CXResult<MIRConstant> {
    use MIRIntBinaryOp as Op;

    let int = |value: i128| MIRConstant::Integer { value, ty, signed };
    let boolean = bool_constant;

    Ok(match op {
        Op::Add => int(lhs.wrapping_add(rhs)),
        Op::Sub => int(lhs.wrapping_sub(rhs)),
        Op::Mul | Op::SignedMul => int(lhs.wrapping_mul(rhs)),
        Op::Div | Op::SignedDiv => {
            if rhs == 0 {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::COMPTIME_ZERO_DIVISOR, "division".into()),
                );
            }
            int(lhs.wrapping_div(rhs))
        }
        Op::Mod | Op::SignedMod => {
            if rhs == 0 {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::COMPTIME_ZERO_DIVISOR, "remainder".into()),
                );
            }
            int(lhs.wrapping_rem(rhs))
        }
        Op::Eq => boolean(lhs == rhs),
        Op::Ne => boolean(lhs != rhs),
        Op::Lt | Op::SignedLt => boolean(if signed || matches!(op, Op::SignedLt) {
            lhs < rhs
        } else {
            (lhs as u128) < (rhs as u128)
        }),
        Op::Le | Op::SignedLe => boolean(if signed || matches!(op, Op::SignedLe) {
            lhs <= rhs
        } else {
            (lhs as u128) <= (rhs as u128)
        }),
        Op::Gt | Op::SignedGt => boolean(if signed || matches!(op, Op::SignedGt) {
            lhs > rhs
        } else {
            (lhs as u128) > (rhs as u128)
        }),
        Op::Ge | Op::SignedGe => boolean(if signed || matches!(op, Op::SignedGe) {
            lhs >= rhs
        } else {
            (lhs as u128) >= (rhs as u128)
        }),
        Op::LogicalAnd => boolean(is_truthy(&int(lhs)) && is_truthy(&int(rhs))),
        Op::LogicalOr => boolean(is_truthy(&int(lhs)) || is_truthy(&int(rhs))),
        Op::BitAnd => int(lhs & rhs),
        Op::BitOr => int(lhs | rhs),
        Op::BitXor => int(lhs ^ rhs),
        Op::ShiftLeft => int(width_masked(lhs << (rhs & 127), ty)),
        Op::ArithmeticShiftRight => int(lhs >> (rhs & 127)),
        Op::LogicalShiftRight => int(((lhs as u128) >> (rhs & 127)) as i128),
    })
}

pub(super) fn evaluate_unop(op: MIRUnaryOp, operand: MIRConstant) -> CXResult<MIRConstant> {
    Ok(match op {
        MIRUnaryOp::IntegerNeg { ty, signed } => {
            let value = as_integer(&operand).wrapping_neg();
            MIRConstant::Integer { value, ty, signed }
        }
        MIRUnaryOp::FloatNeg(ty) => MIRConstant::Float {
            value: FloatWrapper::from(-as_float(&operand)),
            ty,
        },
        MIRUnaryOp::BitNot(ty) => MIRConstant::Integer {
            value: width_masked(!as_integer(&operand), ty),
            ty,
            signed: false,
        },
        MIRUnaryOp::LogicalNot => bool_constant(!is_truthy(&operand)),
        MIRUnaryOp::Increment { .. } => {
            return comptime_error(
                TokenRange::internal(),
                (&catalogue::REQUIRED_CONTEXT, ("increment".into(), "the comptime execution loop".into())),
            );
        }
    })
}

pub(super) fn evaluate_coercion(
    coercion: MIRCoercion,
    operand: MIRConstant,
    to_type: MIRTypeID,
) -> CXResult<MIRConstant> {
    Ok(match coercion {
        MIRCoercion::Integral {
            sign_extend,
            from,
            to,
        } => {
            let _ = from;
            let source_signed = matches!(operand, MIRConstant::Integer { signed: true, .. });
            let raw = as_integer(&operand);
            let bits = to.bytes() as u32 * 8;
            let mut value = width_masked(raw, to);
            if sign_extend && source_signed && bits > 0 && bits < 128 {
                let shift = 128 - bits;
                value = (value << shift) >> shift;
            }
            MIRConstant::Integer {
                value,
                ty: to,
                signed: source_signed,
            }
        }
        MIRCoercion::FloatCast { to, .. } => MIRConstant::Float {
            value: FloatWrapper::from(as_float(&operand)),
            ty: to,
        },
        MIRCoercion::IntToFloat { signed, to, .. } => {
            let raw = as_integer(&operand);
            let value = if signed {
                raw as f64
            } else {
                (raw as u128) as f64
            };
            MIRConstant::Float {
                value: FloatWrapper::from(value),
                ty: to,
            }
        }
        MIRCoercion::FloatToInt { signed, to, .. } => {
            let value = as_float(&operand) as i128;
            MIRConstant::Integer {
                value: width_masked(value, to),
                ty: to,
                signed,
            }
        }
        MIRCoercion::PointerToInt { .. } => match &operand {
            MIRConstant::Nullptr { .. } => MIRConstant::Integer {
                value: 0,
                ty: MIRIntType::I64,
                signed: false,
            },
            _ => {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::ENTITY_REQUIREMENT, ("pointer-to-integer coercion".into(), "a null pointer".into(), Some("non-null pointer".into()))),
                );
            }
        },
        MIRCoercion::IntToPointer { .. } => {
            match operand {
                MIRConstant::Integer { value: 0, .. } => MIRConstant::Nullptr { ty: to_type },

                _ => return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::ENTITY_REQUIREMENT, ("integer-to-pointer coercion".into(), "a null integer".into(), Some("non-zero integer".into()))),
                ),
            }
        }
        MIRCoercion::FunctionToPointer => match &operand {
            MIRConstant::Function(_) | MIRConstant::Nullptr { .. } => operand.clone(),
            
            _ => {
                return comptime_error(
                    TokenRange::internal(),
                    (&catalogue::ENTITY_REQUIREMENT, ("function-to-pointer coercion".into(), "a function value".into(), Some(format!("{:?}", operand)))),
                );
            }
        },
        MIRCoercion::TypeChange | MIRCoercion::ReinterpretBits => operand,
    })
}

fn pointer_constants_equal(lhs: &MIRConstant, rhs: &MIRConstant) -> CXResult<bool> {
    let as_address = |constant: &MIRConstant| match constant {
        MIRConstant::Nullptr { .. } => Some(None),
        MIRConstant::Global { global, .. } => Some(Some((*global, 0i64))),

        _ => None,
    };

    let (lhs_address, rhs_address) = match (as_address(lhs), as_address(rhs)) {
        (Some(lhs), Some(rhs)) => (lhs, rhs),
        _ => {
            return comptime_error(
                TokenRange::internal(),
                (
                    &catalogue::ENTITY_REQUIREMENT,
                    ("pointer comparison".into(), "pointer constants".into(), Some(format!("{:?} and {:?}", lhs, rhs))),
                ),
            );
        }
    };

    Ok(match (lhs_address, rhs_address) {
        (None, None) => true,
        (None, Some(_)) | (Some(_), None) => false,
        (Some(lhs), Some(rhs)) => lhs == rhs,
    })
}

pub(super) fn is_truthy(constant: &MIRConstant) -> bool {
    match constant {
        MIRConstant::Integer { value, .. } => *value != 0,
        MIRConstant::Nullptr { .. } | MIRConstant::Undefined => false,
        _ => true,
    }
}

pub(super) fn constant_equals(lhs: &MIRConstant, rhs: &MIRConstant) -> bool {
    match (lhs, rhs) {
        (MIRConstant::Integer { value: l, .. }, MIRConstant::Integer { value: r, .. }) => l == r,
        _ => lhs == rhs,
    }
}

fn as_integer(constant: &MIRConstant) -> i128 {
    match constant {
        MIRConstant::Integer { value, .. } => *value,
        _ => 0,
    }
}

fn as_float(constant: &MIRConstant) -> f64 {
    match constant {
        MIRConstant::Float { value, .. } => f64::from(value),
        MIRConstant::Integer { value, .. } => *value as f64,
        _ => 0.0,
    }
}

pub(super) fn variant_discriminant(constant: &MIRConstant) -> Option<usize> {
    match constant {
        MIRConstant::Aggregate { fields, .. } => fields.first().map(|(index, _)| *index),
        _ => None,
    }
}

pub(super) fn increment_constant(constant: MIRConstant, amount: i8) -> CXResult<MIRConstant> {
    match constant {
        MIRConstant::Integer { value, ty, signed } => Ok(MIRConstant::Integer {
            value: value.wrapping_add(amount as i128),
            ty,
            signed,
        }),
        other => comptime_error(
            TokenRange::internal(),
            (
                &catalogue::ENTITY_REQUIREMENT,
                ("increment operand".into(), "an integer constant".into(), Some(format!("{:?}", other))),
            ),
        ),
    }
}

fn width_masked(value: i128, ty: MIRIntType) -> i128 {
    let bits = ty.bytes() as u32 * 8;
    if bits == 0 || bits >= 128 {
        value
    } else {
        value & ((1i128 << bits) - 1)
    }
}

fn bool_constant(value: bool) -> MIRConstant {
    MIRConstant::Integer {
        value: value as i128,
        ty: MIRIntType::I1,
        signed: false,
    }
}
