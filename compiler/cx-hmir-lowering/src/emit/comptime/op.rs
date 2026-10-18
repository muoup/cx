use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    expr::constant::HMIRConstant,
    ty::{
        HMIRTypeID, HMIRTypeKind,
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatWidth, HMIRIntWidth},
    },
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use super::read;
use crate::{env::function::LocalLoweringContext, log::hmir_error};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_kind(ty).clone()
}

fn constant(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    match read(context, value.clone(), span)?.kind() {
        HMIRValueKind::Constant(constant) => Ok(constant.clone()),
        _ => unreachable!("a comptime read yields a constant"),
    }
}

fn unsupported<T>(span: &TokenRange, operation: &str) -> CXResult<T> {
    Err(hmir_error(
        span,
        &mir::COMPTIME_INVALID_OPERATION,
        operation.into(),
    ))
}

// Integers are held sign or zero extended from their type's width, as a runtime register of it would be read
fn wrap(context: &LocalLoweringContext, value: i128, ty: HMIRTypeID) -> i128 {
    let HMIRTypeKind::Int(int) = kind(context, ty) else {
        unreachable!("an integer constant has an integer type")
    };
    let bits = match int.width() {
        HMIRIntWidth::I1 => return (value != 0) as i128,
        HMIRIntWidth::I128 => return value,
        width => width.bytes() as u32 * 8,
    };

    let truncated = value & ((1i128 << bits) - 1);
    match int.signed() && truncated >> (bits - 1) == 1 {
        true => truncated - (1i128 << bits),
        false => truncated,
    }
}

fn int(context: &LocalLoweringContext, value: i128, ty: HMIRTypeID) -> HMIRValue {
    let value = wrap(context, value, ty);
    HMIRValue::constant(ty, HMIRConstant::Int { value, ty })
}

fn float(context: &LocalLoweringContext, value: f64, ty: HMIRTypeID) -> HMIRValue {
    let value = match kind(context, ty) {
        HMIRTypeKind::Float(float) if float.width() == HMIRFloatWidth::F32 => value as f32 as f64,
        _ => value,
    };
    let value = FloatWrapper::from(value);
    HMIRValue::constant(ty, HMIRConstant::Float { value, ty })
}

fn compare(op: HMIRBinaryOp, ordering: std::cmp::Ordering) -> Option<bool> {
    use std::cmp::Ordering::*;

    Some(match op {
        HMIRBinaryOp::Eq => ordering == Equal,
        HMIRBinaryOp::Neq => ordering != Equal,
        HMIRBinaryOp::Lt => ordering == Less,
        HMIRBinaryOp::Le => ordering != Greater,
        HMIRBinaryOp::Gt => ordering == Greater,
        HMIRBinaryOp::Ge => ordering != Less,
        _ => return None,
    })
}

pub fn binary(
    context: &mut LocalLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let lhs = constant(context, lhs, span)?;
    let rhs = constant(context, rhs, span)?;

    match (lhs, rhs) {
        (HMIRConstant::Int { value: a, .. }, HMIRConstant::Int { value: b, .. }) => {
            if let Some(truth) = compare(op, a.cmp(&b)) {
                return Ok(int(context, truth as i128, result));
            }

            let value = match op {
                HMIRBinaryOp::Div | HMIRBinaryOp::Mod if b == 0 => {
                    let operation = op.path().into();
                    return Err(hmir_error(span, &mir::COMPTIME_ZERO_DIVISOR, operation));
                }
                HMIRBinaryOp::Add => a.wrapping_add(b),
                HMIRBinaryOp::Sub => a.wrapping_sub(b),
                HMIRBinaryOp::Mul => a.wrapping_mul(b),
                HMIRBinaryOp::Div => a.wrapping_div(b),
                HMIRBinaryOp::Mod => a.wrapping_rem(b),
                HMIRBinaryOp::BAnd => a & b,
                HMIRBinaryOp::BOr => a | b,
                HMIRBinaryOp::BXor => a ^ b,
                // Operands are already extended according to their signedness, so a shift of the extended value
                // is arithmetic or logical as the type requires
                HMIRBinaryOp::LShift => a.wrapping_shl(b as u32),
                HMIRBinaryOp::RShift => a.wrapping_shr(b as u32),
                _ => unreachable!("'{}' has no integer form", op.path()),
            };

            Ok(int(context, value, result))
        }
        (HMIRConstant::Float { value: a, .. }, HMIRConstant::Float { value: b, .. }) => {
            let (a, b) = (f64::from(&a), f64::from(&b));
            if let Some(ordering) = a.partial_cmp(&b)
                && let Some(truth) = compare(op, ordering)
            {
                return Ok(int(context, truth as i128, result));
            }

            let value = match op {
                HMIRBinaryOp::Add => a + b,
                HMIRBinaryOp::Sub => a - b,
                HMIRBinaryOp::Mul => a * b,
                HMIRBinaryOp::Div => a / b,
                // An unordered comparison is only true for '!='
                _ if compare(op, std::cmp::Ordering::Equal).is_some() => {
                    return Ok(int(context, (op == HMIRBinaryOp::Neq) as i128, result));
                }
                _ => unreachable!("'{}' has no floating form", op.path()),
            };

            Ok(float(context, value, result))
        }
        (lhs, rhs) if matches!(op, HMIRBinaryOp::Eq | HMIRBinaryOp::Neq) => {
            let equal = match (lhs, rhs) {
                (HMIRConstant::Null(_), HMIRConstant::Null(_)) => true,
                (HMIRConstant::Null(_), HMIRConstant::Address { .. })
                | (HMIRConstant::Address { .. }, HMIRConstant::Null(_)) => false,
                (
                    HMIRConstant::Address {
                        place: a,
                        path: a_path,
                        ..
                    },
                    HMIRConstant::Address {
                        place: b,
                        path: b_path,
                        ..
                    },
                ) => a == b && a_path == b_path,
                _ => return unsupported(span, "comparison of these constants"),
            };

            Ok(int(
                context,
                (equal == (op == HMIRBinaryOp::Eq)) as i128,
                result,
            ))
        }
        _ => unsupported(span, op.path()),
    }
}

pub fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();

    match (op, constant(context, value, span)?) {
        (HMIRUnaryOp::Neg, HMIRConstant::Int { value, .. }) => {
            Ok(int(context, value.wrapping_neg(), ty))
        }
        (HMIRUnaryOp::BNot, HMIRConstant::Int { value, .. }) => Ok(int(context, !value, ty)),
        (HMIRUnaryOp::LNot, HMIRConstant::Int { value, .. }) => {
            Ok(int(context, (value == 0) as i128, ty))
        }
        (HMIRUnaryOp::Neg, HMIRConstant::Float { value, .. }) => {
            Ok(float(context, -f64::from(&value), ty))
        }
        _ => unsupported(span, op.path()),
    }
}

pub fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = constant(context, value, span)?;

    match (source, kind(context, target)) {
        (HMIRConstant::Int { value, .. }, HMIRTypeKind::Int(_)) => Ok(int(context, value, target)),
        (HMIRConstant::Int { value, .. }, HMIRTypeKind::Float(_)) => {
            Ok(float(context, value as f64, target))
        }
        (HMIRConstant::Float { value, .. }, HMIRTypeKind::Int(_)) => {
            Ok(int(context, f64::from(&value) as i128, target))
        }
        (HMIRConstant::Float { value, .. }, HMIRTypeKind::Float(_)) => {
            Ok(float(context, f64::from(&value), target))
        }

        (
            HMIRConstant::Int { value: 0, .. } | HMIRConstant::Null(_),
            HMIRTypeKind::PointerTo(_),
        ) => Ok(HMIRValue::constant(target, HMIRConstant::Null(target))),
        (HMIRConstant::Null(_), HMIRTypeKind::Int(_)) => Ok(int(context, 0, target)),
        (HMIRConstant::Address { place, path, .. }, HMIRTypeKind::PointerTo(_)) => {
            let address = HMIRConstant::Address {
                place,
                path,
                ty: target,
            };
            Ok(HMIRValue::constant(target, address))
        }
        _ => unsupported(span, "conversion"),
    }
}
