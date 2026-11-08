use std::cmp::Ordering;

use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use super::rvalue;
use crate::{
    emit::comptime::{ScalarFault, float, int},
    env::function::{LocalLoweringContext, memory::layout},
    log::hmir_error,
    value::{
        HMIRValue,
        immediate::{HMIRAddress, HMIRImmediate},
    },
};

fn integer(context: &LocalLoweringContext, value: i128, ty: HMIRTypeID) -> HMIRValue {
    let HMIRTypeKind::Int(int) = context.env().resolve_kind(ty) else {
        unreachable!("an integer constant has an integer type")
    };
    let value = int::normalize(value, *int);

    HMIRValue::new(ty, HMIRImmediate::Int(value))
}

fn floating(context: &LocalLoweringContext, value: f64, ty: HMIRTypeID) -> HMIRValue {
    let HMIRTypeKind::Float(float) = context.env().resolve_kind(ty) else {
        unreachable!("a floating constant has a floating type")
    };
    let value = FloatWrapper::from(float::round(value, float.width()));

    HMIRValue::new(ty, HMIRImmediate::Float(value))
}

fn fault(span: &TokenRange, fault: ScalarFault, operation: &str) -> CXError {
    match fault {
        ScalarFault::ZeroDivisor => hmir_error(span, &mir::COMPTIME_ZERO_DIVISOR, operation.into()),
        ScalarFault::Undefined => {
            hmir_error(span, &mir::COMPTIME_UNDEFINED_ARITHMETIC, operation.into())
        }
    }
}

fn unsupported<T>(span: &TokenRange, operation: String) -> CXResult<T> {
    Err(hmir_error(
        span,
        &mir::COMPTIME_INVALID_OPERATION,
        operation,
    ))
}

fn compare(lhs: &HMIRImmediate, rhs: &HMIRImmediate) -> Option<Ordering> {
    Some(match (lhs, rhs) {
        (HMIRImmediate::Unit, HMIRImmediate::Unit) => Ordering::Equal,
        (HMIRImmediate::Address(lhs), HMIRImmediate::Address(rhs)) if lhs.base() == rhs.base() => {
            lhs.offset().cmp(&rhs.offset())
        },
        (HMIRImmediate::Int(i1), HMIRImmediate::Int(i2)) => {
            i1.cmp(i2)
        },
        (HMIRImmediate::Float(f1), HMIRImmediate::Float(f2)) => {
            f1.cmp(f2)
        },

        _ => return None
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
    let kind = context.env().resolve_kind(lhs.ty()).clone();
    let operands = &kind;
    let constants = (rvalue(context, lhs, span)?, rvalue(context, rhs, span)?);

    match (operands, constants) {
        (HMIRTypeKind::Int(ty), (HMIRImmediate::Int(lhs), HMIRImmediate::Int(rhs))) => {
            if let Some(truth) = int::compare(op, lhs, rhs, *ty) {
                return Ok(integer(context, truth as i128, result));
            }

            int::arithmetic(op, lhs, rhs, *ty)
                .map(|value| integer(context, value, result))
                .map_err(|error| fault(span, error, op.path()))
        }
        (HMIRTypeKind::Float(ty), (HMIRImmediate::Float(lhs), HMIRImmediate::Float(rhs))) => {
            let (lhs, rhs) = (
                float::round(f64::from(&lhs), ty.width()),
                float::round(f64::from(&rhs), ty.width()),
            );
            if let Some(truth) = float::compare(op, lhs, rhs) {
                return Ok(integer(context, truth as i128, result));
            }

            let value = float::arithmetic(op, lhs, rhs, ty.width());
            Ok(floating(context, value, result))
        }
        (HMIRTypeKind::PointerTo(_), (lhs, rhs)) => {
            let Some(truth) = compare(op, &lhs, &rhs) else {
                return unsupported(span, "comparison of these addresses".into());
            };

            Ok(integer(context, truth as i128, result))
        }
        _ => unsupported(span, format!("'{}' on these operands", op.path())),
    }
}

pub fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();

    let kind = context.env().resolve_kind(ty).clone();
    match (&kind, rvalue(context, value, span)?) {
        (HMIRTypeKind::Int(int), HMIRImmediate::Int(value)) => {
            Ok(integer(context, int::unary(op, value, *int), ty))
        }
        (HMIRTypeKind::Float(_), HMIRImmediate::Float(value)) if op == HMIRUnaryOp::Neg => {
            Ok(floating(context, -f64::from(&value), ty))
        }
        _ => unsupported(span, format!("'{}' on this operand", op.path())),
    }
}

pub fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source_kind = context.env().resolve_kind(value.ty()).clone();
    let target_kind = context.env().resolve_kind(target).clone();
    let kinds = (&source_kind, &target_kind);

    match (kinds, rvalue(context, value, span)?) {
        ((HMIRTypeKind::Int(from), HMIRTypeKind::Int(_)), HMIRImmediate::Int(value)) => {
            Ok(integer(context, int::normalize(value, *from), target))
        }
        ((HMIRTypeKind::Int(from), HMIRTypeKind::Float(to)), HMIRImmediate::Int(value)) => {
            let value = int::to_float(value, *from, to.width());
            Ok(floating(context, value, target))
        }
        ((HMIRTypeKind::Float(from), HMIRTypeKind::Int(to)), HMIRImmediate::Float(value)) => {
            let value = float::round(f64::from(&value), from.width());
            float::to_int(value, *to)
                .map(|value| integer(context, value, target))
                .map_err(|error| fault(span, error, "conversion"))
        }
        ((HMIRTypeKind::Float(from), HMIRTypeKind::Float(_)), HMIRImmediate::Float(value)) => {
            Ok(floating(
                context,
                float::round(f64::from(&value), from.width()),
                target,
            ))
        }

        ((HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)), HMIRImmediate::Int(value))
            if int::normalize(value, *from) == 0 =>
        {
            Ok(HMIRValue::new(target, HMIRImmediate::Int(0)))
        }
        ((HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)), HMIRImmediate::Int(0)) => {
            Ok(integer(context, 0, target))
        }
        ((HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)), HMIRImmediate::Int(0)) => {
            Ok(HMIRValue::new(target, HMIRImmediate::Int(0)))
        }
        (
            (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)),
            address @ HMIRImmediate::Address(_),
        ) => Ok(HMIRValue::new(target, address)),

        _ => unsupported(span, "this conversion".into()),
    }
}

fn element_size(
    context: &mut LocalLoweringContext,
    element: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<i128> {
    Ok(layout(context.env_mut(), element, span)?.size() as i128)
}

pub fn offset(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRImmediate::Int(index) = rvalue(context, index, span)? else {
        unreachable!("a pointer offset is an integer")
    };
    let bytes = element_size(context, element, span)?
        .checked_mul(index)
        .and_then(|bytes| i64::try_from(bytes).ok());
    let constant = match (rvalue(context, pointer, span)?, bytes) {
        (_, None) => return Err(hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ())),
        (HMIRImmediate::Int(0), Some(0)) => HMIRImmediate::Int(0),
        (HMIRImmediate::Address(address), Some(bytes)) if address.base().is_storage() => {
            let offset = match subtract {
                true => address.offset().checked_sub(bytes),
                false => address.offset().checked_add(bytes),
            };
            let Some(offset) = offset else {
                return Err(hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()));
            };
            HMIRImmediate::Address(HMIRAddress::new(address.base().clone(), offset))
        }
        (HMIRImmediate::Int(0), Some(_)) => {
            let operation = "pointer arithmetic".into();
            return Err(hmir_error(
                span,
                &mir::COMPTIME_UNDEFINED_ARITHMETIC,
                operation,
            ));
        }
        _ => return unsupported(span, "arithmetic on this address".into()),
    };

    Ok(HMIRValue::new(pointer.ty(), constant))
}

pub fn difference(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let size = element_size(context, element, span)?;
    let bytes = match (rvalue(context, lhs, span)?, rvalue(context, rhs, span)?) {
        (HMIRImmediate::Int(0), HMIRImmediate::Int(0)) => 0,
        (HMIRImmediate::Address(lhs), HMIRImmediate::Address(rhs))
            if lhs.base() == rhs.base() && lhs.base().is_storage() =>
        {
            lhs.offset() as i128 - rhs.offset() as i128
        }
        (HMIRImmediate::Address(lhs), HMIRImmediate::Address(rhs))
            if lhs.base().is_storage() && rhs.base().is_storage() =>
        {
            let operation = "pointer difference across allocations".into();
            return Err(hmir_error(
                span,
                &mir::COMPTIME_UNDEFINED_ARITHMETIC,
                operation,
            ));
        }
        _ => return unsupported(span, "the difference of these addresses".into()),
    };
    if size == 0 || bytes % size != 0 {
        let operation = "pointer difference".into();
        return Err(hmir_error(
            span,
            &mir::COMPTIME_UNDEFINED_ARITHMETIC,
            operation,
        ));
    }

    Ok(integer(context, bytes / size, result))
}
