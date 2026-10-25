use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use super::rvalue;
use crate::{
    env::function::{LocalLoweringContext, memory::layout},
    log::hmir_error,
    value::{
        HMIRValue,
        address::Addressable,
        constant::HMIRConstant,
        scalar::{ScalarFault, float, int},
    },
};

fn integer(context: &LocalLoweringContext, value: i128, ty: HMIRTypeID) -> HMIRValue {
    let HMIRTypeKind::Int(int) = context.env().resolve_kind(ty) else {
        unreachable!("an integer constant has an integer type")
    };
    let value = int::normalize(value, *int);

    HMIRValue::constant(ty, HMIRConstant::Int(value))
}

fn floating(context: &LocalLoweringContext, value: f64, ty: HMIRTypeID) -> HMIRValue {
    let HMIRTypeKind::Float(float) = context.env().resolve_kind(ty) else {
        unreachable!("a floating constant has a floating type")
    };
    let value = FloatWrapper::from(float::round(value, float.width()));

    HMIRValue::constant(ty, HMIRConstant::Float(value))
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

fn compare(op: HMIRBinaryOp, lhs: &HMIRConstant, rhs: &HMIRConstant) -> Option<bool> {
    let ordering = match (lhs, rhs) {
        (HMIRConstant::Null, HMIRConstant::Null) => std::cmp::Ordering::Equal,
        (HMIRConstant::Address(lhs), HMIRConstant::Address(rhs)) if lhs.base() == rhs.base() => {
            lhs.offset().cmp(&rhs.offset())
        }
        (HMIRConstant::Null, HMIRConstant::Address(_) | HMIRConstant::Function { .. })
        | (HMIRConstant::Address(_) | HMIRConstant::Function { .. }, HMIRConstant::Null)
        | (HMIRConstant::Address(_), HMIRConstant::Address(_)) => {
            return match op {
                HMIRBinaryOp::Eq => Some(false),
                HMIRBinaryOp::Neq => Some(true),
                _ => None,
            };
        }
        _ => return None,
    };
    Some(match op {
        HMIRBinaryOp::Eq => ordering.is_eq(),
        HMIRBinaryOp::Neq => ordering.is_ne(),
        HMIRBinaryOp::Lt => ordering.is_lt(),
        HMIRBinaryOp::Le => ordering.is_le(),
        HMIRBinaryOp::Gt => ordering.is_gt(),
        HMIRBinaryOp::Ge => ordering.is_ge(),
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
    let kind = context.env().resolve_kind(lhs.ty()).clone();
    let operands = &kind;
    let constants = (rvalue(context, lhs, span)?, rvalue(context, rhs, span)?);

    match (operands, constants) {
        (HMIRTypeKind::Int(ty), (HMIRConstant::Int(lhs), HMIRConstant::Int(rhs))) => {
            if let Some(truth) = int::compare(op, lhs, rhs, *ty) {
                return Ok(integer(context, truth as i128, result));
            }

            int::arithmetic(op, lhs, rhs, *ty)
                .map(|value| integer(context, value, result))
                .map_err(|error| fault(span, error, op.path()))
        }
        (HMIRTypeKind::Float(ty), (HMIRConstant::Float(lhs), HMIRConstant::Float(rhs))) => {
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
        (HMIRTypeKind::Int(int), HMIRConstant::Int(value)) => {
            Ok(integer(context, int::unary(op, value, *int), ty))
        }
        (HMIRTypeKind::Float(_), HMIRConstant::Float(value)) if op == HMIRUnaryOp::Neg => {
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
        ((HMIRTypeKind::Int(from), HMIRTypeKind::Int(_)), HMIRConstant::Int(value)) => {
            Ok(integer(context, int::normalize(value, *from), target))
        }
        ((HMIRTypeKind::Int(from), HMIRTypeKind::Float(to)), HMIRConstant::Int(value)) => {
            let value = int::to_float(value, *from, to.width());
            Ok(floating(context, value, target))
        }
        ((HMIRTypeKind::Float(from), HMIRTypeKind::Int(to)), HMIRConstant::Float(value)) => {
            let value = float::round(f64::from(&value), from.width());
            float::to_int(value, *to)
                .map(|value| integer(context, value, target))
                .map_err(|error| fault(span, error, "conversion"))
        }
        ((HMIRTypeKind::Float(from), HMIRTypeKind::Float(_)), HMIRConstant::Float(value)) => {
            Ok(floating(
                context,
                float::round(f64::from(&value), from.width()),
                target,
            ))
        }

        ((HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)), HMIRConstant::Int(value))
            if int::normalize(value, *from) == 0 =>
        {
            Ok(HMIRValue::constant(target, HMIRConstant::Null))
        }
        ((HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)), HMIRConstant::Null) => {
            Ok(integer(context, 0, target))
        }
        ((HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)), HMIRConstant::Null) => {
            Ok(HMIRValue::constant(target, HMIRConstant::Null))
        }
        (
            (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)),
            address @ HMIRConstant::Address(_),
        ) => Ok(HMIRValue::constant(target, address)),

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
    let HMIRConstant::Int(index) = rvalue(context, index, span)? else {
        unreachable!("a pointer offset is an integer")
    };
    let bytes = element_size(context, element, span)?
        .checked_mul(index)
        .and_then(|bytes| i64::try_from(bytes).ok());
    let constant = match (rvalue(context, pointer, span)?, bytes) {
        (_, None) => return Err(hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ())),
        (HMIRConstant::Null, Some(0)) => HMIRConstant::Null,
        (HMIRConstant::Address(address), Some(bytes))
            if matches!(address.base(), Addressable::Place(_)) =>
        {
            let address = match subtract {
                true => address.checked_sub(bytes),
                false => address.checked_add(bytes),
            };
            let Some(address) = address else {
                return Err(hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()));
            };
            HMIRConstant::Address(address)
        }
        (HMIRConstant::Null, Some(_)) => {
            let operation = "pointer arithmetic".into();
            return Err(hmir_error(
                span,
                &mir::COMPTIME_UNDEFINED_ARITHMETIC,
                operation,
            ));
        }
        _ => return unsupported(span, "arithmetic on this address".into()),
    };

    Ok(HMIRValue::constant(pointer.ty(), constant))
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
        (HMIRConstant::Null, HMIRConstant::Null) => 0,
        (HMIRConstant::Address(lhs), HMIRConstant::Address(rhs))
            if lhs.base() == rhs.base() && matches!(lhs.base(), Addressable::Place(_)) =>
        {
            lhs.offset() as i128 - rhs.offset() as i128
        }
        (HMIRConstant::Address(lhs), HMIRConstant::Address(rhs))
            if matches!(
                (lhs.base(), rhs.base()),
                (Addressable::Place(_), Addressable::Place(_))
            ) =>
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
