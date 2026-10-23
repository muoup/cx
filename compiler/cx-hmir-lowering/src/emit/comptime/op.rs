use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use super::read;
use crate::{
    env::function::LocalLoweringContext,
    log::hmir_error,
    value::{
        HMIRValue,
        constant::HMIRConstant,
        scalar::{ScalarFault, float, int},
    },
};

fn integer(context: &LocalLoweringContext, value: i128, ty: HMIRTypeID) -> HMIRValue {
    let HMIRTypeKind::Int(int) = context.env().resolve_kind(ty) else {
        unreachable!("an integer constant has an integer type")
    };
    let value = int::normalize(value, *int);

    HMIRValue::constant(ty, HMIRConstant::Int { value, ty })
}

fn floating(context: &LocalLoweringContext, value: f64, ty: HMIRTypeID) -> HMIRValue {
    let HMIRTypeKind::Float(float) = context.env().resolve_kind(ty) else {
        unreachable!("a floating constant has a floating type")
    };
    let value = FloatWrapper::from(float::round(value, float.width()));

    HMIRValue::constant(ty, HMIRConstant::Float { value, ty })
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

fn addresses_equal(lhs: &HMIRConstant, rhs: &HMIRConstant) -> Option<bool> {
    match (lhs, rhs) {
        (HMIRConstant::Null(_), HMIRConstant::Null(_)) => Some(true),
        (HMIRConstant::Null(_), HMIRConstant::Address { .. } | HMIRConstant::Function { .. })
        | (HMIRConstant::Address { .. } | HMIRConstant::Function { .. }, HMIRConstant::Null(_)) => {
            Some(false)
        }
        (
            HMIRConstant::Address {
                place: lhs,
                path: lhs_path,
                ..
            },
            HMIRConstant::Address {
                place: rhs,
                path: rhs_path,
                ..
            },
        ) => Some(lhs == rhs && lhs_path == rhs_path),
        _ => None,
    }
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
    let constants = (read(context, lhs, span)?, read(context, rhs, span)?);

    match (operands, constants) {
        (
            HMIRTypeKind::Int(ty),
            (HMIRConstant::Int { value: lhs, .. }, HMIRConstant::Int { value: rhs, .. }),
        ) => {
            if let Some(truth) = int::compare(op, lhs, rhs, *ty) {
                return Ok(integer(context, truth as i128, result));
            }

            int::arithmetic(op, lhs, rhs, *ty)
                .map(|value| integer(context, value, result))
                .map_err(|error| fault(span, error, op.path()))
        }
        (
            HMIRTypeKind::Float(ty),
            (HMIRConstant::Float { value: lhs, .. }, HMIRConstant::Float { value: rhs, .. }),
        ) => {
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
        (HMIRTypeKind::PointerTo(_), (lhs, rhs))
            if matches!(op, HMIRBinaryOp::Eq | HMIRBinaryOp::Neq) =>
        {
            let Some(equal) = addresses_equal(&lhs, &rhs) else {
                return unsupported(span, "comparison of these addresses".into());
            };

            Ok(integer(
                context,
                (equal == (op == HMIRBinaryOp::Eq)) as i128,
                result,
            ))
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
    match (&kind, read(context, value, span)?) {
        (HMIRTypeKind::Int(int), HMIRConstant::Int { value, .. }) => {
            Ok(integer(context, int::unary(op, value, *int), ty))
        }
        (HMIRTypeKind::Float(_), HMIRConstant::Float { value, .. }) if op == HMIRUnaryOp::Neg => {
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

    match (kinds, read(context, value, span)?) {
        ((HMIRTypeKind::Int(from), HMIRTypeKind::Int(_)), HMIRConstant::Int { value, .. }) => {
            Ok(integer(context, int::normalize(value, *from), target))
        }
        ((HMIRTypeKind::Int(from), HMIRTypeKind::Float(to)), HMIRConstant::Int { value, .. }) => {
            let value = int::to_float(value, *from, to.width());
            Ok(floating(context, value, target))
        }
        ((HMIRTypeKind::Float(from), HMIRTypeKind::Int(to)), HMIRConstant::Float { value, .. }) => {
            let value = float::round(f64::from(&value), from.width());
            float::to_int(value, *to)
                .map(|value| integer(context, value, target))
                .map_err(|error| fault(span, error, "conversion"))
        }
        (
            (HMIRTypeKind::Float(from), HMIRTypeKind::Float(_)),
            HMIRConstant::Float { value, .. },
        ) => Ok(floating(
            context,
            float::round(f64::from(&value), from.width()),
            target,
        )),

        (
            (HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)),
            HMIRConstant::Int { value, .. },
        ) if int::normalize(value, *from) == 0 => {
            Ok(HMIRValue::constant(target, HMIRConstant::Null(target)))
        }
        ((HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)), HMIRConstant::Null(_)) => {
            Ok(integer(context, 0, target))
        }
        ((HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)), HMIRConstant::Null(_)) => {
            Ok(HMIRValue::constant(target, HMIRConstant::Null(target)))
        }
        (
            (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)),
            HMIRConstant::Address { place, path, .. },
        ) => Ok(HMIRValue::constant(
            target,
            HMIRConstant::Address {
                place,
                path,
                ty: target,
            },
        )),

        _ => unsupported(span, "this conversion".into()),
    }
}
