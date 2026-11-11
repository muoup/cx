use std::cmp::Ordering;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_intrinsics::{
    AggregateIntrinsic, FloatIntrinsic, IntIntrinsic, InternalIntrinsic, PointerCompareOp,
    PointerIntrinsic, PointerOffsetOp, VAIntrinsic,
};
use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_tokens::TokenRange;

use crate::{
    emit::{
        comptime::{self, ScalarFault, float, int, storage},
        unsupported,
    },
    env::function::{
        LocalLoweringContext,
        memory::{field_offset, layout},
    },
    eval::types,
    log::hmir_error,
    lower::declaration,
    value::{
        self, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmediate,
    },
};

fn fault(span: &TokenRange, fault: ScalarFault, operation: &str) -> CXError {
    match fault {
        ScalarFault::ZeroDivisor => hmir_error(span, &mir::COMPTIME_ZERO_DIVISOR, operation.into()),
        ScalarFault::Undefined => {
            hmir_error(span, &mir::COMPTIME_UNDEFINED_ARITHMETIC, operation.into())
        }
    }
}

fn undefined(span: &TokenRange, operation: &str) -> CXError {
    fault(span, ScalarFault::Undefined, operation)
}

fn overflow(span: &TokenRange) -> CXError {
    hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ())
}

fn int_value(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) -> CXResult<i128> {
    match comptime::immediate(context, value, span)? {
        Some(HMIRImmediate::Int(value)) => Ok(value),
        _ => unsupported(span, "integer arithmetic on this value"),
    }
}

fn float_value(context: &mut LocalLoweringContext, value: &HMIRValue, span: &TokenRange) -> CXResult<f64> {
    let width = types::float_type(context.env(), value.ty()).width();
    match comptime::immediate(context, value, span)? {
        Some(HMIRImmediate::Float(value)) => Ok(float::round(f64::from(&value), width)),
        _ => unsupported(span, "floating arithmetic on this value"),
    }
}

pub fn int(
    context: &mut LocalLoweringContext,
    intrinsic: &IntIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        IntIntrinsic::Binary { op, lhs, rhs } => {
            let ty = types::int_type(context.env(), lhs.ty());
            let (lhs, rhs) = (int_value(context, lhs, span)?, int_value(context, rhs, span)?);
            int::binary(*op, lhs, rhs, ty)
                .map(|value| value::int(context.env(), value, result))
                .map_err(|error| fault(span, error, op.path()))
        }
        IntIntrinsic::Unary { op, value } => {
            let ty = types::int_type(context.env(), value.ty());
            let value = int_value(context, value, span)?;
            Ok(value::int(context.env(), int::unary(*op, value, ty), result))
        }
        IntIntrinsic::ToFloat {
            value,
            target,
            signed,
        } => {
            let ty = types::int_type(context.env(), value.ty());
            let width = types::float_type(context.env(), *target).width();
            let value = int_value(context, value, span)?;
            Ok(value::float(context.env(), int::to_float(value, ty, *signed, width), result))
        }
        IntIntrinsic::Cast {
            value, sign_extend, ..
        } => {
            let bits = int::bits(types::int_type(context.env(), value.ty()));
            let value = int_value(context, value, span)?;
            Ok(value::int(context.env(), int::extend(value, bits, *sign_extend), result))
        }
        IntIntrinsic::ToPtr { value, .. } => match int_value(context, value, span)? {
            0 => Ok(HMIRValue::new(result, HMIRImmediate::Int(0))),
            _ => unsupported(span, "converting this integer to an address"),
        },
    }
}

pub fn float(
    context: &mut LocalLoweringContext,
    intrinsic: &FloatIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        FloatIntrinsic::Binary { op, lhs, rhs } => {
            let width = types::float_type(context.env(), lhs.ty()).width();
            let (lhs, rhs) = (float_value(context, lhs, span)?, float_value(context, rhs, span)?);
            Ok(match float::compare(*op, lhs, rhs) {
                Some(truth) => value::int(context.env(), truth as i128, result),
                None => value::float(context.env(), float::arithmetic(*op, lhs, rhs, width), result),
            })
        }
        FloatIntrinsic::Neg { value } => {
            let value = float_value(context, value, span)?;
            Ok(value::float(context.env(), -value, result))
        }
        FloatIntrinsic::ToInt { value, target, signed } => {
            let ty = types::int_type(context.env(), *target);
            let value = float_value(context, value, span)?;
            float::to_int(value, ty, *signed)
                .map(|value| value::int(context.env(), value, result))
                .map_err(|error| fault(span, error, "conversion"))
        }
        FloatIntrinsic::Cast { value, .. } => {
            let value = float_value(context, value, span)?;
            Ok(value::float(context.env(), value, result))
        }
    }
}

// Whether an address designates storage that pointer arithmetic may move within
fn arithmetic(context: &LocalLoweringContext, address: &HMIRValueKind) -> bool {
    match address {
        HMIRValueKind::Reference(_) => true,
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) => match address.base() {
            Addressable::Def(def) => {
                !declaration::is_function(context.env().symbols().resolve_definition(*def))
            }
            Addressable::Literal(_) => true,
            Addressable::Label { .. } => false,
        },
        _ => false,
    }
}

fn shifted(address: &HMIRValueKind, bytes: i64) -> Option<HMIRValueKind> {
    Some(match address {
        HMIRValueKind::Reference(address) => address.offset_by(bytes)?.into(),
        HMIRValueKind::Immediate(HMIRImmediate::Address(address)) => {
            HMIRImmediate::Address(address.offset_by(bytes)?).into()
        }
        _ => unreachable!("only an address is shifted"),
    })
}

// The distance in bytes between two addresses into the same storage
fn distance(lhs: &HMIRValueKind, rhs: &HMIRValueKind) -> Option<i64> {
    fn within<B: PartialEq>(lhs: &HMIRAddress<B>, rhs: &HMIRAddress<B>) -> Option<i64> {
        (lhs.base() == rhs.base()).then(|| lhs.offset() - rhs.offset())
    }
    match (lhs, rhs) {
        (HMIRValueKind::Immediate(HMIRImmediate::Int(0)), HMIRValueKind::Immediate(HMIRImmediate::Int(0))) => {
            Some(0)
        }
        (HMIRValueKind::Reference(lhs), HMIRValueKind::Reference(rhs)) => within(lhs, rhs),
        (
            HMIRValueKind::Immediate(HMIRImmediate::Address(lhs)),
            HMIRValueKind::Immediate(HMIRImmediate::Address(rhs)),
        ) => within(lhs, rhs),
        _ => None,
    }
}

// Offsets are counted in bytes
pub fn pointer(
    context: &mut LocalLoweringContext,
    intrinsic: &PointerIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        PointerIntrinsic::ToInt { ptr, .. } => match comptime::immediate(context, ptr, span)? {
            Some(HMIRImmediate::Int(0)) => Ok(value::int(context.env(), 0, result)),
            _ => unsupported(span, "converting this address to an integer"),
        },
        PointerIntrinsic::Offset { op, ptr, offset } => {
            let bytes = int_value(context, offset, span)?;
            let bytes = match op {
                PointerOffsetOp::Add => Some(bytes),
                PointerOffsetOp::Sub => bytes.checked_neg(),
            };
            let bytes = bytes.and_then(|bytes| i64::try_from(bytes).ok()).ok_or_else(|| overflow(span))?;
            let address = comptime::resolve(context, ptr, span)?;
            let kind = match address {
                HMIRValueKind::Immediate(HMIRImmediate::Int(0)) if bytes == 0 => address,
                HMIRValueKind::Immediate(HMIRImmediate::Int(0)) => {
                    return Err(undefined(span, "pointer arithmetic"));
                }
                ref address if arithmetic(context, address) => {
                    shifted(address, bytes).ok_or_else(|| overflow(span))?
                }
                _ => return unsupported(span, "arithmetic on this address"),
            };
            Ok(HMIRValue::new(result, kind))
        }
        PointerIntrinsic::Diff { lhs, rhs, element } => {
            let size = layout(context.env_mut(), *element, span)?.size() as i128;
            let (lhs, rhs) = (comptime::resolve(context, lhs, span)?, comptime::resolve(context, rhs, span)?);
            let bytes = match distance(&lhs, &rhs) {
                Some(bytes) => bytes as i128,
                None if arithmetic(context, &lhs) && arithmetic(context, &rhs) => {
                    return Err(undefined(span, "pointer difference across allocations"));
                }
                None => return unsupported(span, "the difference of these addresses"),
            };
            if size == 0 || bytes % size != 0 {
                return Err(undefined(span, "pointer difference"));
            }
            Ok(value::int(context.env(), bytes / size, result))
        }
        PointerIntrinsic::Compare { op, lhs, rhs } => {
            let (lhs, rhs) = (comptime::resolve(context, lhs, span)?, comptime::resolve(context, rhs, span)?);
            let null = |kind: &HMIRValueKind| matches!(kind, HMIRValueKind::Immediate(HMIRImmediate::Int(0)));
            let truth = match (distance(&lhs, &rhs).map(|bytes| bytes.cmp(&0)), op) {
                (Some(ordering), op) => compare(*op, ordering),
                (None, PointerCompareOp::Eq) if null(&lhs) != null(&rhs) => false,
                (None, PointerCompareOp::Neq) if null(&lhs) != null(&rhs) => true,
                (None, _) => return unsupported(span, "comparison of these addresses"),
            };
            Ok(value::int(context.env(), truth as i128, result))
        }
    }
}

fn compare(op: PointerCompareOp, ordering: Ordering) -> bool {
    match op {
        PointerCompareOp::Eq => ordering.is_eq(),
        PointerCompareOp::Neq => ordering.is_ne(),
        PointerCompareOp::Lt => ordering.is_lt(),
        PointerCompareOp::Leq => ordering.is_le(),
        PointerCompareOp::Gt => ordering.is_gt(),
        PointerCompareOp::Geq => ordering.is_ge(),
    }
}

// A projection is the address of the aggregate moved by the offset of the member; an aggregate without storage is
// spilled into a temporary of its own first
fn project(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    offset: usize,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let base = match context.env().is_reference(base.ty()) {
        true => base.clone(),
        false => {
            let temporary = storage::allocate(context, base.ty(), None, span)?;
            storage::write(context, &temporary, base, span)?;
            temporary
        }
    };
    let offset = i64::try_from(offset).map_err(|_| overflow(span))?;
    let address = shifted(base.kind(), offset).ok_or_else(|| overflow(span))?;

    Ok(HMIRValue::new(result, address))
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    intrinsic: &AggregateIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        AggregateIntrinsic::SumIndex { value, .. } => {
            let value = match context.env().is_reference(value.ty()) {
                true => storage::read(context, value, span)?,
                false => value.clone(),
            };
            let Some(HMIRImmediate::TaggedUnion { variant, .. }) = comptime::immediate(context, &value, span)? else {
                unreachable!("a tagged union constant names its variant")
            };
            Ok(value::int(context.env(), variant as i128, result))
        }
        AggregateIntrinsic::SumVariant { value, .. } => project(context, value, 0, result, span),
        AggregateIntrinsic::StructField {
            base,
            field,
            aggregate,
        } => {
            let offset = field_offset(context.env_mut(), *aggregate, *field, span)?;
            project(context, base, offset, result, span)
        }
        AggregateIntrinsic::ArrayIndex {
            base,
            index,
            element,
        } => {
            let size = layout(context.env_mut(), *element, span)?.size() as i128;
            let offset = int_value(context, index, span)?
                .checked_mul(size)
                .and_then(|offset| usize::try_from(offset).ok())
                .ok_or_else(|| overflow(span))?;
            project(context, base, offset, result, span)
        }
        AggregateIntrinsic::Init { ty, fields } => init(context, *ty, fields, span),
    }
}

fn init(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(mut constant) = types::zero(context.env(), ty) else {
        unreachable!("an initialized aggregate has a zero value")
    };
    let union = matches!(context.env().resolve_kind(ty), HMIRTypeKind::Union { .. });
    for (index, value) in fields {
        let Some(value) = comptime::immediate(context, value, span)? else {
            return unsupported(span, "an address of comptime storage inside an aggregate");
        };
        if union {
            constant = value;
            continue;
        }
        match &mut constant {
            HMIRImmediate::Aggregate { fields: items } => items[*index] = value,
            HMIRImmediate::TaggedUnion {
                variant: member,
                payload: slot,
            } => (*member, **slot) = (*index, value),
            _ => unreachable!("an initialized aggregate has an aggregate zero value"),
        }
    }

    Ok(HMIRValue::new(ty, constant))
}

pub fn internal(
    context: &mut LocalLoweringContext,
    intrinsic: &InternalIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        InternalIntrinsic::Bitcast { value, target } => {
            let pointers = context.env().is_pointer(value.ty()) && context.env().is_pointer(*target);
            match pointers {
                true => Ok(HMIRValue::new(result, comptime::resolve(context, value, span)?)),
                false => unsupported(span, "this conversion"),
            }
        }
        InternalIntrinsic::Assert { condition, message } => match int_value(context, condition, span)? {
            0 => Err(hmir_error(span, &mir::COMPTIME_ASSERTION, message.clone())),
            _ => Ok(HMIRValue::new(result, HMIRImmediate::Unit)),
        },
        InternalIntrinsic::Assume { .. } => Ok(HMIRValue::new(result, HMIRImmediate::Unit)),
    }
}

pub fn va(
    _: &mut LocalLoweringContext,
    intrinsic: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    _: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    unsupported(span, intrinsic.path())
}
