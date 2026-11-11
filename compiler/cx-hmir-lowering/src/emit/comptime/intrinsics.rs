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
        comptime::{ScalarFault, float, int, storage},
        unsupported,
    },
    env::function::{
        LocalLoweringContext,
        memory::{field_offset, layout, placement},
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

fn int_value(value: &HMIRValue, span: &TokenRange) -> CXResult<i128> {
    match value::as_int(value) {
        Some(value) => Ok(value),
        None => unsupported(span, "integer arithmetic on this value"),
    }
}

fn float_value(context: &LocalLoweringContext, value: &HMIRValue, span: &TokenRange) -> CXResult<f64> {
    let width = types::float_type(context.env(), value.ty()).width();
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Float(value)) => Ok(float::round(f64::from(value), width)),
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
            let (lhs, rhs) = (int_value(lhs, span)?, int_value(rhs, span)?);
            int::binary(*op, lhs, rhs, ty)
                .map(|value| value::int(context.env(), value, result))
                .map_err(|error| fault(span, error, op.path()))
        }
        IntIntrinsic::Unary { op, value } => {
            let ty = types::int_type(context.env(), value.ty());
            let value = int_value(value, span)?;
            Ok(value::int(context.env(), int::unary(*op, value, ty), result))
        }
        IntIntrinsic::ToFloat {
            value,
            target,
            signed,
        } => {
            let ty = types::int_type(context.env(), value.ty());
            let width = types::float_type(context.env(), *target).width();
            let value = int_value(value, span)?;
            Ok(value::float(context.env(), int::to_float(value, ty, *signed, width), result))
        }
        IntIntrinsic::Cast {
            value, sign_extend, ..
        } => {
            let bits = int::bits(types::int_type(context.env(), value.ty()));
            let value = int_value(value, span)?;
            Ok(value::int(context.env(), int::extend(value, bits, *sign_extend), result))
        }
        IntIntrinsic::ToPtr { value, .. } => match int_value(value, span)? {
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
        PointerIntrinsic::ToInt { ptr, .. } => match value::as_int(ptr) {
            Some(0) => Ok(value::int(context.env(), 0, result)),
            _ => unsupported(span, "converting this address to an integer"),
        },
        PointerIntrinsic::Offset { op, ptr, offset } => {
            let bytes = int_value(offset, span)?;
            let bytes = match op {
                PointerOffsetOp::Add => Some(bytes),
                PointerOffsetOp::Sub => bytes.checked_neg(),
            };
            let bytes = bytes.and_then(|bytes| i64::try_from(bytes).ok()).ok_or_else(|| overflow(span))?;
            let address = ptr.kind();
            let kind = match address {
                HMIRValueKind::Immediate(HMIRImmediate::Int(0)) if bytes == 0 => address.clone(),
                HMIRValueKind::Immediate(HMIRImmediate::Int(0)) => {
                    return Err(undefined(span, "pointer arithmetic"));
                }
                address if arithmetic(context, address) => {
                    shifted(address, bytes).ok_or_else(|| overflow(span))?
                }
                _ => return unsupported(span, "arithmetic on this address"),
            };
            Ok(HMIRValue::new(result, kind))
        }
        PointerIntrinsic::Diff { lhs, rhs, element } => {
            let size = layout(context.env_mut(), *element, span)?.size() as i128;
            let (lhs, rhs) = (lhs.kind(), rhs.kind());
            let bytes = match distance(lhs, rhs) {
                Some(bytes) => bytes as i128,
                None if arithmetic(context, lhs) && arithmetic(context, rhs) => {
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
            let (lhs, rhs) = (lhs.kind(), rhs.kind());
            let truth = match (op, distance(lhs, rhs)) {
                (PointerCompareOp::Eq, _) => lhs == rhs,
                (PointerCompareOp::Neq, _) => lhs != rhs,
                (op, Some(bytes)) => compare(*op, bytes.cmp(&0)),
                (_, None) => return Err(undefined(span, "ordering addresses across allocations")),
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
            let variant = storage::variant(context, value, span)?;
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
            let offset = int_value(index, span)?
                .checked_mul(size)
                .and_then(|offset| usize::try_from(offset).ok())
                .ok_or_else(|| overflow(span))?;
            project(context, base, offset, result, span)
        }
        AggregateIntrinsic::Init { ty, fields } => init(context, *ty, fields, span),
    }
}

// An initialized aggregate is constructed in storage of its own, untracked as it binds nothing: its zero value with
// the variant of a union selected, then each member written over it
fn init(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let zero = |context: &LocalLoweringContext, ty| {
        types::zero(context.env(), ty).expect("an initialized aggregate has a zero value")
    };
    let mut constant = zero(context, ty);
    if let (HMIRImmediate::Union { variant, payload }, Some((index, _))) = (&mut constant, fields.last()) {
        (*variant, **payload) = (*index, zero(context, member(context, ty, *index)));
    }
    let storage = HMIRAddress::new(context.allocate(ty, span)?, 0);
    context.store(&storage, ty, &constant.into(), None, span)?;
    for (index, value) in fields {
        let offset = field_offset(context.env_mut(), ty, *index, span)?;
        let offset = i64::try_from(offset).map_err(|_| overflow(span))?;
        let bitfield = placement(context.env_mut(), ty, *index, span)?;
        let member = member(context, ty, *index);
        let reference = types::reference(context.env_mut(), member, bitfield, span);
        let address = storage.offset_by(offset).ok_or_else(|| overflow(span))?;
        storage::write(context, &HMIRValue::new(reference, address), value, span)?;
    }

    Ok(HMIRValue::new(ty, storage))
}

fn member(context: &LocalLoweringContext, ty: HMIRTypeID, index: usize) -> HMIRTypeID {
    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Array { element, .. } => *element,
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => *fields[index].ty(),
        _ => unreachable!("an initialized aggregate has members"),
    }
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
                true => Ok(value.clone().with_ty(result)),
                false => unsupported(span, "this conversion"),
            }
        }
        InternalIntrinsic::Assert { condition, message } => match int_value(condition, span)? {
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
