use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::{
    AggregateIntrinsic, FloatBinaryOp, FloatIntrinsic, IntBinaryOp, IntIntrinsic, IntUnaryOp,
    InternalIntrinsic, PointerCompareOp, PointerIntrinsic, PointerOffsetOp,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, intrinsics},
    env::function::{
        LocalLoweringContext,
        memory::{layout, placement},
    },
    eval::types,
    value::{HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_kind(ty).clone()
}

fn unit(context: &mut LocalLoweringContext, span: &TokenRange) -> HMIRTypeID {
    types::realize(context.env_mut(), HMIRTypeKind::Void, span)
}

fn int_op(op: HMIRBinaryOp, signed: bool) -> IntBinaryOp {
    match (op, signed) {
        (HMIRBinaryOp::Add, _) => IntBinaryOp::Add,
        (HMIRBinaryOp::Sub, _) => IntBinaryOp::Sub,
        (HMIRBinaryOp::Mul, true) => IntBinaryOp::SMul,
        (HMIRBinaryOp::Mul, false) => IntBinaryOp::UMul,
        (HMIRBinaryOp::Div, true) => IntBinaryOp::SDiv,
        (HMIRBinaryOp::Div, false) => IntBinaryOp::UDiv,
        (HMIRBinaryOp::Mod, true) => IntBinaryOp::SMod,
        (HMIRBinaryOp::Mod, false) => IntBinaryOp::UMod,
        (HMIRBinaryOp::Eq, _) => IntBinaryOp::Eq,
        (HMIRBinaryOp::Neq, _) => IntBinaryOp::Neq,
        (HMIRBinaryOp::Lt, true) => IntBinaryOp::SLt,
        (HMIRBinaryOp::Lt, false) => IntBinaryOp::ULt,
        (HMIRBinaryOp::Le, true) => IntBinaryOp::SLe,
        (HMIRBinaryOp::Le, false) => IntBinaryOp::ULe,
        (HMIRBinaryOp::Gt, true) => IntBinaryOp::SGt,
        (HMIRBinaryOp::Gt, false) => IntBinaryOp::UGt,
        (HMIRBinaryOp::Ge, true) => IntBinaryOp::SGe,
        (HMIRBinaryOp::Ge, false) => IntBinaryOp::UGe,
        (HMIRBinaryOp::BAnd, _) => IntBinaryOp::BAnd,
        (HMIRBinaryOp::BOr, _) => IntBinaryOp::BOr,
        (HMIRBinaryOp::BXor, _) => IntBinaryOp::BXor,
        (HMIRBinaryOp::LShift, _) => IntBinaryOp::LShift,
        (HMIRBinaryOp::RShift, true) => IntBinaryOp::ARShift,
        (HMIRBinaryOp::RShift, false) => IntBinaryOp::LRShift,
        (HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr, _) => {
            unreachable!("logical operators evaluate as short circuits")
        }
    }
}

fn float_op(op: HMIRBinaryOp) -> FloatBinaryOp {
    match op {
        HMIRBinaryOp::Add => FloatBinaryOp::Add,
        HMIRBinaryOp::Sub => FloatBinaryOp::Sub,
        HMIRBinaryOp::Mul => FloatBinaryOp::Mul,
        HMIRBinaryOp::Div => FloatBinaryOp::Div,
        HMIRBinaryOp::Eq => FloatBinaryOp::Eq,
        HMIRBinaryOp::Neq => FloatBinaryOp::Neq,
        HMIRBinaryOp::Lt => FloatBinaryOp::Lt,
        HMIRBinaryOp::Le => FloatBinaryOp::Le,
        HMIRBinaryOp::Gt => FloatBinaryOp::Gt,
        HMIRBinaryOp::Ge => FloatBinaryOp::Geq,
        _ => unreachable!("'{}' has no floating form", op.path()),
    }
}

fn pointer_op(op: HMIRBinaryOp) -> PointerCompareOp {
    match op {
        HMIRBinaryOp::Eq => PointerCompareOp::Eq,
        HMIRBinaryOp::Neq => PointerCompareOp::Neq,
        HMIRBinaryOp::Lt => PointerCompareOp::Lt,
        HMIRBinaryOp::Le => PointerCompareOp::Leq,
        HMIRBinaryOp::Gt => PointerCompareOp::Gt,
        HMIRBinaryOp::Ge => PointerCompareOp::Geq,
        _ => unreachable!("'{}' has no pointer form", op.path()),
    }
}

pub(crate) fn binary(
    context: &mut LocalLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (lhs, rhs) = (lhs.clone(), rhs.clone());
    match kind(context, lhs.ty()) {
        HMIRTypeKind::Int(int) => {
            let op = int_op(op, int.signed());
            intrinsics::int(context, &IntIntrinsic::Binary { op, lhs, rhs }, result, span)
        }
        HMIRTypeKind::Float(_) => {
            let op = float_op(op);
            intrinsics::float(context, &FloatIntrinsic::Binary { op, lhs, rhs }, result, span)
        }
        HMIRTypeKind::PointerTo(_) => {
            let op = pointer_op(op);
            intrinsics::pointer(context, &PointerIntrinsic::Compare { op, lhs, rhs }, result, span)
        }
        _ => unreachable!("'{}' applies to scalar operands", op.path()),
    }
}

pub(crate) fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (ty, value) = (value.ty(), value.clone());
    match (op, kind(context, ty)) {
        (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_)) => {
            intrinsics::float(context, &FloatIntrinsic::Neg { value }, ty, span)
        }
        (op, _) => {
            let op = match op {
                HMIRUnaryOp::Neg => IntUnaryOp::Neg,
                HMIRUnaryOp::LNot => IntUnaryOp::LNot,
                HMIRUnaryOp::BNot => IntUnaryOp::BNot,
                _ => unreachable!("'{}' is not a value operator", op.path()),
            };
            intrinsics::int(context, &IntIntrinsic::Unary { op, value }, ty, span)
        }
    }
}

pub(crate) fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = value.clone();
    match (kind(context, value.ty()), kind(context, target)) {
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(_)) => {
            let sign_extend = from.signed() && from.width() != HMIRIntWidth::I1;
            let cast = IntIntrinsic::Cast { value, target, sign_extend };
            intrinsics::int(context, &cast, target, span)
        }
        (HMIRTypeKind::Int(from), HMIRTypeKind::Float(_)) => {
            let cast = IntIntrinsic::ToFloat { value, target, signed: from.signed() };
            intrinsics::int(context, &cast, target, span)
        }
        (HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)) => {
            let cast = IntIntrinsic::ToPtr { value, sign_extend: from.signed() };
            intrinsics::int(context, &cast, target, span)
        }
        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(to)) => {
            let cast = FloatIntrinsic::ToInt { value, target, signed: to.signed() };
            intrinsics::float(context, &cast, target, span)
        }
        (HMIRTypeKind::Float(_), HMIRTypeKind::Float(_)) => {
            intrinsics::float(context, &FloatIntrinsic::Cast { value, target }, target, span)
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => {
            let cast = PointerIntrinsic::ToInt { ptr: value, target };
            intrinsics::pointer(context, &cast, target, span)
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => {
            let cast = InternalIntrinsic::Bitcast { value, target };
            intrinsics::internal(context, &cast, target, span)
        }
        _ => unreachable!("conversions are between scalar types"),
    }
}

pub(crate) fn offset(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = index.ty();
    let size = layout(context.env_mut(), element, span)?.size() as i128;
    let offset = match index.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Int(index)) => {
            HMIRValue::new(ty, HMIRImmediate::Int(index * size))
        }
        _ => {
            let size = HMIRValue::new(ty, HMIRImmediate::Int(size));
            binary(context, HMIRBinaryOp::Mul, index, &size, ty, span)?
        }
    };
    let op = match subtract {
        true => PointerOffsetOp::Sub,
        false => PointerOffsetOp::Add,
    };
    let ptr = pointer.clone();
    intrinsics::pointer(context, &PointerIntrinsic::Offset { op, ptr, offset }, pointer.ty(), span)
}

pub(crate) fn difference(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (lhs, rhs) = (lhs.clone(), rhs.clone());
    intrinsics::pointer(context, &PointerIntrinsic::Diff { lhs, rhs, element }, result, span)
}

// The member 'index' of the aggregate 'base' references, referenced as 'ty&'
pub(crate) fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let owner = context.env().referent(base.ty());
    let bitfield = placement(context.env_mut(), owner, index, span)?;
    let reference = types::reference(context.env_mut(), ty, bitfield, span);
    let base = base.clone();
    let projection = match kind(context, owner) {
        HMIRTypeKind::TaggedUnion { .. } => AggregateIntrinsic::SumVariant {
            value: base,
            variant: index,
            sum: owner,
        },
        _ => AggregateIntrinsic::StructField {
            base,
            field: index,
            aggregate: owner,
        },
    };
    intrinsics::aggregate(context, &projection, reference, span)
}

pub(crate) fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
    if let HMIRValueKind::Immediate(HMIRImmediate::TaggedUnion { variant, .. }) = value.kind() {
        return Ok(HMIRValue::new(ty, HMIRImmediate::Int(*variant as i128)));
    }
    let sum = context.env().referent(value.ty());
    let index = AggregateIntrinsic::SumIndex { value: value.clone(), sum };
    intrinsics::aggregate(context, &index, ty, span)
}

pub(crate) fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let init = AggregateIntrinsic::Init { ty, fields: fields.to_vec() };
    intrinsics::aggregate(context, &init, ty, span)
}

// A message makes a checked assertion, elided without assertions enabled; without one the condition is assumed
pub(crate) fn assert(
    context: &mut LocalLoweringContext,
    condition: &HMIRValue,
    message: Option<&str>,
    span: &TokenRange,
) -> CXResult<()> {
    if message.is_some() && !context.env().settings().assertions() {
        return Ok(());
    }
    let condition = condition.clone();
    let intrinsic = match message {
        Some(message) => InternalIntrinsic::Assert {
            condition,
            message: Some(message.into()),
        },
        None => InternalIntrinsic::Assume { condition },
    };
    let unit = unit(context, span);
    intrinsics::internal(context, &intrinsic, unit, span)?;

    Ok(())
}

pub(crate) fn null_check(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValuenormalize,
    span: &TokenRange,
) -> CXResult<()> {
    if !context.env().settings().assertions() {
        return Ok(());
    }
    let null = HMIRValue::new(pointer.ty(), HMIRImmediate::Int(0));
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let valid = binary(context, HMIRBinaryOp::Neq, pointer, &null, boolean, span)?;

    assert(context, &valid, Some("Attempted to dereference a null pointer"), span)
}

// A value given storage of its own, for an operation that needs an address
pub(crate) fn temporary(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let storage = emit::storage::scratch(context, value.ty(), span)?;
    emit::storage::initialize(context, &storage, Some(value), span)?;

    Ok(storage)
}
