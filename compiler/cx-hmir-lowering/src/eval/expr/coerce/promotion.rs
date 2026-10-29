use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind,
    aggregate::HMIRTypeSemantics,
    context::HMIRTypeContext,
    intrinsic::{HMIRFloatType, HMIRFloatWidth, HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use std::cmp::Ordering;

use super::Coercion;
use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        expr,
        flow, staged, types,
    },
    log::hmir_error,
    value::{HMIRPlace, HMIRValue, HMIRValueKind, address::Addressable, constant::HMIRConstant},
};

pub(super) fn step(context: &mut LocalLoweringContext, value: HMIRValue, span: &TokenRange) -> CXResult<Coercion> {
    let env = context.env();
    let subject = env.referent(value.ty());
    match env.resolve_kind(subject) {
        HMIRTypeKind::Staged(_) if context.is_runtime() && subject == value.ty() => {
            let value = match flow::top(flow::catch_diverged(staged::materialize(context, &value, &[])))? {
                Some(value) => value,
                None => {
                    let ty = types::realize(context.env_mut(), HMIRTypeKind::Unreachable, span);
                    expr::unit(context, span).with_ty(ty)
                }
            };
            Ok(Coercion::Applied(value))
        }
        HMIRTypeKind::Array { .. } => {
            let element = types::element(context, subject, span).expect("an array has an element type");
            let value = match context.env().is_reference(value.ty()) {
                true => value,
                false => emit::temporary(context, value, span)?,
            };
            address(context, value, element, span)
        }
        HMIRTypeKind::Str => {
            let semantics = env.semantics(subject);
            let qualifiers = HMIRTypeSemantics {
                constant: semantics.constant,
                volatile: semantics.volatile,
                ..Default::default()
            };
            let char = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
            let char = types::specify(context, char, qualifiers, span);
            address(context, value, char, span)
        }
        HMIRTypeKind::Function(_) if !types::comptime_only(env, subject) => address(context, value, subject, span),
        _ if subject != value.ty() => lvalue(context, value, subject, span),
        _ => Ok(Coercion::Unapplied(value)),
    }
}

fn address(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    pointee: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let storage = match context.env().reference_inner(value.ty()) {
        Some(inner) => HMIRValue::indirect(inner, value),
        None => value,
    };
    let pointer = types::pointer(context.env_mut(), pointee, span);
    Ok(Coercion::Applied(emit::address(context, &storage, pointer, span)?))
}

pub(super) fn lvalue(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let env = context.env();
    if env.is_nocopy(inner) {
        let names = (types::name(env, value.ty()), types::name(env, inner));
        return Err(hmir_error(span, &typecheck::INVALID_CONVERSION, names).into());
    }
    let semantics = env.semantics(inner);
    let global = match value.kind() {
        HMIRValueKind::PlaceRef(place) => matches!(place, HMIRPlace::Global(_)),
        HMIRValueKind::Constant(HMIRConstant::Address(address)) => {
            matches!(address.base(), Addressable::Place(HMIRPlace::Global(_)))
        }
        _ => false,
    };
    if semantics.volatile {
        expr::require_pure(context, span, "reading a volatile value")?;
    } else if global && !semantics.constant && context.is_runtime() {
        expr::require_pure(context, span, "reading a mutable global")?;
    }
    let qualifiers = HMIRTypeSemantics { constant: true, volatile: true, ..Default::default() };
    let ty = types::unspecify(context, inner, qualifiers, span);
    Ok(Coercion::Applied(emit::copy(context, value, span)?.with_ty(ty)))
}

pub(super) fn integer(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() < HMIRIntWidth::I32 => {
            types::int(context.env_mut(), HMIRIntWidth::I32, true, span)
        }
        _ => ty,
    }
}

// What an argument that no parameter gives a type to is passed as
pub(super) fn argument(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Float(float) if float.width() < HMIRFloatWidth::F64 => {
            let kind = HMIRTypeKind::Float(HMIRFloatType::new(HMIRFloatWidth::F64));
            types::realize(context.env_mut(), kind, span)
        }
        _ => integer(context, ty, span),
    }
}

pub(super) fn common(
    context: &mut LocalLoweringContext,
    lhs: HMIRTypeID,
    rhs: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let lhs = integer(context, lhs, span);
    let rhs = integer(context, rhs, span);
    let kinds = (context.env().resolve_kind(lhs).clone(), context.env().resolve_kind(rhs).clone());
    match kinds {
        (HMIRTypeKind::Float(left), HMIRTypeKind::Float(right)) => {
            let kind = HMIRTypeKind::Float(HMIRFloatType::new(left.width().max(right.width())));
            Some(types::realize(context.env_mut(), kind, span))
        }
        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_)) => Some(lhs),
        (HMIRTypeKind::Int(_), HMIRTypeKind::Float(_)) => Some(rhs),
        (HMIRTypeKind::Int(left), HMIRTypeKind::Int(right)) => {
            let (width, signed) = match left.width().cmp(&right.width()) {
                Ordering::Equal => (left.width(), left.signed() && right.signed()),
                Ordering::Greater => (left.width(), left.signed()),
                Ordering::Less => (right.width(), right.signed()),
            };
            Some(types::int(context.env_mut(), width, signed, span))
        }
        _ => None,
    }
}
