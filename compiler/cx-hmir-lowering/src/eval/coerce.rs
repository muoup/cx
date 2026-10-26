mod compatible;
mod explicit;
mod implicit;
mod promotion;
mod result;
mod truth;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext, intrinsic::HMIRIntWidth};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{promotion::arithmetic, result::Coercion},
        op::{specify, unspecify},
        safety::require_nonsafe,
        types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn value_type(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let ty = context.env().reference_inner(ty).unwrap_or(ty);
    let qualifiers = HMIRTypeSemantics {
        constant: true,
        volatile: true,
        ..Default::default()
    };
    unspecify(context, ty, qualifiers, span)
}

pub(crate) fn implicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    require_unsafe_conversion(context, &value, target, span, false)?;
    let source = value.ty();
    let coercion = implicit::convert(context, value, target, span)?;

    applied(context, coercion, source, target, span)
}

pub(crate) fn explicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    require_unsafe_conversion(context, &value, target, span, true)?;
    let source = value.ty();
    let coercion = explicit::convert(context, value, target, span)?;

    applied(context, coercion, source, target, span)
}

pub(crate) fn std_rvalue(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    promotion::std_rvalue(context, value, span)
}

pub(crate) fn arithmetic(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let target = arithmetic::integer(context, value.ty(), span);

    implicit(context, value, target, span)
}

pub(crate) fn default_argument(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let target = arithmetic::argument(context, value.ty(), span);

    implicit(context, value, target, span)
}

pub(crate) fn condition(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let source = value.ty();

    match truth::apply(context, value, span)? {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => Err(hmir_error(
            span,
            &typecheck::NO_TRUTH_VALUE,
            types::name(context.env(), source),
        )),
    }
}

pub(crate) fn common(
    context: &mut LocalLoweringContext,
    lhs: HMIRTypeID,
    rhs: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    arithmetic::common(context, lhs, rhs, span)
}

pub(crate) fn pointer(
    context: &mut LocalLoweringContext,
    left: HMIRTypeID,
    right: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env();
    let (pointee, other) = match matches!(env.resolve_kind(right), HMIRTypeKind::Void) {
        true => (right, left),
        false => (left, right),
    };
    let other = env.semantics(other);
    let qualifiers = HMIRTypeSemantics {
        constant: other.constant,
        volatile: other.volatile,
        ..Default::default()
    };
    let pointee = specify(context, pointee, qualifiers, span);
    types::pointer(context.env_mut(), pointee, span)
}

fn require_unsafe_conversion(
    context: &LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CXResult<()> {
    let source = context
        .env()
        .reference_inner(value.ty())
        .unwrap_or(value.ty());
    let from = context.env().resolve_kind(source);
    let to = context.env().resolve_kind(target);
    let operation = match (from, to) {
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(int)) if int.width() != HMIRIntWidth::I1 => {
            Some("Unsafe type conversion")
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_))
            if explicit
                && !matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Int(0))) =>
        {
            Some("Unsafe type conversion")
        }
        (HMIRTypeKind::PointerTo(pointee), HMIRTypeKind::ReferenceTo { inner, .. })
            if context.env().same_type(*pointee, *inner)
                || (explicit && context.env().is_str(*inner)
                    && matches!(context.env().resolve_kind(*pointee), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8)) =>
        {
            Some("Dereferencing a pointer")
        }
        (HMIRTypeKind::Array { element, .. }, HMIRTypeKind::ReferenceTo { inner, .. })
            if explicit && context.env().is_str(*inner)
                && matches!(context.env().resolve_kind(*element), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8) =>
        {
            Some("Unsafe type conversion")
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_))
            if explicit
                && (!compatible::compatible(context.env(), source, target)
                    || !compatible::qualified(context.env(), source, target)) =>
        {
            Some("Unsafe type conversion")
        }
        _ => None,
    };
    if let Some(operation) = operation {
        require_nonsafe(context, span, operation)?;
    }
    Ok(())
}

fn applied(
    context: &LocalLoweringContext,
    coercion: Coercion,
    source: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match coercion {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => Err(hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (
                types::name(context.env(), source),
                types::name(context.env(), target),
            ),
        )),
    }
}
