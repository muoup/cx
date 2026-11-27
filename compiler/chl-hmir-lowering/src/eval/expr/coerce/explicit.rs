use chl_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use chl_log::CHLResult;
use chl_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        coerce::{self, Coercion, applied, implicit, is_char, promotion},
        expr,
    },
    value::{self, HMIRValue, HMIRValueKind, immediate::HMIRImmValue},
};

pub(crate) fn standard(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let source = value.ty();
    if nominal_reference(context.env(), source, target) {
        return Ok(value.with_ty(target));
    }
    let coercion = match context.env().is_void(target) {
        true => Coercion::Applied(value::unit(context.env()).with_ty(target)),
        false => implicit::convert(context, value, target, span, true)?
            .or_else(|value| extension(context, value, target, span))?,
    };
    applied(context, coercion, source, target, span)
}

fn nominal_reference(env: &HMIREnvironment, source: HMIRTypeID, target: HMIRTypeID) -> bool {
    let (
        HMIRTypeKind::ReferenceTo {
            inner: from,
            bitfield: None,
        },
        HMIRTypeKind::ReferenceTo {
            inner: to,
            bitfield: None,
        },
    ) = (env.resolve_kind(source), env.resolve_kind(target))
    else {
        return false;
    };
    preserves_semantics(env, source, target)
        && (unwraps_nominal(env, source, target)
            || (env.strong_identifier(source) == env.strong_identifier(target)
                && unwraps_nominal(env, *from, *to)
                && preserves_semantics(env, *from, *to)))
}

fn unwraps_nominal(env: &HMIREnvironment, source: HMIRTypeID, target: HMIRTypeID) -> bool {
    matches!(
        env.resolve_type_id(env.underlying(source)).kind(),
        HMIRTypeKind::Nominal { inner, strong_identifier: Some(_) }
            if env.underlying(*inner) == env.underlying(target)
    )
}

fn preserves_semantics(env: &HMIREnvironment, source: HMIRTypeID, target: HMIRTypeID) -> bool {
    let (from, to) = (env.semantics(source), env.semantics(target));
    from.move_semantics == to.move_semantics && from.apply(to) == to
}

fn extension(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<Coercion> {
    let subject = context.env().referent(value.ty());
    let array = context.env().is_array(subject);
    let value = match array || context.env().pointer_inner(subject).is_some() {
        true => promotion::std_rvalue(context, value, span)?,
        false => value,
    };
    let env = context.env();
    let source = value.ty();
    if env.semantics(source).move_semantics != env.semantics(target).move_semantics {
        return Ok(Coercion::Unapplied(value));
    }
    Ok(match (env.resolve_kind(source), env.resolve_kind(target)) {
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => {
            expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::PointerTo(char), HMIRTypeKind::ReferenceTo { inner: string, .. })
            if env.is_str(*string) && is_char(env, *char) =>
        {
            let operation = if array {
                "Unsafe type conversion"
            } else {
                "Dereferencing a pointer"
            };
            expr::require_nonsafe(context, span, operation)?;
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_)) => {
            if !matches!(
                value.kind(),
                HMIRValueKind::Immediate(HMIRImmValue::Int(0))
            ) {
                expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            }
            Coercion::Applied(coerce::cast(context, &value, target, span)?)
        }
        _ => Coercion::Unapplied(value),
    })
}
