use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{Coercion, applied, implicit, is_char, promotion},
        expr, helpers,
    },
    value::{HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

pub(crate) fn standard(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    let coercion = match context.env().is_void(target) {
        true => Coercion::Applied(expr::unit(context, span).with_ty(target)),
        false => implicit::convert(context, value, target, span, true)?
            .or_else(|value| extension(context, value, target, span))?,
    };
    applied(context, coercion, source, target, span)
}

fn extension(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
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
            if !matches!(value.kind(), HMIRValueKind::Immediate(HMIRImmediate::Int(0))) {
                expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            }
            Coercion::Applied(helpers::cast(context, &value, target, span)?)
        }
        _ => Coercion::Unapplied(value),
    })
}
