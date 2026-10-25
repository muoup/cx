use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        coerce::{implicit, promotion, result::Coercion},
        literal,
    },
    value::HMIRValue,
};

pub fn convert(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    if matches!(context.env().resolve_kind(target), HMIRTypeKind::Void) {
        return Ok(Coercion::Applied(
            literal::unit(context, span).with_ty(target),
        ));
    }

    implicit::convert(context, value, target, span)?
        .or_else(|value| extension(context, value, target, span))
}

fn extension(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    // REVIEW: The logic here is just messy, we should refactor this into a grander match
    
    let subject = context
        .env()
        .reference_inner(value.ty())
        .unwrap_or(value.ty());
    let value = if matches!(
        context.env().resolve_kind(subject),
        HMIRTypeKind::PointerTo(_) | HMIRTypeKind::Array { .. }
    ) {
        promotion::std_rvalue(context, value, span)?
    } else {
        value
    };
    let source = value.ty();
    if context.env().semantics(source).move_semantics
        != context.env().semantics(target).move_semantics
    {
        return Ok(Coercion::Unapplied(value));
    }

    let kinds = (
        context.env().resolve_kind(source),
        context.env().resolve_kind(target),
    );
    Ok(match kinds {
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::PointerTo(char), HMIRTypeKind::ReferenceTo { inner: string, .. })
            if context.env().is_str(*string)
                && matches!(context.env().resolve_kind(*char), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8) =>
        {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_)) => {
            Coercion::Applied(emit::cast(context, &value, target, span)?)
        }
        _ => Coercion::Unapplied(value),
    })
}
