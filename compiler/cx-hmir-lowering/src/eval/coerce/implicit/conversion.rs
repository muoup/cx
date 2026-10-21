use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::coerce::{compatible::is_void, implicit::reference, promotion, result::Coercion, truth},
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let source = value.ty();
    let kinds = (
        context.env().resolve_kind(source).clone(),
        context.env().resolve_kind(target).clone(),
    );

    match &kinds {
        (HMIRTypeKind::ReferenceTo(inner), HMIRTypeKind::ReferenceTo(_))
            if context.env().is_reference(*inner) =>
        {
            return promotion::step(context, value, span);
        }
        (HMIRTypeKind::ReferenceTo(_), HMIRTypeKind::ReferenceTo(_)) => {
            return Ok(Coercion::Unapplied(value));
        }
        (HMIRTypeKind::ReferenceTo(_), _) => return promotion::step(context, value, span),
        (_, HMIRTypeKind::ReferenceTo(inner)) => {
            return reference::bind(context, value, *inner, target, span);
        }

        (HMIRTypeKind::Str, HMIRTypeKind::Array { .. }) => {
            todo!("conversion of a string to an array")
        }
        (HMIRTypeKind::Str, HMIRTypeKind::PointerTo(_)) => {
            let decayed = promotion::step(context, value, span)?;
            return Ok(decayed.map(|value| value.with_ty(target)));
        }
        (HMIRTypeKind::Function(_), HMIRTypeKind::PointerTo(inner))
            if context.env().same_type(source, *inner) =>
        {
            return promotion::step(context, value, span);
        }
        (HMIRTypeKind::Array { .. }, HMIRTypeKind::PointerTo(_)) => {
            return promotion::step(context, value, span);
        }

        _ => {}
    }

    if context.env().semantics(source).move_semantics
        != context.env().semantics(target).move_semantics
    {
        return Ok(Coercion::Unapplied(value));
    }

    Ok(match kinds {
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_)) if is_zero(&value) => {
            Coercion::Applied(HMIRValue::constant(target, HMIRConstant::Null(target)))
        }

        (
            HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_),
            HMIRTypeKind::Int(to),
        ) if to.width() == HMIRIntWidth::I1 => {
            truth::apply(context, value, span)?.map(|value| value.with_ty(target))
        }

        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) if from.width() == to.width() => {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => {
            Coercion::Applied(emit::cast(context, &value, target, span)?)
        }

        (HMIRTypeKind::PointerTo(from), HMIRTypeKind::PointerTo(to))
            if is_void(context.env(), from) || is_void(context.env(), to) =>
        {
            Coercion::Applied(value.with_ty(target))
        }

        _ => Coercion::Unapplied(value),
    })
}

fn is_zero(value: &HMIRValue) -> bool {
    matches!(
        value.kind(),
        HMIRValueKind::Constant(HMIRConstant::Int { value: 0, .. })
    )
}
