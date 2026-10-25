mod reference;
mod string;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        coerce::{compatible, promotion, result::Coercion, truth},
        types,
    },
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub fn convert(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let env = context.env();
    let source = value.ty();
    if let (
        HMIRTypeKind::Array {
            element: from,
            length: Some(length),
        },
        HMIRTypeKind::Array {
            element: to,
            length: None,
        },
    ) = (env.resolve_kind(source), env.resolve_kind(target))
        && env.same_type(*from, *to)
        && env.semantics(source).move_semantics == env.semantics(target).move_semantics
    {
        let length = *length;
        let ty = types::array(context.env_mut(), target, length, span);
        return Ok(Coercion::Applied(value.with_ty(ty)));
    }
    if matches!(env.resolve_kind(source), HMIRTypeKind::Unreachable)
        || (compatible::compatible(env, source, target)
            && compatible::qualified(env, source, target))
    {
        return Ok(Coercion::Applied(value.with_ty(target)));
    }

    match step(context, value, target, span)? {
        Coercion::Applied(value) => convert(context, value, target, span),
        unapplied => Ok(unapplied),
    }
}

fn step(
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

    let designated = context.env().reference_inner(source).unwrap_or(source);
    match (context.env().resolve_kind(designated), &kinds.1) {
        (HMIRTypeKind::Str | HMIRTypeKind::Function(_), HMIRTypeKind::PointerTo(_)) => {
            return promotion::step(context, value, span);
        }
        _ => {}
    }

    match &kinds {
        (
            HMIRTypeKind::ReferenceTo(inner),
            HMIRTypeKind::Array {
                element: to,
                length: target_length,
            },
        ) => {
            let HMIRTypeKind::Array {
                element: from,
                length: Some(length),
            } = *context.env().resolve_kind(*inner)
            else {
                return Ok(Coercion::Unapplied(value));
            };
            if !context.env().same_type(from, *to)
                || target_length.is_some_and(|target| target != length)
                || context.env().is_nocopy(*inner)
                || context.env().semantics(*inner).move_semantics
                    != context.env().semantics(target).move_semantics
            {
                return Ok(Coercion::Unapplied(value));
            }
            let ty = match target_length {
                Some(_) => target,
                None => types::array(context.env_mut(), target, length, span),
            };
            return Ok(Coercion::Applied(
                emit::copy(context, value, span)?.with_ty(ty),
            ));
        }
        (HMIRTypeKind::ReferenceTo(inner), HMIRTypeKind::ReferenceTo(_))
            if context.env().is_reference(*inner) =>
        {
            return promotion::step(context, value, span);
        }
        (HMIRTypeKind::ReferenceTo(_), HMIRTypeKind::ReferenceTo(_)) => {
            return Ok(Coercion::Unapplied(value));
        }
        (HMIRTypeKind::ReferenceTo(_), _) => return promotion::step(context, value, span),

        (HMIRTypeKind::Staged(_), target) if !matches!(target, HMIRTypeKind::Staged(_)) => {
            return promotion::step(context, value, span);
        }
        (_, HMIRTypeKind::ReferenceTo(inner)) => {
            return reference::bind(context, value, *inner, target, span);
        }

        (HMIRTypeKind::Str, HMIRTypeKind::Array { .. }) => {
            return string::array(context, value, target, span);
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
            Coercion::Applied(HMIRValue::constant(target, HMIRConstant::Null))
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
            if (compatible::is_void(context.env(), from)
                || compatible::is_void(context.env(), to))
                && compatible::qualified(context.env(), source, target) =>
        {
            Coercion::Applied(value.with_ty(target))
        }

        _ => Coercion::Unapplied(value),
    })
}

fn is_zero(value: &HMIRValue) -> bool {
    matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Int(0)))
}
