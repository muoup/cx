use cx_hmir::{
    HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use super::{Coercion, compatible, is_char, is_designator, promotion, qualified, truth};
use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
        eval::{expr, types},
    log::hmir_error,
    symbol::composite::signatures_alike,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(super) fn convert(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CXResult<Coercion> {
    let env = context.env();
    let source = value.ty();
    if let (
        HMIRTypeKind::Array { element: from, length: Some(length) },
        HMIRTypeKind::Array { element: to, length: None },
    ) = (env.resolve_kind(source), env.resolve_kind(target))
        && env.same_type(*from, *to)
        && env.semantics(source).move_semantics == env.semantics(target).move_semantics
    {
        let length = *length;
        let ty = types::array(context.env_mut(), target, length, span);
        return Ok(Coercion::Applied(value.with_ty(ty)));
    }
    let unreachable = |ty| matches!(env.resolve_kind(ty), HMIRTypeKind::Unreachable);
    let staged_unreachable = matches!(
        (env.resolve_kind(source), env.resolve_kind(target)),
        (HMIRTypeKind::Staged(inner), HMIRTypeKind::Staged(_)) if unreachable(*inner)
    );
    if unreachable(source) || staged_unreachable || (compatible(env, source, target) && qualified(env, source, target)) {
        return Ok(Coercion::Applied(value.with_ty(target)));
    }
    match step(context, value, target, span, explicit)? {
        Coercion::Applied(value) => convert(context, value, target, span, explicit),
        unapplied => Ok(unapplied),
    }
}

fn step(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CXResult<Coercion> {
    let env = context.env();
    let source = value.ty();
    let (from, to) = (env.resolve_kind(source).clone(), env.resolve_kind(target).clone());
    match (env.resolve_kind(env.referent(source)), &to) {
        (HMIRTypeKind::Str | HMIRTypeKind::Function(_), HMIRTypeKind::PointerTo(_)) => {
            return promotion::step(context, value, span);
        }
        _ => {}
    }

    match (&from, &to) {
        (HMIRTypeKind::ReferenceTo { inner, .. }, HMIRTypeKind::Array { element: to, length: target_length }) => {
            let HMIRTypeKind::Array { element: from, length: Some(length) } = *env.resolve_kind(*inner) else {
                return Ok(Coercion::Unapplied(value));
            };
            if !env.same_type(from, *to)
                || target_length.is_some_and(|target| target != length)
                || env.is_nocopy(*inner)
                || env.semantics(*inner).move_semantics != env.semantics(target).move_semantics
            {
                return Ok(Coercion::Unapplied(value));
            }
            let ty = match target_length {
                Some(_) => target,
                None => types::array(context.env_mut(), target, length, span),
            };
            return Ok(Coercion::Applied(emit::copy(context, value, span)?.with_ty(ty)));
        }
        (HMIRTypeKind::ReferenceTo { inner, .. }, HMIRTypeKind::ReferenceTo { .. }) if env.is_reference(*inner) => {
            return promotion::step(context, value, span);
        }
        (
            HMIRTypeKind::ReferenceTo { inner: from, bitfield: Some(_) },
            HMIRTypeKind::ReferenceTo { inner: to, bitfield: None },
        ) if compatible(env, *from, *to) => {
            let usage = "bind a reference to".into();
            return Err(hmir_error(span, &typecheck::BITFIELD_REFERENCE, usage).into());
        }
        (HMIRTypeKind::ReferenceTo { .. }, HMIRTypeKind::ReferenceTo { .. }) => {
            return Ok(Coercion::Unapplied(value));
        }
        (HMIRTypeKind::ReferenceTo { .. }, _)
        | (HMIRTypeKind::Array { .. }, HMIRTypeKind::PointerTo(_)) => return promotion::step(context, value, span),
        (HMIRTypeKind::Staged(_), target) if !matches!(target, HMIRTypeKind::Staged(_)) => {
            return promotion::step(context, value, span);
        }
        (_, HMIRTypeKind::ReferenceTo { inner, .. }) => return bind(context, value, *inner, target, span),
        (HMIRTypeKind::Str, HMIRTypeKind::Array { element, length }) => {
            return string(context, value, target, *element, *length, span);
        }
        _ => {}
    }

    if env.semantics(source).move_semantics != env.semantics(target).move_semantics {
        return Ok(Coercion::Unapplied(value));
    }
    Ok(match (from, to) {
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_))
            if matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Int(0))) =>
        {
            Coercion::Applied(HMIRValue::constant(target, HMIRConstant::Null))
        }
        (HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(to))
            if to.width() == HMIRIntWidth::I1 =>
        {
            truth(context, value, span)?.map(|value| value.with_ty(target))
        }
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) if from.width() == to.width() => {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_)) => {
            Coercion::Applied(emit::cast(context, &value, target, span)?)
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => {
            expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            Coercion::Applied(emit::cast(context, &value, target, span)?)
        }
        (HMIRTypeKind::PointerTo(from), HMIRTypeKind::PointerTo(to))
            if (env.is_void(from) || env.is_void(to)) && qualified(env, source, target) =>
        {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => discard(context, value, target, span, explicit)?,
        _ => Coercion::Unapplied(value),
    })
}

fn bind(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let env = context.env();
    if !compatible(env, value.ty(), inner) {
        return Ok(Coercion::Unapplied(value));
    }
    let storage = match value.kind() {
        _ if value.transparent() && env.semantics(value.ty()).constant && !env.semantics(inner).constant => {
            return Ok(Coercion::Unapplied(value));
        }
        _ if value.transparent() => value,
        HMIRValueKind::Indirect(_) if env.semantics(value.ty()).constant && !env.semantics(inner).constant => {
            return Ok(Coercion::Unapplied(value));
        }
        HMIRValueKind::Indirect(_) => value,
        _ if is_designator(env, value.ty()) => value,
        _ => emit::temporary(context, value, span)?,
    };
    Ok(Coercion::Applied(emit::address(context, &storage, target, span)?))
}

fn string(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    element: HMIRTypeID,
    length: Option<u64>,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let HMIRValueKind::Constant(HMIRConstant::Str(string)) = value.kind() else {
        return Ok(Coercion::Unapplied(value));
    };
    if !is_char(context.env(), element) {
        return Ok(Coercion::Unapplied(value));
    }
    let bound = length.unwrap_or(string.len() as u64 + 1);
    if string.len() as u64 > bound {
        let limit = ("string".into(), usize::try_from(bound).ok());
        return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit).into());
    }
    let mut items = vec![HMIRConstant::Int(0); bound as usize];
    for (item, byte) in items.iter_mut().zip(string.bytes()) {
        *item = HMIRConstant::Int(byte as i128);
    }
    let ty = match length {
        None => types::array(context.env_mut(), target, bound, span),
        Some(_) => target,
    };
    Ok(Coercion::Applied(HMIRValue::constant(ty, HMIRConstant::Aggregate(items))))
}

fn discard(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CXResult<Coercion> {
    fn signature<'a>(env: &'a HMIREnvironment, ty: HMIRTypeID) -> Option<&'a HMIRFnSignature<HMIRTypeID>> {
        match env.resolve_kind(env.pointer_inner(ty)?) {
            HMIRTypeKind::Function(signature) => Some(signature),
            _ => None,
        }
    }

    let env = context.env();
    let (Some(from), Some(to)) = (signature(env, value.ty()), signature(env, target)) else {
        return Ok(Coercion::Unapplied(value));
    };
    let result = *from.return_type();
    if !env.is_void(*to.return_type()) || env.is_void(result) {
        return Ok(Coercion::Unapplied(value));
    }
    let discarding = HMIRFnSignature::new(
        from.stage(),
        from.params().to_vec(),
        *to.return_type(),
        from.is_variadic(),
        from.contract().clone(),
    );
    if !signatures_alike(env, &discarding, to) {
        return Ok(Coercion::Unapplied(value));
    }
    if env.is_nodrop(result) {
        let names = (types::name(env, value.ty()), types::name(env, target));
        return Err(hmir_error(span, &typecheck::DISCARDED_NODROP, names).into());
    }
    if explicit && !(compatible(env, value.ty(), target) && qualified(env, value.ty(), target)) {
        expr::require_nonsafe(context, span, "Unsafe type conversion")?;
    }
    Ok(Coercion::Applied(value.with_ty(target)))
}
