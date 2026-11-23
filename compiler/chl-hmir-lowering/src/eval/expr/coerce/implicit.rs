use chl_hmir::{
    HMIRFnSignature, HMIRFunctionStage,
    ty::{
        HMIRTypeID, HMIRTypeKind, HMIRUndefined, context::HMIRTypeContext, intrinsic::HMIRIntWidth,
    },
    unit::function::HMIRFnParam,
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_tokens::TokenRange;

use super::{Coercion, compatible, is_char, promotion, qualified, truth};
use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        coerce::{self, applied},
        comptime, expr, flow, staged,
        types::{self, signatures_alike},
    },
    log::hmir_error,
    value::{self, HMIRApplication, HMIRValue, HMIRValueKind, immediate::HMIRImmValue},
};

pub(crate) fn standard(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let source = value.ty();
    let coercion = convert(context, value, target, span, false)?;
    applied(context, coercion, source, target, span)
}

pub(super) fn convert(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CHLResult<Coercion> {
    let env = context.env();
    let source = value.ty();
    let same_identity = env.strong_identifier(source) == env.strong_identifier(target);
    if let (
        HMIRTypeKind::Array {
            element: from,
            length,
        },
        HMIRTypeKind::Undefined(HMIRUndefined::Array { element: to }),
    ) = (env.resolve_kind(source), env.resolve_kind(target))
        && same_identity
        && env.same_type(*from, *to)
        && env.semantics(source).move_semantics == env.semantics(target).move_semantics
    {
        let length = *length;
        let ty = types::array(context.env_mut(), target, length, span);
        return Ok(Coercion::Applied(value.with_ty(ty)));
    }
    let unreachable = |ty| matches!(env.resolve_kind(ty), HMIRTypeKind::Unreachable);
    let staged_unreachable = same_identity && matches!(
        (env.resolve_kind(source), env.resolve_kind(target)),
        (HMIRTypeKind::Staged(inner), HMIRTypeKind::Staged(_)) if unreachable(*inner)
    );
    if unreachable(source)
        || staged_unreachable
        || (compatible(env, source, target) && qualified(env, source, target))
    {
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
) -> CHLResult<Coercion> {
    let env = context.env();
    let source = value.ty();
    let source_identity = env.strong_identifier(source);
    let target_identity = env.strong_identifier(target);
    let carries_value = |ty| {
        matches!(env.resolve_kind(ty), HMIRTypeKind::Staged(_) | HMIRTypeKind::ReferenceTo { .. })
    };
    if source_identity != target_identity && !explicit
        && !(source_identity.is_none() && carries_value(source))
        && !(target_identity.is_none() && carries_value(target))
    {
        return Ok(Coercion::Unapplied(value));
    }
    if explicit && source_identity != target_identity
        && env.representation(source) == env.representation(target)
        && env.semantics(source).move_semantics == env.semantics(target).move_semantics
        && !env.is_reference(source)
    {
        return Ok(Coercion::Applied(value.with_ty(target)));
    }
    let (from, to) = (
        env.resolve_kind(source).clone(),
        env.resolve_kind(target).clone(),
    );
    match (env.resolve_kind(env.referent(source)), &to) {
        (HMIRTypeKind::Str(Some(length)), HMIRTypeKind::Array { element, length: bound }) => {
            return string(context, value, target, *element, *length, Some(*bound), span);
        }
        (HMIRTypeKind::Str(Some(length)), HMIRTypeKind::Undefined(HMIRUndefined::Array { element })) => {
            return string(context, value, target, *element, *length, None, span);
        }
        (HMIRTypeKind::Str(_), HMIRTypeKind::PointerTo(_)) => {
            return promotion::step(context, value, true, span);
        }
        (HMIRTypeKind::Function { .. }, HMIRTypeKind::PointerTo(_)) => {
            return promotion::function_pointer(context, value, span);
        }
        (
            HMIRTypeKind::Function {
                signature: from, ..
            },
            HMIRTypeKind::Staged(result),
        ) if from.stage() == HMIRFunctionStage::Runtime
            && from.params().is_empty()
            && (env.same_type(*from.return_type(), *result)
                || discards(env, *from.return_type(), *result)) =>
        {
            let from = from.clone();
            let to = HMIRFnSignature::new(
                HMIRFunctionStage::Comptime,
                Vec::new(),
                target,
                false,
                from.contract().clone(),
            );
            return eta(context, value, &from, &to, target, span);
        }
        (
            HMIRTypeKind::Function {
                signature: from, ..
            },
            HMIRTypeKind::Function { signature: to, .. },
        ) if from.stage() == HMIRFunctionStage::Runtime
            && to.stage() == HMIRFunctionStage::Comptime =>
        {
            let (from, to) = (from.clone(), to.clone());
            return eta(context, value, &from, &to, target, span);
        }
        (HMIRTypeKind::Function { .. }, HMIRTypeKind::Function { .. })
            if context.is_comptime() && source != env.referent(source) =>
        {
            let function = env.referent(source);
            return promotion::function(context, value, function, span).map(Coercion::Applied);
        }
        _ => {}
    }

    match (&from, &to) {
        (_, HMIRTypeKind::Staged(inner))
            if !context.is_runtime() && !staged::is_code(context, source) =>
        {
            return lift(context, value, *inner, target, span, explicit);
        }
        (
            HMIRTypeKind::ReferenceTo { inner, .. },
            HMIRTypeKind::Array { element, .. }
            | HMIRTypeKind::Undefined(HMIRUndefined::Array { element }),
        ) => {
            let HMIRTypeKind::Array {
                element: from,
                length,
            } = *env.resolve_kind(*inner)
            else {
                return if env.is_reference(*inner) {
                    promotion::step(context, value, false, span)
                } else {
                    Ok(Coercion::Unapplied(value))
                };
            };
            let target_length = match &to {
                HMIRTypeKind::Array { length, .. } => Some(*length),
                _ => None,
            };
            if !env.same_type(from, *element)
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
            return Ok(Coercion::Applied(
                emit::storage::read(context, &value, span)?.with_ty(ty),
            ));
        }
        (HMIRTypeKind::ReferenceTo { inner, .. }, HMIRTypeKind::ReferenceTo { .. })
            if env.is_reference(*inner) =>
        {
            return promotion::step(context, value, false, span);
        }
        (
            HMIRTypeKind::ReferenceTo {
                inner: from,
                bitfield: Some(_),
            },
            HMIRTypeKind::ReferenceTo {
                inner: to,
                bitfield: None,
            },
        ) if compatible(env, *from, *to) => {
            let usage = "bind a reference to".into();
            return Err(hmir_error(span, &typecheck::BITFIELD_REFERENCE, usage));
        }
        (HMIRTypeKind::ReferenceTo { .. }, HMIRTypeKind::ReferenceTo { .. }) => {
            return Ok(Coercion::Unapplied(value));
        }
        (HMIRTypeKind::ReferenceTo { .. }, _)
        | (
            HMIRTypeKind::Array { .. } | HMIRTypeKind::Undefined(HMIRUndefined::Array { .. }),
            HMIRTypeKind::PointerTo(_),
        ) => {
            let decay_comptime_arrays = matches!(to, HMIRTypeKind::PointerTo(_));
            return promotion::step(context, value, decay_comptime_arrays, span);
        }
        (HMIRTypeKind::Staged(_), _) if !staged::is_code(context, target) => {
            return match context.is_comptime() {
                true => {
                    let value = flow::top(staged::materialize(context, value, span))?;
                    Ok(Coercion::Applied(value))
                }
                false => promotion::step(context, value, false, span),
            };
        }
        (_, HMIRTypeKind::ReferenceTo { inner, .. }) => {
            return bind(context, value, *inner, target, span);
        }
        _ => {}
    }

    if env.semantics(source).move_semantics != env.semantics(target).move_semantics {
        return Ok(Coercion::Unapplied(value));
    }
    Ok(match (from, to) {
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_))
            if matches!(value.kind(), HMIRValueKind::Immediate(HMIRImmValue::Int(0))) =>
        {
            Coercion::Applied(HMIRValue::new(target, HMIRImmValue::Int(0)))
        }
        (
            HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_),
            HMIRTypeKind::Int(to),
        ) if to.width() == HMIRIntWidth::I1 => {
            truth(context, value, span)?.map(|value| value.with_ty(target))
        }
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) if from.width() == to.width() => {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_)) => {
            Coercion::Applied(coerce::cast(context, &value, target, span)?)
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => {
            expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            Coercion::Applied(coerce::cast(context, &value, target, span)?)
        }
        (HMIRTypeKind::PointerTo(from), HMIRTypeKind::PointerTo(to))
            if (env.is_void(from) || env.is_void(to)) && qualified(env, source, target) =>
        {
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => {
            discard(context, value, target, span, explicit)?
        }
        _ => Coercion::Unapplied(value),
    })
}

fn lift(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CHLResult<Coercion> {
    if types::comptime_only(context.env(), inner) {
        return Ok(Coercion::Unapplied(value));
    }
    let value = match convert(context, value, inner, span, explicit)? {
        Coercion::Applied(value) => comptime::converge(context, value, span)?,
        unapplied => return Ok(unapplied),
    };
    Ok(Coercion::Applied(value.with_ty(target)))
}

// A runtime function as a comptime function on code, which calls it where the code it yields is spliced. Each
// parameter is the same or lifted, and the result is lifted, or discarded for a void one
fn eta(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    from: &HMIRFnSignature<HMIRTypeID>,
    to: &HMIRFnSignature<HMIRTypeID>,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<Coercion> {
    let env = context.env();
    let HMIRTypeKind::Staged(result) = *env.resolve_kind(*to.return_type()) else {
        return Ok(Coercion::Unapplied(value));
    };
    if types::comptime_only(env, *from.return_type())
        || from.is_variadic()
        || from.params().len() != to.params().len()
        || from.params().iter().any(|param| param.comptime())
    {
        return Ok(Coercion::Unapplied(value));
    }
    let returned = *from.return_type();
    let discarding = discards(env, returned, result);
    let mut params = Vec::with_capacity(from.params().len());
    for (param, expected) in from.params().iter().zip(to.params()) {
        let ty = match context.env().resolve_kind(*expected.ty()) {
            HMIRTypeKind::Staged(_) => staged::lift(context.env_mut(), *param.ty(), span),
            _ => *param.ty(),
        };
        params.push(HMIRFnParam::new(false, ty));
    }
    let result = staged::lift(
        context.env_mut(),
        if discarding { result } else { returned },
        span,
    );
    let lifted = HMIRFnSignature::new(
        HMIRFunctionStage::Comptime,
        params,
        result,
        from.is_variadic(),
        from.contract().clone(),
    );
    if !signatures_alike(context.env(), &lifted, to) {
        return Ok(Coercion::Unapplied(value));
    }
    if discarding {
        discarded(context, returned, value.ty(), target, span)?;
    }
    let Some(callee) = comptime::known(context, &value) else {
        return Ok(Coercion::Unapplied(value));
    };
    let application = value::into_application(callee).unwrap_or_else(|callee| HMIRApplication {
        callee: Box::new(callee),
        args: vec![None; from.params().len()],
    });
    let lifted = value::application(target, application);
    Ok(Coercion::Applied(lifted))
}

fn bind(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<Coercion> {
    if !compatible(context.env(), value.ty(), inner) {
        let source = types::reference(context.env_mut(), value.ty(), None, span);
        if !compatible(context.env(), source, target) || !qualified(context.env(), source, target) {
            return Ok(Coercion::Unapplied(value));
        }
    }
    let storage = coerce::temporary(context, &value, span)?;
    Ok(Coercion::Applied(storage.with_ty(target)))
}

fn string(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    element: HMIRTypeID,
    extent: u64,
    length: Option<u64>,
    span: &TokenRange,
) -> CHLResult<Coercion> {
    if !is_char(context.env(), element) || context.env().strong_identifier(element).is_some() {
        return Ok(Coercion::Unapplied(value));
    }
    let bound = length.unwrap_or(extent);
    if extent - 1 > bound {
        let limit = ("string".into(), usize::try_from(bound).ok());
        return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit));
    }
    let ty = match length {
        None => types::array(context.env_mut(), target, bound, span),
        Some(_) => target,
    };
    let value = if context.env().is_reference(value.ty()) {
        value
    } else {
        coerce::temporary(context, &value, span)?
    };
    let copy_ty = types::realize(
        context.env_mut(),
        HMIRTypeKind::Array { element, length: bound.min(extent) },
        span,
    );
    let reference = types::reference(context.env_mut(), copy_ty, None, span);
    let copy = emit::storage::read(context, &value.with_ty(reference), span)?;
    if bound <= extent {
        return Ok(Coercion::Applied(copy.with_ty(ty)));
    }
    let empty = HMIRValue::new(ty, types::zero(context.env(), ty).expect("a character array has a zero value"));
    let storage = coerce::temporary(context, &empty, span)?;
    emit::storage::write(context, &storage.clone().with_ty(reference), &copy, span)?;
    Ok(Coercion::Applied(emit::storage::read(context, &storage, span)?))
}

fn discard(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    explicit: bool,
) -> CHLResult<Coercion> {
    fn signature<'a>(
        env: &'a HMIREnvironment,
        ty: HMIRTypeID,
    ) -> Option<&'a HMIRFnSignature<HMIRTypeID>> {
        match env.resolve_kind(env.pointer_inner(ty)?) {
            HMIRTypeKind::Function { signature, .. } => Some(signature),
            _ => None,
        }
    }

    let env = context.env();
    let (Some(from), Some(to)) = (signature(env, value.ty()), signature(env, target)) else {
        return Ok(Coercion::Unapplied(value));
    };
    let result = *from.return_type();
    if !discards(env, result, *to.return_type()) {
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
    discarded(context, result, value.ty(), target, span)?;
    let env = context.env();
    if explicit && !(compatible(env, value.ty(), target) && qualified(env, value.ty(), target)) {
        expr::require_nonsafe(context, span, "Unsafe type conversion")?;
    }
    Ok(Coercion::Applied(value.with_ty(target)))
}

// Whether a function returning 'from' may be taken as one returning 'to', which discards what it returns
fn discards(env: &HMIREnvironment, from: HMIRTypeID, to: HMIRTypeID) -> bool {
    env.is_void(to) && !env.is_void(from)
}

// A function of type 'source' taken as one of type 'target' discards its result, which must be one that drops
fn discarded(
    context: &LocalLoweringContext,
    result: HMIRTypeID,
    source: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<()> {
    let env = context.env();
    if !env.is_nodrop(result) {
        return Ok(());
    }
    let names = (types::name(env, source), types::name(env, target));
    Err(hmir_error(span, &typecheck::DISCARDED_NODROP, names))
}
