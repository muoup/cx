use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRPattern,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprTypeKind, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics,
        context::HMIRTypeContext, intrinsic::HMIRIntWidth,
    },
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use super::variant::{self, Variant};
use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, eval, eval_type,
        flow::{self, Evaluation},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn variant(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: HMIRTypeID,
    pattern: &HMIRPattern,
    span: &TokenRange,
) -> CXResult<Variant> {
    let HMIRPattern::Variant { sum, name, inner } = pattern else {
        return Err(hmir_error(span, &typecheck::INVALID_PATTERN, ()).into());
    };
    if let Some(sum) = sum {
        let qualifier = flow::top(eval_type(context, body, *sum).and_then(Evaluation::description))?;
        if context.env().underlying(qualifier) != context.env().underlying(ty) {
            let mismatch = (
                "variant pattern".into(),
                format!("'{}'", types::name(context.env(), ty)),
                format!("'{}'", types::name(context.env(), qualifier)),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
        }
    }
    let Some(found) = variant::lookup(context.env(), ty, name) else {
        let unknown = (types::name(context.env(), ty), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_VARIANT, unknown).into());
    };
    let void = context.env().is_void(found.payload);
    match inner.as_deref() {
        None if !void => {
            let missing = (name.to_string(), types::name(context.env(), ty));
            return Err(hmir_error(span, &typecheck::MATCH_PAYLOAD_BINDING, missing).into());
        }
        Some(HMIRPattern::Binding(_) | HMIRPattern::Wildcard) | None => {}
        Some(_) if void => {
            let name = variant::name(context.env(), ty, found.index);
            let invalid = (format!("the pattern of void variant '{name}'"), "payload".into());
            return Err(hmir_error(span, &typecheck::INVALID_FORM, invalid).into());
        }
        Some(_) => {
            let feature = "nested payload patterns".into();
            return Err(hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature).into());
        }
    }
    Ok(found)
}

pub(crate) fn payload(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let semantics = context.env().semantics(context.env().referent(source.ty()));
    let qualifiers = HMIRTypeSemantics {
        constant: semantics.constant,
        volatile: semantics.volatile,
        ..Default::default()
    };
    let ty = types::specify(context, ty, qualifiers, span);
    let value = emit::field(context, source, index, ty, span)?;
    emit::reference(context, &value, span)
}

pub(crate) fn bind(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    local: HMIRLocalID,
    source: HMIRValue,
    owned: bool,
    span: &TokenRange,
) -> CXResult<bool> {
    let ty = context.env().referent(source.ty());
    let local_info = body.local(local);
    let reference = match body.expr(local_info.ty()).kind() {
        HMIRExprKind::Type(declared) => match declared.kind() {
            HMIRExprTypeKind::ReferenceTo { inner, .. } => Some(
                matches!(body.expr(*inner).kind(), HMIRExprKind::Type(inner) if inner.semantics().constant),
            ),
            _ => None,
        },
        _ => None,
    };
    if !owned && reference.is_none() && (context.env().is_nodrop(ty) || context.env().is_nocopy(ty))
    {
        let name = local_info.name().map_or_else(|| "this binding".into(), ToString::to_string);
        return Err(hmir_error(span, &typecheck::BINDING_BEHIND_REFERENCE, name).into());
    }
    let (value, binding_ty) = match reference {
        Some(_) if context.env().is_reference(ty) => (coerce::implicit(context, source, ty, span)?, ty),
        Some(constant) => {
            let target = match constant {
                true => types::specify(context, ty, HMIRTypeSemantics { constant: true, ..Default::default() }, span),
                false => ty,
            };
            let target = types::reference(context.env_mut(), target, None, span);
            (coerce::implicit(context, source, target, span)?, target)
        }
        None => {
            let ty = coerce::unqualified(context, ty, span);
            let value = if owned {
                emit::copy(context, source, span)?
            } else {
                coerce::implicit(context, source, ty, span)?
            };
            (coerce::implicit(context, value, ty, span)?, ty)
        }
    };
    let binding = types::reference(context.env_mut(), binding_ty, None, span);
    let destination = emit::allocate(context, binding_ty, local_info.name().cloned(), span)?.with_ty(binding);
    emit::initialize(context, &destination, Some(&value), span)?;
    emit::bind_local(context, local, destination);
    Ok(reference.is_none())
}

pub(crate) fn eval_is(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: HMIRExprID,
    pattern: &HMIRPattern,
) -> EvalResult<Evaluation<HMIRValue>> {
    let HMIRPattern::Variant { inner, .. } = pattern else {
        let feature = "an 'is' test against a pattern other than a variant".into();
        return Err(hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature).into());
    };
    let subject = (eval(context, body, value))?.ready()?;
    let ty = context.env().referent(subject.ty());
    if context.env().pointer_inner(ty).is_some() {
        return Err(hmir_error(span, &typecheck::POINTER_PATTERN, ()).into());
    }
    if !matches!(context.env().resolve_kind(ty), HMIRTypeKind::TaggedUnion { .. }) {
        let kind = (types::name(context.env(), ty), "a tagged union".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    }
    let Variant { index, payload } = self::variant(context, body, ty, pattern, span)?;
    let tag = emit::tag(context, &subject, span)?;
    let expected = HMIRValue::constant(tag.ty(), HMIRConstant::Int(index as i128));
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let result = emit::binary(context, HMIRBinaryOp::Eq, &tag, &expected, boolean, span)?;
    if let Some(HMIRPattern::Binding(local)) = inner.as_deref() {
        let owned = !context.env().is_reference(subject.ty());
        let subject = match owned {
            true => emit::temporary(context, subject, span)?,
            false => subject,
        };
        let value = self::payload(context, &subject, index, payload, span)?;
        bind(context, body, *local, value, owned, span)?;
    }
    Ok(Evaluation::Ready(result))
}
