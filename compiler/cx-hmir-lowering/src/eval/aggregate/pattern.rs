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

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, constructor, eval, eval_type,
        flow::{self, Evaluation, ready},
        op::specify,
        storage, types,
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
) -> CXResult<(usize, HMIRTypeID)> {
    let HMIRPattern::Variant { sum, name, inner } = pattern else {
        return Err(hmir_error(span, &typecheck::INVALID_PATTERN, ()));
    };
    if let Some(sum) = sum {
        let qualifier = flow::top(eval_type(context, body, *sum)?.description())?;
        if context.env().underlying(qualifier) != context.env().underlying(ty) {
            let mismatch = (
                "variant pattern".into(),
                format!("'{}'", types::name(context.env(), ty)),
                format!("'{}'", types::name(context.env(), qualifier)),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch));
        }
    }
    let Some((index, payload)) = constructor::variant(context.env(), ty, name) else {
        let unknown = (types::name(context.env(), ty), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_VARIANT, unknown));
    };
    let void = matches!(context.env().resolve_kind(payload), HMIRTypeKind::Void);
    match inner.as_deref() {
        None if !void => {
            let missing = (name.to_string(), types::name(context.env(), ty));
            return Err(hmir_error(span, &typecheck::MATCH_PAYLOAD_BINDING, missing));
        }
        Some(_) if void => {
            let subject = format!(
                "the pattern of void variant '{}'",
                constructor::name(context.env(), ty, index)
            );
            let invalid = (subject, "payload".into());
            return Err(hmir_error(span, &typecheck::INVALID_FORM, invalid));
        }
        Some(HMIRPattern::Binding(_) | HMIRPattern::Wildcard) | None => {}
        Some(_) => {
            let feature = "nested payload patterns".into();
            return Err(hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature));
        }
    }
    Ok((index, payload))
}

pub(crate) fn payload(
    context: &mut LocalLoweringContext,
    source: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let owner = context
        .env()
        .reference_inner(source.ty())
        .unwrap_or(source.ty());
    let semantics = context.env().semantics(owner);
    let ty = specify(
        context,
        ty,
        HMIRTypeSemantics {
            constant: semantics.constant,
            volatile: semantics.volatile,
            ..Default::default()
        },
        span,
    );
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
    let ty = context
        .env()
        .reference_inner(source.ty())
        .unwrap_or(source.ty());
    let local_info = body.local(local);
    let reference = match body.expr(local_info.ty()).kind() {
        HMIRExprKind::Type(declared) => match declared.kind() {
            HMIRExprTypeKind::ReferenceTo(inner) => Some(
                matches!(body.expr(*inner).kind(), HMIRExprKind::Type(inner) if inner.semantics().constant),
            ),
            _ => None,
        },
        _ => None,
    };
    if !owned && reference.is_none() && (context.env().is_nodrop(ty) || context.env().is_nocopy(ty))
    {
        return Err(hmir_error(
            span,
            &typecheck::BINDING_BEHIND_REFERENCE,
            local_info
                .name()
                .map(ToString::to_string)
                .unwrap_or_else(|| "this binding".into()),
        ));
    }
    let reference = reference.or((!owned).then_some(false));
    let (value, binding_ty) = match reference {
        Some(constant) => {
            let target = if constant {
                specify(
                    context,
                    ty,
                    HMIRTypeSemantics {
                        constant: true,
                        ..Default::default()
                    },
                    span,
                )
            } else {
                ty
            };
            let target = types::reference(context.env_mut(), target, span);
            (coerce::implicit(context, source, target, span)?, target)
        }
        None => {
            let value = if owned {
                emit::copy(context, source, span)?
            } else {
                coerce::implicit(context, source, ty, span)?
            };
            let ty = coerce::value_type(context, value.ty(), span);
            (coerce::implicit(context, value, ty, span)?, ty)
        }
    };
    let destination = emit::allocate(context, binding_ty, local_info.name().cloned(), span)?;
    let binding = types::reference(context.env_mut(), binding_ty, span);
    let destination = destination.with_ty(binding);
    storage::initialize(context, &destination, &value, span)?;
    emit::storage::bind_local(context, local, destination);
    Ok(reference.is_none())
}

pub(crate) fn eval_is(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: HMIRExprID,
    pattern: &HMIRPattern,
) -> EvalResult<HMIRValue> {
    if !matches!(pattern, HMIRPattern::Variant { .. }) {
        let feature = "an 'is' test against a pattern other than a variant".into();
        return Err(hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature).into());
    }
    let subject = ready!(eval(context, body, value));
    let ty = context
        .env()
        .reference_inner(subject.ty())
        .unwrap_or(subject.ty());
    if context.env().pointer_inner(ty).is_some() {
        return Err(hmir_error(span, &typecheck::POINTER_PATTERN, ()));
    }
    if !matches!(
        context.env().resolve_kind(ty),
        HMIRTypeKind::TaggedUnion { .. }
    ) {
        let kind = (types::name(context.env(), ty), "a tagged union".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    }
    let (index, payload) = variant(context, body, ty, pattern, span)?;
    let tag = emit::tag(context, &subject, span)?;
    let expected = HMIRValue::constant(tag.ty(), HMIRConstant::Int(index as i128));
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let result = emit::binary(context, HMIRBinaryOp::Eq, &tag, &expected, boolean, span)?;
    if let HMIRPattern::Variant {
        inner: Some(inner), ..
    } = pattern
        && let HMIRPattern::Binding(local) = inner.as_ref()
    {
        let owned = !context.env().is_reference(subject.ty());
        let subject = if owned {
            storage::temporary(context, subject, span)?
        } else {
            subject
        };
        let value = self::payload(context, &subject, index, payload, span)?;
        bind(context, body, *local, value, owned, span)?;
    }
    Ok(Evaluation::Ready(result))
}
