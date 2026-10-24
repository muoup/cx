use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRPattern,
    expr::HMIRExprID,
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, constructor, eval, eval_type,
        flow::{Evaluation, ready},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_is(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: HMIRExprID,
    pattern: &HMIRPattern,
) -> EvalResult<HMIRValue> {
    let HMIRPattern::Variant { sum, name, inner } = pattern else {
        let feature = "an 'is' test against a pattern other than a variant".into();
        return Err(hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature).into());
    };

    let subject = ready!(eval(context, body, value));
    let ty = context
        .env()
        .reference_inner(subject.ty())
        .unwrap_or(subject.ty());
    if context.env().pointer_inner(ty).is_some() {
        return Err(hmir_error(span, &typecheck::POINTER_PATTERN, ()).into());
    }
    if !matches!(
        context.env().resolve_kind(ty),
        HMIRTypeKind::TaggedUnion { .. }
    ) {
        let kind = (types::name(context.env(), ty), "a tagged union".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    }
    if let Some(sum) = sum {
        let qualifier = ready!(eval_type(context, body, *sum)?.description());
        if context.env().underlying(qualifier) != context.env().underlying(ty) {
            let mismatch = (
                "variant pattern".into(),
                format!("'{}'", types::name(context.env(), ty)),
                format!("'{}'", types::name(context.env(), qualifier)),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
        }
    }

    let Some((variant, payload)) = constructor::variant(context.env(), ty, name) else {
        let unknown = (types::name(context.env(), ty), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_VARIANT, unknown).into());
    };
    let void = matches!(context.env().resolve_kind(payload), HMIRTypeKind::Void);
    match inner.as_deref() {
        None if !void => {
            let missing = (name.to_string(), types::name(context.env(), ty));
            return Err(hmir_error(span, &typecheck::MATCH_PAYLOAD_BINDING, missing).into());
        }
        Some(_) if void => {
            let subject = format!(
                "the pattern of void variant '{}'",
                constructor::name(context.env(), ty, variant)
            );
            let invalid = (subject, "payload".into());
            return Err(hmir_error(span, &typecheck::INVALID_FORM, invalid).into());
        }
        None | Some(HMIRPattern::Wildcard) => {}
        Some(_) => {
            let feature = "binding or matching a variant payload in an 'is' test".into();
            return Err(hmir_error(span, &typecheck::UNSUPPORTED_FEATURE, feature).into());
        }
    }

    let tag = emit::tag(context, &subject, span)?;
    let index = HMIRValue::constant(
        tag.ty(),
        HMIRConstant::Int {
            value: variant as i128,
            ty: tag.ty(),
        },
    );
    let result = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);

    Ok(Evaluation::Ready(emit::binary(
        context,
        HMIRBinaryOp::Eq,
        &tag,
        &index,
        result,
        span,
    )?))
}
