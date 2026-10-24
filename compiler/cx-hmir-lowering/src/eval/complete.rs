use cx_hmir::{
    HMIRBody,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        flow::{Evaluation, Incomplete, IncompleteReason},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn type_result(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let value = super::def::type_value(context, ty, span)?;
    if !context.env().resolver().settled(ty) {
        context.force_type(ty, span)?;
    }
    if !types::unsized_array(context.env(), ty) {
        return Ok(Evaluation::Ready(value));
    }
    let span = span.clone();
    Ok(Evaluation::Incomplete(Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ArrayBound,
        Some(value),
        move |context, _, expected| {
            let env = context.env();
            let HMIRTypeKind::Array {
                element: from,
                length: Some(length),
            } = *env.resolve_kind(expected)
            else {
                return Err(hmir_error(
                    &span,
                    &typecheck::CANNOT_INFER,
                    "the array bound".into(),
                ));
            };
            let HMIRTypeKind::Array { element: to, .. } = *env.resolve_kind(ty) else {
                unreachable!("an incomplete array retains its constructor")
            };
            if !env.same_type(from, to) {
                return Err(hmir_error(
                    &span,
                    &typecheck::TYPE_REQUIREMENT,
                    (
                        "array initializer".into(),
                        types::name(env, ty),
                        Some(types::name(env, expected)),
                    ),
                ));
            }
            let ty = types::array(context.env_mut(), ty, length, &span);
            let universe = types::universe(context.env_mut(), &span);
            Ok(Evaluation::Ready(HMIRValue::constant(
                universe,
                HMIRConstant::Type(ty),
            )))
        },
    )))
}

pub(crate) fn hole(context: &mut LocalLoweringContext, span: &TokenRange) -> EvalResult<HMIRValue> {
    let span = span.clone();
    Ok(Evaluation::Incomplete(Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ExpectedType,
        None,
        move |context, _, expected| {
            let expected = context.env().reference_inner(expected).unwrap_or(expected);
            let universe = types::universe(context.env_mut(), &span);
            Ok(Evaluation::Ready(HMIRValue::constant(
                universe,
                HMIRConstant::Type(expected),
            )))
        },
    )))
}

pub(crate) fn value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    evaluation: Evaluation<HMIRValue>,
    expected: Option<HMIRTypeID>,
) -> EvalResult<HMIRValue> {
    match expected {
        Some(expected) if matches!(context.env().resolve_kind(expected), HMIRTypeKind::Type) => {
            Ok(evaluation)
        }
        _ => evaluation.complete(context, body, expected),
    }
}
