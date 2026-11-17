mod arguments;
pub(crate) mod pattern;

pub(crate) use super::flow::{Incomplete, IncompleteReason};
pub(crate) use super::subject::{detached, placeholder, shadowed, subject, typed};
pub(crate) use arguments::{
    Deduction, Pending, blocking, generated, generates_type, infer, undeduced,
};

use chl_hmir::{
    HMIRBodyContext,
    expr::HMIRExprID,
    ty::{HMIRExprType, HMIRTypeID, HMIRTypeKind, HMIRUndefined, context::HMIRTypeContext},
};
use chl_log::catalogue::typecheck;
use chl_tokens::TokenRange;

use crate::{
    env::{function::LocalLoweringContext, resolver::demand},
    eval::{EvalResult, coerce, flow::Evaluation, name::type_value, types},
    log::hmir_error,
    value::{self, HMIRValue, immediate::HMIRImmValue},
};

pub(crate) fn type_result(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = type_value(context, ty, span)?;
    Ok(match array_bound(context, ty, span) {
        Some(bound) => Evaluation::Incomplete(
            bound.project(move |ty| HMIRValue::new(value.ty(), HMIRImmValue::Type(ty))),
        ),
        None => Evaluation::Ready(value),
    })
}

fn array_bound(
    context: &LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> Option<Incomplete<HMIRTypeID>> {
    let HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) = *context.env().resolve_kind(ty)
    else {
        return None;
    };
    let bound = span.clone();
    Some(Incomplete::new(
        context,
        span,
        IncompleteReason::ArrayBound,
        Some(ty),
        move |context, _, expected| {
            let span = &bound;
            let env = context.env();
            let HMIRTypeKind::Array {
                element: from,
                length,
            } = *env.resolve_kind(expected)
            else {
                return Err(
                    hmir_error(span, &typecheck::CANNOT_INFER, "the array bound".into()).into(),
                );
            };
            if !env.same_type(from, element) {
                let requirement = (
                    "array initializer".into(),
                    types::name(env, ty),
                    Some(types::name(env, expected)),
                );
                return Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, requirement).into());
            }
            Ok(Evaluation::Ready(types::array(
                context.env_mut(),
                ty,
                length,
                span,
            )))
        },
    ))
}

// Unlike 'T[]', a bound of '_' must be deduced, so its undefined array only guides what completes it
pub(crate) fn hole_bound(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: &HMIRExprType,
    element: HMIRExprID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let undefined = types::undefined_array(context, body, ty, element)?;
    let universe = type_value(context, undefined, span)?.ty();
    let pending =
        array_bound(context, undefined, span).expect("an undefined array has a bound to infer");
    Ok(Evaluation::Incomplete(pending.without_fallback().project(
        move |ty| HMIRValue::new(universe, HMIRImmValue::Type(ty)),
    )))
}

pub(crate) fn eval_hole(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let hole = span.clone();
    let incomplete = Incomplete::new(
        context,
        span,
        IncompleteReason::ExpectedType,
        None,
        move |context, _, expected| Ok(Evaluation::Ready(type_constant(context, expected, &hole))),
    );
    Ok(Evaluation::Incomplete(incomplete))
}

pub(super) fn type_constant(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRValue {
    let universe = types::universe(context.env_mut(), span);
    HMIRValue::new(universe, HMIRImmValue::Type(ty))
}

// Completes an evaluation from the type expected of it, unless a type is expected and the evaluation describes one
pub(crate) fn complete(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    evaluation: Evaluation<HMIRValue>,
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    match expected {
        Some(expected) if matches!(context.env().resolve_kind(expected), HMIRTypeKind::Type) => {
            Ok(evaluation)
        }
        _ => evaluation.try_complete(context, body, expected),
    }
}

// The type a value describes, completed from the type expected of it where it is an unbounded array
pub(crate) fn described_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    value: Evaluation<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let span = span.clone();
    value.chain(
        context,
        body,
        None,
        value::as_type,
        move |context, _, value, _| {
            let value = coerce::promotion::std_rvalue(context, value, &span)?;
            let Some(ty) = value::as_type(&value) else {
                let found = format!(
                    "expression of type '{}'",
                    types::name(context.env(), value.ty())
                );
                return Err(hmir_error(&span, &typecheck::EXPECTED_TYPE, found).into());
            };
            demand::prepare(context.env_mut(), ty)?;
            Ok(match array_bound(context, ty, &span) {
                Some(bound) => Evaluation::Incomplete(bound),
                None => Evaluation::Ready(ty),
            })
        },
    )
}
