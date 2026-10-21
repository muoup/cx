mod compatible;
mod explicit;
mod implicit;
mod promotion;
mod result;
mod truth;

use cx_hmir::ty::HMIRTypeID;
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{promotion::arithmetic, result::Coercion},
        types,
    },
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn implicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    let coercion = implicit::convert(context, value, target, span)?;

    applied(context, coercion, source, target, span)
}

pub(crate) fn explicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    let coercion = explicit::convert(context, value, target, span)?;

    applied(context, coercion, source, target, span)
}

pub(crate) fn std_rvalue(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    promotion::std_rvalue(context, value, span)
}

pub(crate) fn arithmetic(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let target = arithmetic::integer(context, value.ty(), span);

    implicit(context, value, target, span)
}

pub(crate) fn default_argument(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let target = arithmetic::argument(context, value.ty(), span);

    implicit(context, value, target, span)
}

pub(crate) fn condition(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let source = value.ty();

    match truth::apply(context, value, span)? {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => Err(hmir_error(
            span,
            &typecheck::NO_TRUTH_VALUE,
            types::name(context.env(), source),
        )),
    }
}

pub(crate) fn common(
    context: &mut LocalLoweringContext,
    lhs: HMIRTypeID,
    rhs: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    arithmetic::common(context, lhs, rhs, span)
}

fn applied(
    context: &LocalLoweringContext,
    coercion: Coercion,
    source: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match coercion {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => Err(hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (
                types::name(context.env(), source),
                types::name(context.env(), target),
            ),
        )),
    }
}
