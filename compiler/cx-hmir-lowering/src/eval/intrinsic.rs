use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRIntrinsic},
    ty::context::HMIRTypeContext,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, eval, eval_type,
        flow::{Evaluation, ready},
        literal, name, types,
    },
    log::hmir_error,
    symbol::composite::alike,
    value::HMIRValue,
};

pub(crate) fn eval_intrinsic(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    intrinsic: &HMIRIntrinsic,
) -> EvalResult<HMIRValue> {
    let Intrinsic::VA(intrinsic) = intrinsic else {
        unreachable!("only variadic intrinsics are lowered to HMIR")
    };
    let intrinsic = match intrinsic {
        VAIntrinsic::Start { list, last } => {
            let list = ready!(va_list(context, body, *list));
            let last = ready!(eval(context, body, *last));
            let last = coerce::std_rvalue(context, last, span)?;
            VAIntrinsic::Start { list, last }
        }
        VAIntrinsic::End { list } => VAIntrinsic::End {
            list: ready!(va_list(context, body, *list)),
        },
        VAIntrinsic::Arg { list, ty } => VAIntrinsic::Arg {
            list: ready!(va_list(context, body, *list)),
            ty: ready!(eval_type(context, body, *ty)),
        },
    };
    let value = match emit::variadic(context, &intrinsic, span)? {
        Some(value) => value,
        None => literal::unit(context, span),
    };
    Ok(Evaluation::Ready(value))
}

fn va_list(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    list: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let span = body.expr(list).span();
    let value = ready!(eval(context, body, list));
    let expected = name::intrinsic_type(context, "__builtin_va_list", span)
        .expect("va_list is an intrinsic type");
    let actual = context.env().reference_inner(value.ty());
    if !actual.is_some_and(|actual| alike(context.env(), actual, expected, false)) {
        let mismatch = (
            "va_list expression".into(),
            types::name(context.env(), expected),
            types::name(context.env(), value.ty()),
        );
        return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
    }
    let ty = types::pointer(context.env_mut(), expected, span);
    Ok(Evaluation::Ready(emit::address(context, &value, ty, span)?))
}
