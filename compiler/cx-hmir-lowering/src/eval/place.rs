use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, Transfer},
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, eval,
        op::{binary_operands, eval_operand},
        types,
    },
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval_assign(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    target: HMIRExprID,
    op: Option<HMIRBinaryOp>,
    value: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let target = eval(context, body, target)?;

    let Some(inner) = context.env().reference_inner(target.ty()) else {
        return Err(hmir_error(span, &typecheck::NOT_ADDRESSABLE, "assign to".into()).into());
    };
    if context.env().semantics(inner).constant {
        return Err(hmir_error(
            span,
            &typecheck::MUTATE_CONST,
            ("assign to".into(), types::name(context.env(), inner)),
        )
        .into());
    }

    let value = match op {
        Some(op) => {
            let current = coerce::arithmetic(context, target.clone(), span)?;
            let operand = eval_operand(context, body, span, value)?;

            binary_operands(context, span, op, current, operand)?
        }
        None => eval(context, body, value)?,
    };

    let value = coerce::implicit(context, value, inner, span)?;
    emit::transfer(context, &target, value, Transfer::Replace, span)?;

    Ok(target)
}

pub(crate) fn eval_address_of(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, operand)?;
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return match context.env().resolve_kind(value.ty()) {
            HMIRTypeKind::Function(_) => Ok(coerce::std_rvalue(context, value, span)?),
            _ => Err(hmir_error(
                span,
                &typecheck::NOT_ADDRESSABLE,
                "take the address of".into(),
            )
            .into()),
        };
    };
    if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Str) {
        todo!("address of a string")
    }

    let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::PointerTo(inner),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));

    Ok(emit::address(context, &value, ty, span)?)
}

pub(crate) fn eval_dereference(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    pointer: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, pointer)?;

    Ok(dereference(context, value, span)?)
}

pub(crate) fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = coerce::std_rvalue(context, pointer, span)?;
    let Some(inner) = context.env().pointer_inner(pointer.ty()) else {
        return Err(hmir_error(
            span,
            &typecheck::UNEXPECTED_KIND,
            (types::name(context.env(), pointer.ty()), "a pointer".into()),
        ));
    };
    if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Function(_)) {
        todo!("dereference of a function pointer")
    }

    let storage = emit::dereference(context, pointer, inner, span)?;
    emit::reference(context, &storage, span)
}
