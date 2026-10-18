use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        aggregate::eval_initializer,
        coerce::{self, decay, implicit},
        eval,
        op::{binary_operands, eval_operand},
        types,
    },
    log::hmir_error,
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
    let target = emit::referent(context, target, span)?;
    if !emit::addressable(context, &target) {
        return Err(hmir_error(span, &typecheck::NOT_ADDRESSABLE, "assign to".into()).into());
    }
    if context
        .env()
        .resolve_type_id(target.ty())
        .semantics()
        .constant
    {
        return Err(hmir_error(
            span,
            &typecheck::MUTATE_CONST,
            ("assign to".into(), types::name(context.env(), target.ty())),
        )
        .into());
    }

    let value = match op {
        Some(op) => {
            let current = coerce::operand(context, target.clone(), span)?;
            let operand = eval_operand(context, body, span, value)?;

            binary_operands(context, span, op, current, operand)?
        }
        None => eval_initializer(context, body, value, target.ty())?,
    };

    let value = implicit(context, value, target.ty(), span)?;
    emit::write(context, &target, value, false, span)?;

    Ok(emit::reference(context, &target, span)?)
}

pub(crate) fn eval_address_of(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, operand)?;
    let value = emit::referent(context, value, span)?;
    match context.env().resolve_kind(value.ty()) {
        HMIRTypeKind::Function(_) => return Ok(decay(context, value, span)?),
        HMIRTypeKind::Str => todo!("address of a string"),
        _ => {}
    }

    let value = emit::spill(context, value, span)?;
    let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::PointerTo(value.ty()),
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
    let value = emit::referent(context, value, span)?;

    let value = dereference(context, value, span)?;
    Ok(emit::reference(context, &value, span)?)
}

pub(crate) fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = coerce::rvalue(context, pointer, span)?;
    let inner = match context.env().resolve_kind(pointer.ty()) {
        HMIRTypeKind::PointerTo(inner) => *inner,
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::UNEXPECTED_KIND,
                (types::name(context.env(), pointer.ty()), "a pointer".into()),
            ));
        }
    };
    if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Function(_)) {
        todo!("dereference of a function pointer")
    }

    emit::dereference(context, pointer, inner, span)
}
