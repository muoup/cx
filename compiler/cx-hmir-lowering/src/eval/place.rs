use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRType, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, eval,
        flow::{Evaluation, ready},
        op::{binary_operands, eval_operand},
        storage, types,
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
    let target = ready!(eval(context, body, target));

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
            let operand = ready!(eval_operand(context, body, span, value));

            binary_operands(context, span, op, current, operand)?
        }
        None => {
            let evaluated = eval(context, body, value)?;
            ready!(evaluated.complete(context, body, Some(inner)))
        }
    };

    let value = coerce::implicit(context, value, inner, span)?;
    emit::invalidate(context, &target, MIRInvalidationKind::Drop, span);
    storage::initialize(context, &target, &value, span)?;

    Ok(Evaluation::Ready(target))
}

pub(crate) fn eval_move(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, operand));
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return Ok(Evaluation::Ready(value));
    };

    let HMIRExprKind::Local(local) = body.expr(operand).kind() else {
        return Err(hmir_error(span, &typecheck::LOCAL_VARIABLE_REQUIRED, "move".into()).into());
    };
    let Some(binding) = context.get_local(local) else {
        unreachable!("an evaluated local has a binding")
    };
    if context.env().reference_inner(binding.ty()) != Some(inner) {
        todo!("move out of the referent of a reference local")
    }

    let safe = context
        .signature()
        .is_some_and(|signature| signature.contract().is_safe());
    if safe && context.env().semantics(inner).unsafe_move {
        let operation = "move of a type declared as @unsafe_move".into();
        return Err(hmir_error(span, &typecheck::UNSAFE_OPERATION, operation).into());
    }

    let moved = emit::lift(context, &value, span)?;
    emit::invalidate(context, &value, MIRInvalidationKind::Move, span);

    Ok(Evaluation::Ready(moved))
}

pub(crate) fn eval_address_of(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, operand));
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return match context.env().resolve_kind(value.ty()) {
            HMIRTypeKind::Function(_) => {
                Ok(Evaluation::Ready(coerce::std_rvalue(context, value, span)?))
            }
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

    Ok(Evaluation::Ready(emit::address(context, &value, ty, span)?))
}

pub(crate) fn eval_dereference(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    pointer: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, pointer));

    Ok(Evaluation::Ready(dereference(context, value, span)?))
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
