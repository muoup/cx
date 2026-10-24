use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{LocalLoweringContext, Permission},
    eval::{
        EvalResult, coerce, eval,
        flow::{Evaluation, ready},
        literal,
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

    let local = require_local(body, operand, span, "move")?;
    let Some(binding) = context.get_local(&local) else {
        unreachable!("an evaluated local has a binding")
    };
    if context.env().reference_inner(binding.ty()) != Some(inner) {
        todo!("move out of the referent of a reference local")
    }

    if context.env().semantics(inner).unsafe_move {
        require_nonsafe(context, span, "move of a type declared as @unsafe_move")?;
    }

    let moved = emit::lift(context, &value, span)?;
    emit::invalidate(context, &value, MIRInvalidationKind::Move, span);

    Ok(Evaluation::Ready(moved))
}

pub(crate) fn eval_leak(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, operand));
    require_local(body, operand, span, "@leak")?;
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return Err(hmir_error(
            span,
            &typecheck::TYPE_MISMATCH,
            (
                "@leak".into(),
                "memory reference type".into(),
                types::name(context.env(), value.ty()),
            ),
        )
        .into());
    };
    require_nonsafe(context, span, "@leak")?;

    if !context.env().is_nodrop(inner) {
        return Ok(Evaluation::Ready(value));
    }

    emit::invalidate(context, &value, MIRInvalidationKind::Leak, span);
    Ok(Evaluation::Ready(literal::unit(context, span)))
}

fn require_local(
    body: &HMIRBody,
    operand: HMIRExprID,
    span: &TokenRange,
    operation: &str,
) -> CXResult<HMIRLocalID> {
    match body.expr(operand).kind() {
        HMIRExprKind::Local(local) => Ok(*local),
        _ => Err(hmir_error(
            span,
            &typecheck::LOCAL_VARIABLE_REQUIRED,
            operation.into(),
        )),
    }
}

fn require_nonsafe(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if context.permission() < Permission::Nonsafe {
        return Err(hmir_error(
            span,
            &typecheck::UNSAFE_OPERATION,
            operation.into(),
        ));
    }
    Ok(())
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

    let ty = types::pointer(context.env_mut(), inner, span);

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
        return Ok(HMIRValue::indirect(inner, pointer));
    }

    let storage = emit::dereference(context, pointer, inner, span)?;
    emit::reference(context, &storage, span)
}
