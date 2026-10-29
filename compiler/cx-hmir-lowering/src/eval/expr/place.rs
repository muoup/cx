use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, eval,
        expr::{self, require_nonsafe, require_pure},
        flow::Evaluation,
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
) -> EvalResult<Evaluation<HMIRValue>> {
    require_pure(context, span, "assignment")?;
    let target = (eval(context, body, target))?.ready()?;

    let Some(inner) = context.env().reference_inner(target.ty()) else {
        return Err(hmir_error(span, &typecheck::NOT_ADDRESSABLE, "assign to".into()).into());
    };
    if context.env().semantics(inner).constant {
        let mutated = ("assign to".into(), types::name(context.env(), inner));
        return Err(hmir_error(span, &typecheck::MUTATE_CONST, mutated).into());
    }

    let value = match op {
        Some(op) => {
            let current = coerce::arithmetic(context, target.clone(), span)?;
            let operand = (eval_operand(context, body, span, value))?.ready()?;
            binary_operands(context, span, op, current, operand)?
        }
        None => eval(context, body, value)?.complete(context, body, Some(inner))?,
    };
    let value = coerce::implicit(context, value, inner, span)?;
    emit::invalidate(context, &target, MIRInvalidationKind::Drop, span);
    emit::initialize(context, &target, Some(&value), span)?;
    Ok(Evaluation::Ready(target))
}

pub(crate) fn eval_ownership(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIROwnershipOp,
) -> EvalResult<Evaluation<HMIRValue>> {
    match op {
        HMIROwnershipOp::Move(operand) => eval_move(context, body, span, *operand),
        HMIROwnershipOp::Leak(operand) => eval_leak(context, body, span, *operand),
        HMIROwnershipOp::Adopt(operand) => eval_adopt(context, body, span, *operand),
        HMIROwnershipOp::Copy(operand) => eval_copy(context, body, span, *operand),
    }
}

fn eval_copy(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = (eval(context, body, operand))?.ready()?;
    let ty = coerce::value_type(context, value.ty(), span);
    if context.env().is_nocopy(ty) || context.env().is_nodrop(ty) {
        return Err(hmir_error(span, &typecheck::UNCOPYABLE, types::name(context.env(), ty)).into());
    }
    let value = match context.env().is_reference(value.ty()) {
        true => emit::copy(context, value, span)?,
        false => value,
    };
    Ok(Evaluation::Ready(coerce::implicit(context, value, ty, span)?))
}

fn eval_move(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    require_pure(context, span, "move")?;
    let value = (eval(context, body, operand))?.ready()?;
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return Ok(Evaluation::Ready(value));
    };

    let local = require_local(body, operand, span, "move")?;
    let binding = context.get_local(&local).expect("an evaluated local has a binding");
    let declared = context.env().reference_inner(binding.ty());
    if declared.is_some_and(|declared| context.env().is_reference(declared)) {
        return Ok(Evaluation::Ready(value));
    }
    if declared != Some(inner) {
        if context.env().semantics(inner).constant {
            let mutated = ("move out of".into(), types::name(context.env(), inner));
            return Err(hmir_error(span, &typecheck::MUTATE_CONST, mutated).into());
        }
        let invalid = ("move out of a borrowed binding".into(), "a reference binding".into());
        return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, invalid).into());
    }
    if context.env().semantics(inner).unsafe_move {
        require_nonsafe(context, span, "move of a type declared as @unsafe_move")?;
    }
    let moved = emit::lift(context, &value, None, span)?;
    emit::invalidate(context, &value, MIRInvalidationKind::Move, span);
    Ok(Evaluation::Ready(moved))
}

fn eval_leak(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    require_nonsafe(context, span, "@leak")?;
    require_pure(context, span, "@leak")?;
    let value = (eval(context, body, operand))?.ready()?;
    require_local(body, operand, span, "@leak")?;
    let inner = referenced(context, &value, "@leak", span)?;
    if !context.env().is_nodrop(inner) {
        return Ok(Evaluation::Ready(value));
    }
    emit::invalidate(context, &value, MIRInvalidationKind::Leak, span);
    Ok(Evaluation::Ready(expr::unit(context, span)))
}

fn eval_adopt(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    require_nonsafe(context, span, "@adopt")?;
    require_pure(context, span, "@adopt")?;
    if !context.is_runtime() {
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, "'@adopt'".into()).into());
    }
    let value = (eval(context, body, operand))?.ready()?;
    let inner = referenced(context, &value, "@adopt", span)?;
    if context.env().semantics(inner).constant {
        let mismatch = ("@adopt".into(), "non-const type".into(), types::name(context.env(), inner));
        return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
    }
    if emit::place_held(context, &value) {
        return Err(hmir_error(span, &typecheck::ADOPT_LOCAL, ()).into());
    }
    let storage = emit::scratch(context, inner, span)?;
    emit::adopt(context, &storage, &value, span)?;
    Ok(Evaluation::Ready(storage))
}

fn referenced(context: &LocalLoweringContext, value: &HMIRValue, operation: &str, span: &TokenRange) -> CXResult<HMIRTypeID> {
    context.env().reference_inner(value.ty()).ok_or_else(|| {
        let mismatch = (operation.into(), "memory reference type".into(), types::name(context.env(), value.ty()));
        hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch)
    })
}

fn require_local(body: &HMIRBody, operand: HMIRExprID, span: &TokenRange, operation: &str) -> CXResult<HMIRLocalID> {
    match body.expr(operand).kind() {
        HMIRExprKind::Local(local) => Ok(*local),
        _ => Err(hmir_error(span, &typecheck::LOCAL_VARIABLE_REQUIRED, operation.into()).into()),
    }
}

pub(crate) fn eval_address_of(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = (eval(context, body, operand))?.ready()?;
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return match context.env().resolve_kind(value.ty()) {
            HMIRTypeKind::Function(_) if !types::comptime_only(context.env(), value.ty()) => {
                Ok(Evaluation::Ready(coerce::std_rvalue(context, value, span)?))
            }
            _ => Err(hmir_error(span, &typecheck::NOT_ADDRESSABLE, "take the address of".into()).into()),
        };
    };
    if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Str) {
        todo!("address of a string")
    }
    if types::bitfield(context.env(), &value).is_some() {
        let usage = "take the address of".into();
        return Err(hmir_error(span, &typecheck::BITFIELD_REFERENCE, usage).into());
    }
    let ty = types::pointer(context.env_mut(), inner, span);
    Ok(Evaluation::Ready(emit::address(context, &value, ty, span)?))
}

pub(crate) fn eval_dereference(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    pointer: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = (eval(context, body, pointer))?.ready()?;
    Ok(Evaluation::Ready(dereference(context, value, None, span)?))
}

pub(crate) fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    base: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = coerce::std_rvalue(context, pointer, span)?;
    let Some(inner) = context.env().pointer_inner(pointer.ty()) else {
        let kind = (types::name(context.env(), pointer.ty()), "a pointer".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    };
    if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Function(_)) {
        return Ok(HMIRValue::indirect(inner, pointer));
    }

    require_nonsafe(context, span, "dereferencing a pointer")?;
    let storage = emit::dereference(context, pointer, inner, base, span)?;
    emit::reference(context, &storage, span)
}
