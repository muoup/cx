use cx_hmir::{
    HMIRBinaryOp, HMIRBodyContext, HMIRControlOp, HMIROp, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::InternalIntrinsic;
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{LocalLoweringContext, Permission},
    eval::{
        EvalResult,
        arithmetic::{self, binary_operands, eval_operand},
        coerce::{self, promotion},
        eval,
        expr::{self, require_impure, require_nonsafe},
        flow::{self, EvalExit, Evaluation},
        types::{self, qualify},
    },
    log::{hmir_error, log_hmir_error},
    value::{self, HMIRValue, immediate::HMIRImmediate},
};

pub(crate) fn eval_assign(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    target: HMIRExprID,
    op: Option<HMIRBinaryOp>,
    value: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    require_impure(context, span, "assignment")?;
    let target = (eval(context, body, target))?.ready()?;
    let target = coerce::collapse(context, target, span)?;
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
    let value = coerce::implicit::standard(context, value, inner, span)?;
    emit::storage::invalidate(context, &target, MIRInvalidationKind::Drop, span);
    emit::storage::initialize(context, &target, Some(&value), span)?;
    Ok(Evaluation::Ready(target))
}

pub(crate) fn eval_ownership(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
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
    body: &HMIRBodyContext,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = (eval(context, body, operand))?.ready()?;
    let ty = qualify::value(context, value.ty(), span);
    if context.env().is_nocopy(ty) || context.env().is_nodrop(ty) {
        return Err(
            hmir_error(span, &typecheck::UNCOPYABLE, types::name(context.env(), ty)).into(),
        );
    }
    let value = match context.env().reference_inner(value.ty()) {
        Some(inner) => promotion::reference_load(context, value, inner, span)?,
        None => value,
    };
    Ok(Evaluation::Ready(coerce::implicit::standard(
        context, value, ty, span,
    )?))
}

fn eval_move(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    require_impure(context, span, "move")?;
    let local = require_local(body, operand, span, "move")?;
    let binding = context
        .get_local(&local)
        .cloned()
        .expect("an evaluated local has a binding");
    let (Some(_), Some(inner)) = (
        value::as_place(&binding),
        context.env().reference_inner(binding.ty()),
    ) else {
        let invalid = ("non-local variable".into(), "move expression".into());
        return Err(hmir_error(span, &typecheck::INVALID_FORM, invalid).into());
    };
    if context.env().semantics(inner).constant {
        let mutated = ("move out of".into(), types::name(context.env(), inner));
        return Err(hmir_error(span, &typecheck::MUTATE_CONST, mutated).into());
    }
    if context.env().semantics(inner).unsafe_move {
        require_nonsafe(context, span, "move of a type declared as @unsafe_move")?;
    }
    let moved = emit::storage::lift(context, &binding, span)?;
    emit::storage::invalidate(context, &binding, MIRInvalidationKind::Move, span);
    Ok(Evaluation::Ready(moved))
}

fn eval_leak(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    require_nonsafe(context, span, "@leak")?;
    require_impure(context, span, "@leak")?;
    require_local(body, operand, span, "@leak")?;

    let value = (eval(context, body, operand))?.ready()?;
    let inner = referenced(context, &value, "@leak", span)?;
    if !context.env().is_nodrop(inner) {
        return Ok(Evaluation::Ready(value));
    }

    emit::storage::invalidate(context, &value, MIRInvalidationKind::Leak, span);
    Ok(Evaluation::Ready(value::unit(context.env())))
}

fn eval_adopt(
    _: &mut LocalLoweringContext,
    _: &HMIRBodyContext,
    span: &TokenRange,
    _: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    Err(hmir_error(span, &typecheck::ADOPT_DECLARATION, ()).into())
}

// The operand of an '@adopt' initializer, and whether an '@unsafe' wraps the adoption
pub(crate) fn adoption(body: &HMIRBodyContext, expr: HMIRExprID) -> Option<(HMIRExprID, bool)> {
    match body.expr(expr).kind() {
        HMIRExprKind::Op(HMIROp::OwnershipOp(HMIROwnershipOp::Adopt(operand))) => {
            Some((*operand, false))
        }
        HMIRExprKind::Op(HMIROp::Control(HMIRControlOp::Unsafe(inner))) => {
            adoption(body, *inner).map(|(operand, _)| (operand, true))
        }
        _ => None,
    }
}

// Binds a declared local to the storage its initializer designates, taking over its obligations; the declared type
// is exactly the adopted one, so no conversion copies out of the adopted storage
pub(crate) fn adopt(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    local: HMIRLocalID,
    declared: Evaluation<HMIRTypeID>,
    operand: HMIRExprID,
    unsafe_: bool,
) -> EvalResult<Evaluation<HMIRValue>> {
    let expectations = match unsafe_ {
        true => context.expectations().permitting(Permission::Unsafe),
        false => context.expectations(),
    };
    flow::scoped(context, expectations, |context| {
        require_nonsafe(context, span, "@adopt")?;
        require_impure(context, span, "@adopt")?;
        if !context.is_runtime() {
            return Err(
                hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, "'@adopt'".into()).into(),
            );
        }
        let source = (eval(context, body, operand))?.ready()?;
        let adopted = referenced(context, &source, "@adopt", span)?;
        let ty = declared.complete(context, body, Some(adopted))?;
        let (from, to) = (
            context.env().semantics(adopted),
            context.env().semantics(ty),
        );
        let qualified = (!from.constant || to.constant) && (!from.volatile || to.volatile);
        let (bare, adopted_bare) = (
            qualify::unqualified(context, ty, span),
            qualify::unqualified(context, adopted, span),
        );
        if !qualified || !context.env().same_type(bare, adopted_bare) {
            let mismatch = (
                "@adopt".into(),
                types::name(context.env(), ty),
                types::name(context.env(), adopted),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
        }
        let name = body.local(local).name().cloned();
        let storage = emit::storage::adopt(context, ty, name, &source, span)?;
        emit::storage::bind_local(context, local, storage);
        Ok(Evaluation::Ready(value::unit(context.env())))
    })
}

fn referenced(
    context: &LocalLoweringContext,
    value: &HMIRValue,
    operation: &str,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    context.env().reference_inner(value.ty()).ok_or_else(|| {
        let mismatch = (
            operation.into(),
            "memory reference type".into(),
            types::name(context.env(), value.ty()),
        );
        hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch)
    })
}

fn require_local(
    body: &HMIRBodyContext,
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

pub(crate) fn eval_address_of(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = eval(context, body, operand)?
        .ready()
        .and_then(|value| coerce::collapse(context, value, span).map_err(EvalExit::from))?;
    if types::bitfield(context.env(), value.ty()).is_some() {
        let usage = "take the address of".into();
        return Err(hmir_error(span, &typecheck::BITFIELD_REFERENCE, usage).into());
    }

    Ok(Evaluation::Ready(
        match context.env().resolve_kind(value.ty()) {
            HMIRTypeKind::Function(_) => {
                if types::comptime_only(context.env(), value.ty()) {
                    return Err(hmir_error(
                        span,
                        &typecheck::COMPTIME_ONLY_TYPE,
                        "take the address of".into(),
                    )
                    .into());
                }

                let ty = types::pointer(context.env_mut(), value.ty(), span);
                value.with_ty(ty)
            }

            HMIRTypeKind::ReferenceTo { inner, bitfield } => {
                if bitfield.is_some() {
                    return Err(hmir_error(
                        span,
                        &typecheck::BITFIELD_REFERENCE,
                        "take the address of".into(),
                    )
                    .into());
                }

                let inner = *inner;
                let ty = types::pointer(context.env_mut(), inner, span);
                value.with_ty(ty)
            }

            _ => {
                return log_hmir_error(
                    span,
                    &typecheck::NOT_ADDRESSABLE,
                    "take the address of".into(),
                )
                .map_err(EvalExit::from);
            }
        },
    ))
}

pub(crate) fn eval_dereference(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    pointer: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = span.clone();

    eval(context, body, pointer)?.and_then(context, body, move |context, _, value| {
        dereference(context, value, None, &span)
            .map(Evaluation::Ready)
            .map_err(EvalExit::from)
    })
}

pub(crate) fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    base: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = promotion::std_rvalue(context, pointer, span)?;
    let Some(inner) = context.env().pointer_inner(pointer.ty()) else {
        let kind = (types::name(context.env(), pointer.ty()), "a pointer".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind));
    };

    require_nonsafe(context, span, "dereferencing a pointer")?;
    null_check(context, base.unwrap_or(&pointer), span)?;

    let ref_ty = types::reference(context.env_mut(), inner, None, span);
    Ok(pointer.with_ty(ref_ty))
}

pub(crate) fn null_check(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    if !context.env().settings().assertions() {
        return Ok(());
    }
    let null = HMIRValue::new(pointer.ty(), HMIRImmediate::Int(0));
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let valid = arithmetic::binary(context, HMIRBinaryOp::Neq, pointer, &null, boolean, span)?;
    let unit = value::unit(context.env()).ty();
    let assert = InternalIntrinsic::Assert {
        condition: valid,
        message: Some("Attempted to dereference a null pointer".into()),
    };
    emit::intrinsics::internal(context, &assert, unit, span)?;

    Ok(())
}
