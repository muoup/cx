use cx_hmir::{HMIRCoerceMode, HMIRExprID};
use cx_mir::{MIRConstant, MIRInternalIntrinsic, MIRPtrIntrinsic, MIRTarget, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    function::{
        Expect, FunctionLowering, LowerResult, Operand,
        expr::lower_expr,
        lower_eval_type,
        operand::{
            lower_auto_deref, lower_convert, lower_copy, lower_decay, lower_truthy, lower_value,
        },
    },
    value::promote_integer_type,
};

pub(super) fn lower_algebraic_coercion(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let operand = lower_auto_deref(cx, operand, span)?;
    let operand = lower_decay(cx, operand, span)?;
    let operand = if operand.is_lvalue() {
        if !cx.program.types().is_pod(operand.ty()) {
            return cx.error(
                span,
                format!("cannot copy '{}'", cx.program.types().display(operand.ty())),
            );
        }
        Operand::value(lower_copy(cx, &operand, span)?, operand.ty())
    } else {
        operand
    };
    let ty = promote_integer_type(cx.program.types_mut(), operand.ty());
    lower_convert(cx, operand, ty, span)
}

pub(super) fn lower_nonnull_pointer(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let ty = operand.ty();
    let pointer = lower_value(cx, operand, span)?;
    let bool = cx.program.types_mut().bool();
    let condition = cx.register(bool, span)?;
    let null_ty = cx.mir(ty, span)?;
    cx.intrinsic(
        MIRPtrIntrinsic::Neq {
            out: MIRTarget::Register(condition),
            lhs: pointer.clone(),
            rhs: MIRValue::Constant(MIRConstant::Nullptr { ty: null_ty }),
        },
        span,
    );
    cx.intrinsic(
        MIRInternalIntrinsic::Assert {
            condition: MIRValue::Register(condition),
            message: Some("dereferenced a null pointer".into()),
        },
        span,
    );
    Ok(pointer)
}

pub(super) fn lower_coerce(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if mode == HMIRCoerceMode::Truthy {
        let value = lower_expr(cx, frame, value, Expect::Any)?;
        return lower_truthy(cx, value, span);
    }
    let ty = lower_eval_type(cx, frame, target)?;
    let types = cx.program.types();
    let reference = types.reference_inner(ty);
    let expect = if reference.is_some() {
        Expect::Any
    } else {
        Expect::Type(ty)
    };
    let value = lower_expr(cx, frame, value, expect)?;
    let value = if mode == HMIRCoerceMode::CCast
        && reference.is_some_and(|inner| inner != value.ty())
        && cx.program.types().is_pointer(value.ty())
    {
        let value = lower_nonnull_pointer(cx, value, span)?;
        let out = cx.register(ty, span)?;
        let target_ty = cx.mir(ty, span)?;
        cx.intrinsic(
            MIRInternalIntrinsic::Bitcast {
                out: MIRTarget::Register(out),
                value,
                target_ty,
            },
            span,
        );
        Operand::register(out, ty)
    } else {
        lower_convert(cx, value, ty, span)?
    };
    if let Some(inner) = reference {
        if cx.program.types().is_function(inner) {
            let pointer = cx.program.types_mut().pointer_to(inner);
            return Ok(value.with_type(pointer));
        }
        return lower_auto_deref(cx, value, span);
    }
    Ok(value)
}
