use cx_hmir::{HMIRBody, expr::HMIRExprID, ty::HMIRTypeID};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{
        HMIRValue, return_type,
        binding::lower_scoped,
        coerce::{convert, truthy},
        dispatch::lower_expression,
        emit::{
            diverges, emit_branch, emit_jump, emit_return, new_block, new_block_param, rvalue,
            unreachable_value, void_value,
        },
        shape::{TypeShape, arithmetic_type, shape},
    },
    log::hmir_error,
};

fn lower_condition(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
) -> CXResult<Result<MIRValue, HMIRValue>> {
    let value = lower_expression(cx, body, body.expr(condition))?;
    if diverges(cx, &value) {
        return Ok(Err(value));
    }

    let value = truthy(cx, value, span)?;
    Ok(Ok(cx.materialize(&value, span)?))
}

struct Arm {
    block: MIRBasicBlockID,
    value: HMIRValue,
}

pub(crate) fn lower_if(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> CXResult<HMIRValue> {
    let condition = match lower_condition(cx, body, span, condition)? {
        Ok(condition) => condition,
        Err(diverged) => return Ok(diverged),
    };

    let then_block = new_block(cx, "if.then");
    let Some(else_branch) = else_branch else {
        let merge = new_block(cx, "if.merge");
        emit_branch(
            cx,
            condition,
            MIRBlockTarget::new(then_block),
            MIRBlockTarget::new(merge),
            span,
        );

        cx.builder().switch_to(then_block);
        lower_scoped(cx, body, body.expr(then_branch))?;
        emit_jump(cx, merge, Vec::new(), span);

        cx.builder().switch_to(merge);
        return Ok(void_value(cx, span));
    };

    let else_block = new_block(cx, "if.else");
    emit_branch(
        cx,
        condition,
        MIRBlockTarget::new(then_block),
        MIRBlockTarget::new(else_block),
        span,
    );

    let mut arms = Vec::new();
    for (block, branch) in [(then_block, then_branch), (else_block, else_branch)] {
        cx.builder().switch_to(block);
        let value = lower_scoped(cx, body, body.expr(branch))?;
        if !cx.builder().terminated() {
            let block = cx.builder().current_block();
            arms.push(Arm { block, value });
        }
    }

    if arms.is_empty() {
        return Ok(unreachable_value(cx, span));
    }

    let result_ty = merged_type(cx, &arms, span);
    let merge = new_block(cx, "if.merge");
    let result = match result_ty {
        Some(ty) => Some(new_block_param(cx, merge, ty, span)?),
        None => None,
    };

    for arm in arms {
        cx.builder().switch_to(arm.block);
        let args = match result_ty {
            Some(ty) => {
                let value = convert(cx, arm.value, ty, span)?;
                let value = rvalue(cx, value, span)?;
                vec![cx.materialize(&value, span)?]
            }
            None => Vec::new(),
        };
        emit_jump(cx, merge, args, span);
    }

    cx.builder().switch_to(merge);
    match result {
        Some(result) => Ok(result),
        None => Ok(void_value(cx, span)),
    }
}

fn merged_type(
    cx: &mut FnLoweringContext,
    arms: &[Arm],
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let mut merged: Option<HMIRTypeID> = None;

    for arm in arms {
        let ty = arm.value.ty();
        if shape(cx, ty) == TypeShape::Void {
            return None;
        }

        merged = Some(match merged {
            None => ty,
            Some(current) if cx.env().same_type(current, ty) => current,
            Some(current) => arithmetic_type(cx, current, ty, span).unwrap_or(current),
        });
    }

    merged
}

pub(crate) fn lower_while(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    pre_eval: bool,
) -> CXResult<HMIRValue> {
    let condition_block = new_block(cx, "while.condition");
    let body_block = new_block(cx, "while.body");
    let exit = new_block(cx, "while.exit");

    let entry = if pre_eval { condition_block } else { body_block };
    emit_jump(cx, entry, Vec::new(), span);

    cx.builder().switch_to(condition_block);
    let condition = match lower_condition(cx, body, span, condition)? {
        Ok(condition) => condition,
        Err(diverged) => {
            for block in [body_block, exit] {
                cx.builder().switch_to(block);
                cx.builder().emit(MIRInstructionKind::Unreachable, span);
            }

            return Ok(diverged);
        }
    };

    emit_branch(
        cx,
        condition,
        MIRBlockTarget::new(body_block),
        MIRBlockTarget::new(exit),
        span,
    );

    cx.builder().switch_to(body_block);
    lower_scoped(cx, body, body.expr(loop_body))?;
    emit_jump(cx, condition_block, Vec::new(), span);

    cx.builder().switch_to(exit);
    Ok(void_value(cx, span))
}

pub(crate) fn lower_return(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> CXResult<HMIRValue> {
    let value = match value {
        Some(value) => {
            let value = lower_expression(cx, body, body.expr(value))?;
            if diverges(cx, &value) {
                return Ok(value);
            }

            Some(value)
        }
        None => None,
    };

    lower_return_value(cx, value, span)?;
    Ok(unreachable_value(cx, span))
}

pub(crate) fn lower_return_value(
    cx: &mut FnLoweringContext,
    value: Option<HMIRValue>,
    span: &TokenRange,
) -> CXResult<()> {
    let ty = return_type(cx);

    let value = match (shape(cx, ty), value) {
        (TypeShape::Unreachable, _) => {
            return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()));
        }
        (TypeShape::Void, Some(value)) => {
            if shape(cx, value.ty()) != TypeShape::Void {
                return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()));
            }

            None
        }
        (TypeShape::Void, None) => None,
        (_, None) => return Err(hmir_error(span, &typecheck::MISSING_RETURN_VALUE, ())),
        (_, Some(value)) => {
            let value = convert(cx, value, ty, span)?;
            let value = rvalue(cx, value, span)?;
            Some(cx.materialize(&value, span)?)
        }
    };

    emit_return(cx, value, span);
    Ok(())
}
