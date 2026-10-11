use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::lowering::FnLoweringContext,
    log::hmir_error,
    lower::{
        binding::lower_scoped,
        coerce::{convert, truthy},
        constant,
        dispatch::lower_expression,
        emit, promote,
    },
};

fn lower_condition(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
) -> CXResult<Option<MIRValue>> {
    let value = lower_expression(context, body, body.expr(condition))?;
    if emit::terminated(context.builder()) {
        return Ok(None);
    }

    let value = truthy(context, value, span)?;
    Ok(Some(context.materialize(&value, span)?))
}

struct Arm {
    block: MIRBasicBlockID,
    value: HMIRValue,
}

pub(crate) fn lower_if(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> CXResult<HMIRValue> {
    let Some(condition) = lower_condition(context, body, span, condition)? else {
        return Ok(constant::unit(context, span));
    };

    let then_block = context
        .builder_mut()
        .add_block(Some(CXIdent::new("if.then")));

    let Some(else_branch) = else_branch else {
        let merge = context
            .builder_mut()
            .add_block(Some(CXIdent::new("if.merge")));
        emit::branch(
            context.builder_mut(),
            condition,
            MIRBlockTarget::new(then_block),
            MIRBlockTarget::new(merge),
            span.clone(),
        );

        context.builder_mut().switch_to(then_block);
        lower_scoped(context, body, body.expr(then_branch))?;
        emit::jump(context.builder_mut(), merge, Vec::new(), span.clone());

        context.builder_mut().switch_to(merge);
        return Ok(constant::unit(context, span));
    };

    let else_block = context
        .builder_mut()
        .add_block(Some(CXIdent::new("if.else")));
    emit::branch(
        context.builder_mut(),
        condition,
        MIRBlockTarget::new(then_block),
        MIRBlockTarget::new(else_block),
        span.clone(),
    );

    let mut arms = Vec::new();
    for (block, branch) in [(then_block, then_branch), (else_block, else_branch)] {
        context.builder_mut().switch_to(block);
        let value = lower_scoped(context, body, body.expr(branch))?;
        if !emit::terminated(context.builder()) {
            let block = context.builder().current_block();
            arms.push(Arm { block, value });
        }
    }

    if arms.is_empty() {
        return Ok(constant::unit(context, span));
    }

    let merge = context
        .builder_mut()
        .add_block(Some(CXIdent::new("if.merge")));
    let Some(ty) = merged_type(context, &arms, span) else {
        for arm in arms {
            context.builder_mut().switch_to(arm.block);
            emit::jump(context.builder_mut(), merge, Vec::new(), span.clone());
        }

        context.builder_mut().switch_to(merge);
        return Ok(constant::unit(context, span));
    };

    let result = emit::register(context, ty, span)?;
    context.builder_mut().add_block_param(merge, result);

    for arm in arms {
        context.builder_mut().switch_to(arm.block);
        let value = convert(context, arm.value, ty, span)?;
        let value = context.materialize(&value, span)?;
        emit::jump(context.builder_mut(), merge, vec![value], span.clone());
    }

    context.builder_mut().switch_to(merge);
    Ok(context.bind(ty, MIRBindable::Register(result)))
}

fn merged_type(
    context: &mut FnLoweringContext,
    arms: &[Arm],
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let mut merged: Option<HMIRTypeID> = None;

    for arm in arms {
        let ty = arm.value.ty();
        if matches!(context.env().resolve_type_id(ty).kind(), HMIRTypeKind::Void) {
            return None;
        }

        merged = Some(match merged {
            None => ty,
            Some(current) if context.env().same_type(current, ty) => current,
            Some(current) => promote::common(context, current, ty, span).unwrap_or(current),
        });
    }

    merged
}

pub(crate) fn lower_while(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    pre_eval: bool,
) -> CXResult<HMIRValue> {
    let condition_block = context
        .builder_mut()
        .add_block(Some(CXIdent::new("while.condition")));
    let body_block = context
        .builder_mut()
        .add_block(Some(CXIdent::new("while.body")));
    let exit = context
        .builder_mut()
        .add_block(Some(CXIdent::new("while.exit")));

    let entry = if pre_eval {
        condition_block
    } else {
        body_block
    };
    emit::jump(context.builder_mut(), entry, Vec::new(), span.clone());

    context.builder_mut().switch_to(condition_block);
    let Some(condition) = lower_condition(context, body, span, condition)? else {
        for block in [body_block, exit] {
            context.builder_mut().switch_to(block);
            context
                .builder_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
        }

        return Ok(constant::unit(context, span));
    };

    emit::branch(
        context.builder_mut(),
        condition,
        MIRBlockTarget::new(body_block),
        MIRBlockTarget::new(exit),
        span.clone(),
    );

    context.builder_mut().switch_to(body_block);
    lower_scoped(context, body, body.expr(loop_body))?;
    emit::jump(
        context.builder_mut(),
        condition_block,
        Vec::new(),
        span.clone(),
    );

    context.builder_mut().switch_to(exit);
    Ok(constant::unit(context, span))
}

pub(crate) fn lower_return(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> CXResult<HMIRValue> {
    let value = match value {
        Some(value) => {
            let value = lower_expression(context, body, body.expr(value))?;
            if emit::terminated(context.builder()) {
                return Ok(value);
            }

            Some(value)
        }
        None => None,
    };

    lower_return_value(context, body, value, span)?;
    Ok(constant::unit(context, span))
}

pub(crate) fn lower_return_value(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    value: Option<HMIRValue>,
    span: &TokenRange,
) -> CXResult<()> {
    let return_type = context.signature().return_type();
    let ty = context.env_mut().eval_type(body, return_type)?;
    let returns_void = match context.env().resolve_type_id(ty).kind() {
        HMIRTypeKind::Unreachable => {
            return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()));
        }
        HMIRTypeKind::Void => true,
        _ => false,
    };

    let value = match value {
        Some(value) if returns_void => {
            if !matches!(
                context.env().resolve_type_id(value.ty()).kind(),
                HMIRTypeKind::Void
            ) {
                return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()));
            }

            None
        }
        Some(value) => {
            let value = convert(context, value, ty, span)?;
            let value = emit::read(context, value, span)?;
            Some(context.materialize(&value, span)?)
        }
        None if returns_void => None,
        None => return Err(hmir_error(span, &typecheck::MISSING_RETURN_VALUE, ())),
    };

    let scopes = context.open_scopes().to_vec();
    for scope in scopes.into_iter().rev() {
        emit::drop_scope(context.builder_mut(), scope, span.clone());
    }

    context
        .builder_mut()
        .emit(MIRInstructionKind::Return { value }, span.clone());

    Ok(())
}
