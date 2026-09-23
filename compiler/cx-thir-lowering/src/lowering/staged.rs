use std::collections::BTreeMap;

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeOperand, MIRComptimeType, MIRComptimeValue, MIRStagedExpression, MIRValue,
};
use cx_thir::thir::{
    comptime::THIRStagedExpr,
    expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
};

use crate::{
    builder::MIRBuilder, log::mir_error, lowering::{calls, control_flow, lower_expression, types::lower_type},
};

pub(super) fn lower_operand<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expression: &'thir THIRExpression,
) -> CXResult<MIRComptimeOperand> {
    match &expression.kind {
        THIRExpressionKind::StagedExpression(staged) => lower_emit(builder, staged),
        THIRExpressionKind::Variable { local_id, .. } => {
            if let Some(value) = builder.fun().comptime_local(*local_id) {
                Ok(value)
            } else {
                lower_expression(builder, expression).map(MIRComptimeOperand::Runtime)
            }
        }
        THIRExpressionKind::CallFunction {
            function,
            arguments,
            contract,
        } => calls::lower_call(
            builder,
            function,
            arguments,
            contract,
            &expression._type,
            expression.token_range.clone(),
        ),
        _ => lower_expression(builder, expression).map(MIRComptimeOperand::Runtime),
    }
}

pub(super) fn runtime_value<'thir>(
    builder: &mut MIRBuilder<'thir>,
    operand: MIRComptimeOperand,
    range: &cx_tokens::TokenRange,
) -> CXResult<MIRValue> {
    match operand {
        MIRComptimeOperand::Runtime(value)
        | MIRComptimeOperand::Known(MIRComptimeValue::Caller(value)) => Ok(value),
        MIRComptimeOperand::Known(MIRComptimeValue::Constant(value)) => {
            Ok(MIRValue::Constant(value))
        }
        MIRComptimeOperand::Known(MIRComptimeValue::Staged(id)) => materialize(
            builder,
            MIRComptimeOperand::Known(MIRComptimeValue::Staged(id)),
            &[],
            range,
        ),
        MIRComptimeOperand::Comptime(_) => Err(mir_error(
            range,
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "staged value requires materialization".into(),
            ),
        )),
    }
}

fn lower_emit<'thir>(
    builder: &mut MIRBuilder<'thir>,
    staged: &'thir THIRStagedExpr,
) -> CXResult<MIRComptimeOperand> {
    let captures = staged
        .captures()
        .iter()
        .map(|id| capture(builder, *id).map(|value| (*id, value)))
        .collect::<CXResult<BTreeMap<_, _>>>()?;

    if !builder.fun().body().is_comptime() {
        let captures = captures
            .into_iter()
            .map(|(id, operand)| {
                let value = match operand {
                    MIRComptimeOperand::Known(value) => value,
                    MIRComptimeOperand::Runtime(MIRValue::Constant(value)) => {
                        MIRComptimeValue::Constant(value)
                    }
                    MIRComptimeOperand::Runtime(value) => MIRComptimeValue::Caller(value),
                    MIRComptimeOperand::Comptime(_) => {
                        unreachable!("runtime emit contains a deferred comptime register")
                    }
                };
                (id, value)
            })
            .collect();
        let id = builder.module().add_staged_expression(MIRStagedExpression {
            expression: staged.expr(),
            parameters: staged.params(),
            captures,
        });
        return Ok(MIRComptimeOperand::Known(MIRComptimeValue::Staged(id)));
    }

    let result = lower_type(builder, &staged.expr()._type)?;
    let params = staged
        .params()
        .iter()
        .map(|parameter| lower_type(builder, &parameter.ty))
        .collect::<CXResult<Vec<_>>>()?;
    let out = builder
        .fun_mut()
        .new_comptime_register(MIRComptimeType::StagedExpression { result, params }, None);
    builder.emit_comptime(
        cx_mir::MIRComptimeOp::Emit {
            out,
            expression: staged.expr(),
            parameters: staged.params(),
            captures,
        },
        staged.expr().token_range.clone(),
    );
    Ok(MIRComptimeOperand::Comptime(out))
}

fn capture(builder: &MIRBuilder<'_>, id: THIRLocalID) -> CXResult<MIRComptimeOperand> {
    if let Some(value) = builder.fun().comptime_local(id) {
        return Ok(value);
    }
    builder
        .fun()
        .local(id)
        .map(MIRComptimeOperand::Runtime)
        .ok_or_else(|| {
            mir_error(
                &builder.fun().current_scope_range(),
                (
                    &mir::MISSING_ENTITY,
                    (format!("local {id:?}"), "staged capture context".into()),
                ),
            )
        })
}

pub(super) fn materialize<'thir>(
    builder: &mut MIRBuilder<'thir>,
    operand: MIRComptimeOperand,
    arguments: &'thir [THIRExpression],
    range: &cx_tokens::TokenRange,
) -> CXResult<MIRValue> {
    if builder.fun().body().is_comptime() {
        return Err(mir_error(
            range,
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "materialization in a comptime function".into(),
            ),
        ));
    }
    let MIRComptimeOperand::Known(MIRComptimeValue::Staged(id)) = operand else {
        return Err(mir_error(
            range,
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "materialization requires an evaluated staged expression".into(),
            ),
        ));
    };
    let staged = builder
        .module()
        .staged_expression(id)
        .expect("staged expression is absent from its MIR unit");
    if arguments.len() != staged.parameters.len() {
        return Err(mir_error(
            range,
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "staged argument count mismatch".into(),
            ),
        ));
    }

    let lowered_args = arguments
        .iter()
        .map(|argument| lower_expression(builder, argument))
        .collect::<CXResult<Vec<_>>>()?;
    let mut locals = builder.fun().locals();
    let mut comptime = builder.fun().comptime_locals();
    for (id, value) in staged.captures {
        match value {
            MIRComptimeValue::Constant(value) => {
                locals.insert(id, MIRValue::Constant(value));
            }
            MIRComptimeValue::Caller(value) => {
                locals.insert(id, value);
            }
            MIRComptimeValue::Staged(staged_id) => {
                comptime.insert(
                    id,
                    MIRComptimeOperand::Known(MIRComptimeValue::Staged(staged_id)),
                );
            }
        }
    }
    for (parameter, argument) in staged.parameters.iter().zip(lowered_args) {
        locals.insert(parameter.local_id, argument);
    }

    let saved = builder.fun_mut().replace_local_bindings(locals, comptime);
    builder.fun_mut().push_scope(range.clone());
    let result = lower_expression(builder, staged.expression);
    let result = match result {
        Ok(value) => control_flow::auto_pop_scope(builder).map(|_| value),
        Err(error) => {
            let _ = builder.fun_mut().pop_scope();
            Err(error)
        }
    };
    builder.fun_mut().replace_local_bindings(saved.0, saved.1);
    result
}
