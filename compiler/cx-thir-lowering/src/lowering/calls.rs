use cx_log::CXResult;
use std::sync::Arc;

use cx_mir::{
    MIRCallKind, MIRConstant, MIRField, MIRFunctionID, MIRFunctionMode, MIRInstrKind, MIRStagedTemplate, MIRValue
};
use cx_mir_comptime::{
    InterpretedFunction, MIRComptimeEngine, MIRComptimeValue, MIRStagedBinding, MIRStagedValue,
    context::MIRContext,
};
use cx_thir::thir::expression::THIRFnContract;
use cx_thir::thir::{
    data::THIRType,
    expression::{THIRExpression, THIRExpressionKind},
    r#type::{THIRField, THIRTypeKind},
};
use cx_thir::type_context::THIRTypeContext;

use crate::lowering::control_flow::auto_pop_scope;
use crate::lowering::lower_expression;
use crate::{
    builder::MIRBuilder,
    lowering::types::{lower_type, lower_type_id},
};

pub(super) fn lower_call(
    builder: &mut MIRBuilder<'_>,
    function: &THIRExpression,
    arguments: &[THIRExpression],
    contract: &THIRFnContract,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    if let THIRExpressionKind::Variable { .. } = &function.kind
        && matches!(function._type.kind, THIRTypeKind::Undefined)
    {
        let staged = lower_expression(builder, function)?;
        let mut args = Vec::with_capacity(arguments.len());
        for argument in arguments {
            args.push(lower_expression(builder, argument)?);
        }
        let out = if result_type.is_void() || result_type.is_unreachable() {
            None
        } else {
            let ty = lower_type(builder, result_type)?;
            Some(builder.fun_mut().new_register(ty, None))
        };
        builder.emit(MIRInstrKind::ApplyStaged { out, staged, args });
        return Ok(out
            .map(MIRValue::Register)
            .unwrap_or(MIRValue::Constant(MIRConstant::Unit)));
    }

    if let THIRExpressionKind::FunctionReference { name, .. } = &function.kind
        && let Some((id, prototype)) = builder.resolve_function(name.as_str())
        && prototype.signature.mode == MIRFunctionMode::Comptime
    {
        return lower_comptime_call(builder, id, &prototype.signature, arguments, result_type);
    }

    let callee = lower_expression(builder, function)?;
    let mut args = Vec::with_capacity(arguments.len());
    for argument in arguments {
        args.push(lower_expression(builder, argument)?);
    }

    if let Some(precondition) = &contract.precondition {
        builder.fun_mut().push_invisible_scope();
        let parameter_names = builder
            .registry()
            .intern_signature(&function._type)
            .map(|signature| {
                signature
                    .params
                    .iter()
                    .map(|parameter| parameter.name.clone())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        for (name, argument) in parameter_names.iter().zip(args.iter().cloned()) {
            if let Some(name) = name {
                builder.fun_mut().bind_named_value(name, argument);
            }
        }
        lower_expression(builder, precondition)?;
        auto_pop_scope(builder)?;
    }

    let returns_value = !result_type.is_void() && !result_type.is_unreachable();
    let out = if returns_value {
        let result_type_id = lower_type(builder, result_type)?;
        Some(builder.fun_mut().new_register(result_type_id, None))
    } else {
        None
    };
    builder.emit(MIRInstrKind::Call {
        out,
        kind: MIRCallKind::Runtime,
        callee,
        args: args.clone(),
    });
    let unreachable_return = builder
        .registry()
        .intern_signature(&function._type)
        .is_some_and(|signature| signature.return_type.is_unreachable());
    if contract.noreturn || unreachable_return {
        builder.emit(MIRInstrKind::Unreachable);
    }
    let value = out
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit));

    if let Some(postcondition) = &contract.postcondition {
        builder.fun_mut().push_invisible_scope();
        let parameter_names = builder
            .registry()
            .intern_signature(&function._type)
            .map(|signature| {
                signature
                    .params
                    .iter()
                    .map(|parameter| parameter.name.clone())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        for (name, argument) in parameter_names.iter().zip(args) {
            if let Some(name) = name {
                builder.fun_mut().bind_named_value(name, argument);
            }
        }
        if let Some(name) = &postcondition.binding {
            builder.fun_mut().bind_named_value(name, value.clone());
        }
        let condition = super::lower_expression(builder, &postcondition.condition)?;
        builder.emit(MIRInstrKind::Assume { condition });
        auto_pop_scope(builder)?;
    }

    Ok(value)
}

fn lower_comptime_call(
    builder: &mut MIRBuilder<'_>,
    function: MIRFunctionID,
    signature: &cx_mir::MIRFnSignature,
    arguments: &[THIRExpression],
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    if builder.fun().mode() == MIRFunctionMode::Comptime {
        let mut args = Vec::with_capacity(arguments.len());
        for (argument, parameter) in arguments.iter().zip(&signature.params) {
            if parameter.staged_params.is_some() {
                args.push(lower_staged_argument(
                    builder,
                    argument,
                    parameter.staged_diverges,
                )?);
            } else {
                args.push(lower_expression(builder, argument)?);
            }
        }
        let out = if result_type.is_void() || result_type.is_unreachable() {
            None
        } else {
            let ty = lower_type(builder, result_type)?;
            Some(builder.fun_mut().new_register(ty, None))
        };
        builder.emit(MIRInstrKind::Call {
            out,
            kind: MIRCallKind::Comptime,
            callee: MIRValue::Constant(MIRConstant::Function(function)),
            args,
        });
        return Ok(out
            .map(MIRValue::Register)
            .unwrap_or(MIRValue::Constant(MIRConstant::Unit)));
    }

    let mut args = Vec::with_capacity(arguments.len());
    for (argument, parameter) in arguments.iter().zip(&signature.params) {
        if parameter.staged_params.is_some() {
            let (template, captures) =
                capture_staged_argument(builder, argument, parameter.staged_diverges)?;
            let runtime_origin = (!captures.is_empty()).then(|| builder.fun().id());
            let captures = captures.into_iter().map(MIRStagedBinding::Value).collect();
            args.push(MIRComptimeValue::Staged(Arc::new(MIRStagedValue::new(
                template,
                captures,
                Vec::new(),
                runtime_origin,
            ))));
        } else {
            let value = lower_expression(builder, argument)?;
            let MIRValue::Constant(value) = value else {
                return builder.log_error(
                    argument.token_range.clone(),
                    "non-staged comptime arguments must currently be constants",
                );
            };
            args.push(MIRComptimeValue::Constant(value));
        }
    }

    let value = {
        let function = builder
            .module()
            .function(function)
            .expect("resolved comptime function exists");
        let entry = InterpretedFunction::new(function)
            .expect("comptime function has an MIR definition before runtime lowering");
        let mut engine = MIRComptimeEngine::new(builder.module());
        engine.run_values(entry, &args)?
    };

    match value {
        MIRComptimeValue::Constant(value) => Ok(MIRValue::Constant(value)),
        MIRComptimeValue::Staged(value) => super::staged::instantiate(builder, &value),
    }
}

fn lower_staged_argument(
    builder: &mut MIRBuilder<'_>,
    argument: &THIRExpression,
    diverges: bool,
) -> CXResult<MIRValue> {
    let (template, captures) = capture_staged_argument(builder, argument, diverges)?;
    let out = builder.fun_mut().new_register(template.result_type(), None);
    builder.emit(MIRInstrKind::MakeStaged {
        out,
        template,
        captures,
    });
    Ok(MIRValue::Register(out))
}

fn capture_staged_argument(
    builder: &mut MIRBuilder<'_>,
    argument: &THIRExpression,
    diverges: bool,
) -> CXResult<(Arc<MIRStagedTemplate>, Vec<MIRValue>)> {
    match &argument.kind {
        THIRExpressionKind::StagedExpression { params, body } => {
            let params = params
                .iter()
                .map(|(_, local, ty)| (*local, ty))
                .collect::<Vec<_>>();
            builder.capture_staged(body, &params, Some(diverges))
        }
        THIRExpressionKind::Variable { local_id, .. } => {
            let _ = builder.local_value(*local_id, &argument._type)?;
            builder.capture_staged(argument, &[], Some(diverges))
        }
        _ => builder.capture_staged(argument, &[], Some(diverges)),
    }
}

pub fn lower_field(
    builder: &mut MIRBuilder,
    field: &cx_thir::thir::r#type::THIRField,
) -> cx_log::CXResult<MIRField> {
    match field {
        THIRField::Standard { name, type_id } => Ok(MIRField::named(
            name.clone(),
            lower_type_id(builder, *type_id)?,
        )),
        THIRField::Bitfield {
            name,
            integer_type_id,
            width,
        } => Ok(MIRField::Bitfield {
            name: name.clone(),
            integer_type_id: lower_type_id(builder, *integer_type_id)?,
            width: *width,
        }),
    }
}
