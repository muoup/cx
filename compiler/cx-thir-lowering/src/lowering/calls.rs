use cx_log::CXResult;
use cx_mir::{
    MIRComptimeOp, MIRComptimeOperand, MIRComptimeOutput, MIRComptimeType, MIRComptimeValue,
    MIRConstant, MIRField, MIRInstruction, MIRInstructionKind, MIRInternalIntrinsic, MIRValue,
};
use cx_thir::thir::{
    data::THIRType,
    expression::{THIRExpression, THIRExpressionKind, THIRFnContract},
    r#type::{THIRField, THIRTypeKind},
};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;

use crate::{
    builder::MIRBuilder,
    lowering::{
        comptime, control_flow::auto_pop_scope, lower_expression, staged,
        types::{lower_type, lower_type_id},
    },
};

pub(super) fn lower_call<'thir>(
    builder: &mut MIRBuilder<'thir>,
    function: &'thir THIRExpression,
    arguments: &'thir [THIRExpression],
    contract: &'thir THIRFnContract,
    result_type: &'thir THIRType,
    range: TokenRange,
) -> CXResult<MIRComptimeOperand> {
    let callee = lower_expression(builder, function)?;
    if let MIRValue::Constant(MIRConstant::Function(id)) = callee
        && builder.module().comptime_function(id).is_some()
    {
        let signature = builder
            .module()
            .comptime_function(id)
            .expect("comptime function disappeared")
            .prototype()
            .signature()
            .clone();
        let mut args = Vec::with_capacity(arguments.len());
        for (argument, parameter) in arguments.iter().zip(signature.params()) {
            if matches!(parameter.ty, MIRComptimeType::StagedExpression { .. }) {
                args.push(staged::lower_operand(builder, argument)?);
            } else if builder.fun().body().is_comptime() {
                args.push(MIRComptimeOperand::Runtime(lower_expression(
                    builder, argument,
                )?));
            } else {
                args.push(MIRComptimeOperand::Known(MIRComptimeValue::Constant(
                    comptime::evaluate(builder, argument)?,
                )));
            }
        }
        if builder.fun().body().is_comptime() {
            let out = if matches!(
                signature.return_type(),
                MIRComptimeType::StagedExpression { .. }
            ) {
                Some(MIRComptimeOutput::Comptime(
                    builder
                        .fun_mut()
                        .new_comptime_register(signature.return_type().clone(), None),
                ))
            } else if result_type.is_void() || result_type.is_unreachable() {
                None
            } else {
                let ty = lower_type(builder, result_type)?;
                Some(MIRComptimeOutput::Runtime(
                    builder.fun_mut().new_register(ty, None),
                ))
            };
            builder.emit_comptime(
                MIRComptimeOp::Call {
                    out: out.clone(),
                    callee: id,
                    args,
                },
                range,
            );
            return Ok(match out {
                Some(MIRComptimeOutput::Runtime(register)) => {
                    MIRComptimeOperand::Runtime(MIRValue::Register(register))
                }
                Some(MIRComptimeOutput::Comptime(register)) => {
                    MIRComptimeOperand::Comptime(register)
                }
                None => MIRComptimeOperand::Runtime(MIRValue::Constant(MIRConstant::Unit)),
            });
        }
        let args = args
            .iter()
            .map(|argument| match argument {
                MIRComptimeOperand::Known(value) => Ok(value.clone()),
                MIRComptimeOperand::Runtime(MIRValue::Constant(value)) => {
                    Ok(MIRComptimeValue::Constant(value.clone()))
                }
                MIRComptimeOperand::Runtime(value) => Ok(MIRComptimeValue::Caller(value.clone())),
                MIRComptimeOperand::Comptime(_) => {
                    unreachable!("runtime comptime call has a deferred register")
                }
            })
            .collect::<CXResult<Vec<_>>>()?;
        let result = comptime::evaluate_function(builder, id, &args)?;
        return Ok(MIRComptimeOperand::Known(result));
    }
    let mut args = Vec::with_capacity(arguments.len());
    for argument in arguments {
        args.push(lower_expression(builder, argument)?);
    }

    let mut function_type = &function._type;
    let signature = loop {
        match &function_type.kind {
            THIRTypeKind::PointerTo { inner_type }
            | THIRTypeKind::MemoryReference { inner_type, .. } => {
                function_type = builder.registry().resolve_type_id(*inner_type);
            }
            _ => break function_type.function_signature(),
        }
    };
    let parameter_names = signature
        .map(|signature| {
            signature
                .params
                .iter()
                .map(|parameter| parameter.name.clone())
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    if let Some(precondition) = &contract.precondition {
        builder.fun_mut().push_scope(precondition.token_range.clone());
        for (name, value) in parameter_names.iter().zip(&args) {
            if let Some(name) = name {
                builder.fun_mut().bind_named_value(name, value.clone());
            }
        }
        lower_expression(builder, precondition)?;
        auto_pop_scope(builder)?;
    }

    let out = if result_type.is_void() || result_type.is_unreachable() {
        None
    } else {
        let type_id = lower_type(builder, result_type)?;
        Some(builder.fun_mut().new_register(type_id, None))
    };

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Call {
            out,
            callee,
            args: args.clone(),
        },
        range.clone(),
    ));

    if let Some(postcondition) = &contract.postcondition {
        builder
            .fun_mut()
            .push_scope(postcondition.condition.token_range.clone());
        for (name, value) in parameter_names.iter().zip(&args) {
            if let Some(name) = name {
                builder.fun_mut().bind_named_value(name, value.clone());
            }
        }
        if let (Some(name), Some(out)) = (&postcondition.binding, out) {
            builder.fun_mut().bind_named_value(name, MIRValue::Register(out));
        }
        let condition = lower_expression(builder, &postcondition.condition)?;
        builder.fun_mut().emit_intrinsic(
            MIRInternalIntrinsic::Assume { condition },
            postcondition.condition.token_range.clone(),
        );
        auto_pop_scope(builder)?;
    }

    if contract.noreturn
        || matches!(&function.kind, THIRExpressionKind::FunctionReference { name, .. } if name.as_str() == "exit")
    {
        builder.emit(MIRInstruction::new(MIRInstructionKind::Unreachable, range));
    }

    Ok(MIRComptimeOperand::Runtime(
        out.map(MIRValue::Register)
            .unwrap_or(MIRValue::Constant(MIRConstant::Unit)),
    ))
}

pub(crate) fn lower_field(builder: &mut MIRBuilder<'_>, field: &THIRField) -> CXResult<MIRField> {
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
