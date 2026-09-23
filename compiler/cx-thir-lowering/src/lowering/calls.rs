use cx_log::CXResult;
use cx_mir::{MIRComptimeOp, MIRConstant, MIRField, MIRInstruction, MIRInstructionKind, MIRValue};
use cx_thir::thir::{
    data::THIRType,
    expression::{THIRExpression, THIRExpressionKind, THIRFnContract},
    r#type::THIRField,
};
use cx_tokens::TokenRange;

use crate::{
    builder::MIRBuilder,
    lowering::{
        comptime, lower_expression,
        types::{lower_type, lower_type_id},
    },
};

pub(super) fn lower_call(
    builder: &mut MIRBuilder<'_>,
    function: &THIRExpression,
    arguments: &[THIRExpression],
    contract: &THIRFnContract,
    result_type: &THIRType,
    range: TokenRange,
) -> CXResult<MIRValue> {
    let callee = lower_expression(builder, function)?;
    if let MIRValue::Constant(MIRConstant::Function(id)) = callee
        && builder.module().comptime_function(id).is_some()
    {
        if builder.fun().body().is_comptime() {
            let args = arguments
                .iter()
                .map(|argument| lower_expression(builder, argument))
                .collect::<CXResult<Vec<_>>>()?;
            let out = if result_type.is_void() || result_type.is_unreachable() {
                None
            } else {
                let ty = lower_type(builder, result_type)?;
                Some(builder.fun_mut().new_register(ty, None))
            };
            builder.emit_comptime(
                MIRComptimeOp::Call {
                    out,
                    callee: id,
                    args,
                },
                range,
            );
            return Ok(out
                .map(MIRValue::Register)
                .unwrap_or(MIRValue::Constant(MIRConstant::Unit)));
        }
        let args = arguments
            .iter()
            .map(|argument| comptime::evaluate(builder, argument))
            .collect::<CXResult<Vec<_>>>()?;
        let result = comptime::evaluate_function(builder, id, &args)?;
        return Ok(MIRValue::Constant(result));
    }
    let mut args = Vec::with_capacity(arguments.len());
    for argument in arguments {
        args.push(lower_expression(builder, argument)?);
    }

    let out = if result_type.is_void() || result_type.is_unreachable() {
        None
    } else {
        let type_id = lower_type(builder, result_type)?;
        Some(builder.fun_mut().new_register(type_id, None))
    };

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Call { out, callee, args },
        range.clone(),
    ));

    if contract.noreturn
        || matches!(&function.kind, THIRExpressionKind::FunctionReference { name, .. } if name.as_str() == "exit")
    {
        builder.emit(MIRInstruction::new(MIRInstructionKind::Unreachable, range));
    }

    Ok(out
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit)))
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
