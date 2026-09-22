use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRField, MIRInstructionKind, MIRValue};
use cx_thir::thir::{
    data::THIRType,
    expression::{THIRExpression, THIRFnContract, THIRExpressionKind},
    r#type::THIRField,
};
use cx_tokens::TokenRange;

use crate::{
    builder::MIRBuilder,
    lowering::{lower_expression, types::{lower_type, lower_type_id}},
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
    
    builder.emit(
        MIRInstructionKind::Call {
            out,
            callee,
            args,
        },
        range.clone(),
    );

    if contract.noreturn
        || matches!(&function.kind, THIRExpressionKind::FunctionReference { name, .. } if name.as_str() == "exit")
    {
        builder.emit(MIRInstructionKind::Unreachable, range);
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
