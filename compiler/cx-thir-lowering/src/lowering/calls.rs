use cx_log::CXResult;
use cx_mir::{MIRCallKind, MIRConstant, MIRField, MIRInstrKind, MIRValue};
use cx_thir::thir::expression::THIRFnContract;
use cx_thir::thir::{data::THIRType, expression::THIRExpression, r#type::THIRField};
use cx_thir::type_context::THIRTypeContext;

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
        super::lower_expression(builder, precondition)?;
        builder.fun_mut().pop_scope();
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
        builder.fun_mut().pop_scope();
    }

    Ok(value)
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
