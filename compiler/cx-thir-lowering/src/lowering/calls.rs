use cx_log::CXResult;
use cx_mir::{MIRConstant, MIRField, MIRInstrKind, MIRValue};
use cx_thir::thir::{
    data::THIRType,
    expression::THIRExpression,
    r#type::THIRField,
};
use cx_thir::type_context::THIRTypeContext;

use crate::{
    builder::MIRBuilder,
    lowering::types::{lower_type, lower_type_id},
};

pub(super) fn lower_call(
    builder: &mut MIRBuilder<'_>,
    function: &THIRExpression,
    arguments: &[THIRExpression],
    contract: &cx_thir::thir::expression::THIRFnContract,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let callee = super::lower_expression(builder, function)?;
    let mut args = Vec::with_capacity(arguments.len());
    for argument in arguments {
        args.push(super::lower_expression(builder, argument)?);
    }

    if let Some(precondition) = &contract.precondition {
        builder.push_named_scope();
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
                builder.bind_named(name, argument);
            }
        }
        super::lower_expression(builder, precondition)?;
        builder.pop_named_scope();
    }

    let returns_value = !result_type.is_void() && !result_type.is_unreachable();
    let out = returns_value.then(|| {
        let result_type_id = lower_type(builder, result_type);
        builder.register(result_type_id, None)
    });
    builder.emit(MIRInstrKind::Call {
        out,
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
        builder.push_named_scope();
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
                builder.bind_named(name, argument);
            }
        }
        if let Some(name) = &postcondition.binding {
            builder.bind_named(name, value.clone());
        }
        let condition = super::lower_expression(builder, &postcondition.condition)?;
        builder.emit(MIRInstrKind::Assume { condition });
        builder.pop_named_scope();
    }

    Ok(value)
}

pub fn lower_field(builder: &mut MIRBuilder, field: &cx_thir::thir::r#type::THIRField) -> MIRField {
    match field {
        THIRField::Standard { name, type_id } => {
            MIRField::named(name.clone(), lower_type_id(builder, *type_id))
        }
        THIRField::Bitfield {
            name,
            integer_type_id,
            width,
        } => MIRField::Bitfield {
            name: name.clone(),
            integer_type_id: lower_type_id(builder, *integer_type_id),
            width: *width,
        },
    }
}
