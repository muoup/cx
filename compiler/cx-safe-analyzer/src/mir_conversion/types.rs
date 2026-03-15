use cx_ast::data::CXReceiverMode;
use cx_mir::mir::{expression::MIRExpression, types::{MIRFunctionPrototype, MIRType, MIRTypeKind}};
use cx_util::CXResult;

use crate::{log_analysis_error, mir_conversion::environment::FMIREnvironment};

pub(crate) fn validate_safe_function_signature(
    env: &mut FMIREnvironment,
    prototype: &MIRFunctionPrototype,
    body: &MIRExpression
) -> CXResult<()> {
    if !prototype.contract.safe {
        return Ok(());
    }

    if prototype.var_args {
        return log_analysis_error!(
            env,
            body,
            "Safe function '{}' may not use varargs",
            prototype.name
        );
    }

    if !return_type_is_safe_signature(&prototype.return_type) {
        return log_analysis_error!(
            env,
            body,
            "Safe function '{}' has unsupported return type {}",
            prototype.name,
            prototype.return_type
        );
    }

    for (index, param) in prototype.params.iter().enumerate() {
        if !parameter_is_safe_signature(prototype, index, param) {
            return log_analysis_error!(
                env,
                body,
                "Safe function '{}' has unsupported parameter type {}",
                prototype.name,
                param._type
            );
        }
    }

    Ok(())
}

fn type_is_safe_signature(ty: &MIRType) -> bool {
    match &ty.kind {
        MIRTypeKind::Integer { .. } | MIRTypeKind::Float { .. } |
        MIRTypeKind::Unit | MIRTypeKind::Structured { .. } | MIRTypeKind::TaggedUnion { .. } |
        MIRTypeKind::Str => true,
        MIRTypeKind::MemoryReference { inner_type } => type_is_safe_signature(inner_type),
        _ => false,
    }
}

fn return_type_is_safe_signature(return_type: &MIRType) -> bool {
    type_is_safe_signature(return_type) || return_type.is_memory_reference()
}

fn receiver_type_is_safe_signature(receiver_type: &MIRType) -> bool {
    match &receiver_type.kind {
        MIRTypeKind::MemoryReference { inner_type } => type_is_safe_signature(inner_type),
        _ => false,
    }
}

fn parameter_is_safe_signature(
    prototype: &MIRFunctionPrototype,
    index: usize,
    param: &cx_mir::mir::types::MIRParameter,
) -> bool {
    if index == 0
        && prototype
            .source_prototype
            .kind
            .receiver()
            .is_some_and(|receiver| receiver.mode == CXReceiverMode::ByRef)
        && matches!(param.name.as_ref().map(|name| name.as_str()), Some("this"))
    {
        return receiver_type_is_safe_signature(&param._type);
    }

    type_is_safe_signature(&param._type)
}