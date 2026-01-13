use cx_parsing_data::ast::CXExpr;
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRValue},
    program::MIRBaseMappings,
    types::{MIRFunctionPrototype, MIRParameter, MIRType},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{casting::implicit_cast, typechecker::typecheck_expr},
};

fn create_clause_scope(
    env: &mut TypeEnvironment,
    prototype: &MIRFunctionPrototype,
    parameters: &[MIRValue],
) -> CXResult<()> {
    env.push_scope(None, None);

    for (i, MIRParameter { name, _type }) in prototype.params.iter().enumerate() {
        let Some(name) = name else {
            continue;
        };

        env.insert_stack_symbol(name.as_string(), parameters[i].clone());
    }

    Ok(())
}

fn assume_clause(
    env: &mut TypeEnvironment,
    prototype: &MIRFunctionPrototype,
    base_data: &MIRBaseMappings,
    parameters: &[MIRValue],
    clause: &CXExpr,
) -> CXResult<()> {
    create_clause_scope(env, prototype, parameters)?;

    let result = typecheck_expr(env, base_data, clause, Some(&MIRType::bool()))
        .and_then(|value| implicit_cast(env, clause, value, &MIRType::bool()))?;
    env.builder
        .add_instruction(MIRInstruction::Assume { value: result });

    env.pop_scope();

    Ok(())
}

fn assert_clause(
    env: &mut TypeEnvironment,
    prototype: &MIRFunctionPrototype,
    base_data: &MIRBaseMappings,
    parameters: &[MIRValue],
    clause: &CXExpr,
    message: Option<&str>,
) -> CXResult<()> {
    create_clause_scope(env, prototype, parameters)?;

    let result = typecheck_expr(env, base_data, clause, Some(&MIRType::bool()))
        .and_then(|value| implicit_cast(env, clause, value, &MIRType::bool()))?;
    env.builder.add_instruction(MIRInstruction::Assert {
        value: result,
        message: format!("Function contract violated: {}", message.unwrap_or("")),
    });

    env.pop_scope();

    Ok(())
}

pub fn contracted_function_return(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    return_value: Option<MIRValue>,
) -> CXResult<()> {
    let prototype = env.current_function().clone();

    if let Some((result_reg, postcondition)) = &prototype.contract.postcondition {
        if let Some(result_reg) = &result_reg {
            let Some(return_value) = &return_value else {
                return log_typecheck_error!(
                    env,
                    postcondition,
                    "Function postcondition cannot refer to result of function with void return type"
                );
            };

            env.insert_stack_symbol(result_reg.as_string(), return_value.clone());
        }

        // We know this is safe, arg_vals will always remain unmutated during typechecking,
        // it is only instantiated at the start of function typechecking and cleared at the end.
        let unsafe_arg_vals = unsafe { std::mem::transmute(env.arg_vals.as_slice()) };

        env.push_scope(None, None);
        assert_clause(
            env,
            &prototype,
            base_data,
            unsafe_arg_vals,
            postcondition,
            Some("Postcondition not met"),
        )?;
        env.pop_scope();
    }

    env.builder.add_return(return_value);
    Ok(())
}

pub fn contracted_function_call(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: &MIRFunctionPrototype,
    function: MIRValue,
    parameters: &[MIRValue],
) -> CXResult<MIRValue> {
    if let Some(precondition) = &prototype.contract.precondition {
        assert_clause(
            env,
            prototype,
            base_data,
            parameters,
            precondition,
            Some("Precondition not met"),
        )?;
    }

    let result = if !prototype.return_type.is_unit() {
        Some(env.builder.new_register())
    } else {
        None
    };

    let arguments = parameters.to_vec();
    let return_type = prototype.return_type.clone();

    env.builder.add_instruction(MIRInstruction::CallFunction {
        result: result.clone(),
        function,
        arguments,
    });

    if let Some((result_reg, postcondition)) = &prototype.contract.postcondition {
        if let Some(result_reg) = &result_reg {
            let Some(result) = result.clone() else {
                return log_typecheck_error!(
                    env,
                    postcondition,
                    "Function postcondition cannot refer to result of function with void return type"
                );
            };

            env.insert_stack_symbol(
                result_reg.as_string(),
                MIRValue::Register {
                    register: result.clone(),
                    _type: prototype.return_type.clone(),
                },
            );
        }

        assume_clause(env, prototype, base_data, parameters, postcondition)?;
    }

    Ok(match result {
        Some(result) => MIRValue::Register {
            register: result,
            _type: return_type,
        },
        None => MIRValue::NULL,
    })
}
