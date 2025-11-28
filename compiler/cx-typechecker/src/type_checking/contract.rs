use cx_parsing_data::ast::CXExpr;
use cx_typechecker_data::{
    ast::TCBaseMappings,
    mir::{
        expression::{MIRInstruction, MIRValue},
        types::{CXFunctionPrototype, TCParameter},
    },
};
use cx_util::CXResult;

use crate::{environment::TCEnvironment, log_typecheck_error, type_checking::typechecker::typecheck_expr};

fn verify_clause(
    env: &mut TCEnvironment,
    prototype: &CXFunctionPrototype,
    base_data: &TCBaseMappings,
    parameters: &[MIRValue],
    clause: &CXExpr,
) -> CXResult<()> {
    env.push_scope(None, None);
    
    for (i, TCParameter { name, _type }) in prototype.params.iter().enumerate() {
        let Some(name) = name else { continue; };
        
        env.insert_symbol(
            name.as_string(),
            parameters[i].clone()
        );
    }
    
    let result = typecheck_expr(env, base_data, clause)?;
    env.builder.add_instruction(
        MIRInstruction::Assert { value: result, message: "Contract clause failed".to_string() }
    );
    
    env.pop_scope();
    
    Ok(())
}

pub fn contracted_function_call(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    prototype: &CXFunctionPrototype,
    function: MIRValue,
    parameters: impl Iterator<Item = CXExpr>,
) -> CXResult<MIRValue> {
    let params = parameters
        .map(|param| typecheck_expr(env, base_data, &param))
        .collect::<CXResult<Vec<_>>>()?;

    if let Some(precondition) = &prototype.contract.precondition {
        verify_clause(env, prototype, base_data, &params, precondition)?;
    }
    
    let result = if prototype.return_type.is_unit() {
        Some(env.builder.new_register())
    } else {
        None
    };
    
    env.builder.add_instruction(MIRInstruction::CallFunction {
        result: result.clone(),
        function,
        arguments: params.clone(),
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
            
            env.insert_symbol(
                result_reg.as_string(),
                MIRValue::Register {
                    register: result.clone(),
                    _type: prototype.return_type.clone(),
                }
            );
        }
        
        verify_clause(env, prototype, base_data, &params, postcondition)?;
    }

    Ok(match result {
        Some(result) => MIRValue::Register {
            register: result,
            _type: prototype.return_type.clone(),
        },
        None => MIRValue::NULL,
    })
}