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

fn create_clause_scope(
    env: &mut TCEnvironment,
    prototype: &CXFunctionPrototype,
    parameters: &[MIRValue],
) -> CXResult<()> {
    env.push_scope(None, None);
    
    for (i, TCParameter { name, _type }) in prototype.params.iter().enumerate() {
        let Some(name) = name else { continue; };
        
        env.insert_symbol(
            name.as_string(),
            parameters[i].clone()
        );
    }
    
    Ok(())
}

fn assume_clause(
    env: &mut TCEnvironment,
    prototype: &CXFunctionPrototype,
    base_data: &TCBaseMappings,
    parameters: &[MIRValue],
    clause: &CXExpr,
) -> CXResult<()> {
    create_clause_scope(env, prototype, parameters)?;
    
    let result = typecheck_expr(env, base_data, clause)?;
    env.builder.add_instruction(
        MIRInstruction::Assume { value: result }
    );
    
    env.pop_scope();
    
    Ok(())
}

fn assert_clause(
    env: &mut TCEnvironment,
    prototype: &CXFunctionPrototype,
    base_data: &TCBaseMappings,
    parameters: &[MIRValue],
    clause: &CXExpr,
) -> CXResult<()> {
    create_clause_scope(env, prototype, parameters)?;
    
    let result = typecheck_expr(env, base_data, clause)?;
    env.builder.add_instruction(
        MIRInstruction::Assert { value: result, message: "Contract clause failed".to_string() }
    );
    
    env.pop_scope();
    
    Ok(())
}

pub fn contracted_function_return(
    env: &mut TCEnvironment,
    return_value: Option<MIRValue>,
) -> CXResult<()> {
    let prototype = env.current_function();
    
    if let Some((result_reg, postcondition)) = &prototype.contract.postcondition {
        if let Some(result_reg) = &result_reg {
            let Some(return_value) = &return_value else {
                return log_typecheck_error!(
                    env,
                    postcondition,
                    "Function postcondition cannot refer to result of function with void return type"
                );
            };
            
            env.insert_symbol(
                result_reg.as_string(),
                return_value.clone(),
            );
        }
        
        env.push_scope(None, None);
        
        env.builder.add_instruction(
            MIRInstruction::Assert {
                value: return_value.clone().unwrap(),
                message: "Function postcondition failed".to_string(),
            }
        );
        
        env.pop_scope();
    }
    
    env.builder.add_instruction(MIRInstruction::Return {
        value: return_value,
    });
    
    Ok(())
}

pub fn contracted_function_call(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    prototype: &CXFunctionPrototype,
    function: MIRValue,
    parameters: &[MIRValue],
) -> CXResult<MIRValue> {
    if let Some(precondition) = &prototype.contract.precondition {
        assert_clause(env, prototype, base_data, &parameters, precondition)?;
    }
    
    let result = if !prototype.return_type.is_unit() {
        Some(env.builder.new_register())
    } else {
        None
    };
    
    env.builder.add_instruction(MIRInstruction::CallFunction {
        result: result.clone(),
        function,
        arguments: parameters.iter().cloned().collect(),
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
        
        assume_clause(env, prototype, base_data, parameters, postcondition)?;
    }

    Ok(match result {
        Some(result) => MIRValue::Register {
            register: result,
            _type: prototype.return_type.clone(),
        },
        None => MIRValue::NULL,
    })
}