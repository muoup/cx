use cx_parsing_data::{
    ast::{CXAST, CXExpr, CXFunctionStmt},
    data::CXFunctionKind,
};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRValue},
    program::MIRBaseMappings,
    types::{CXIntegerType, CXTemplateInput, MIRFunctionPrototype, MIRParameter, MIRType},
};
use cx_util::{CXError, CXResult};

use crate::{
    environment::{TypeEnvironment, deconstruction::generate_deconstructor}, safe_lowering::lower_safe_fn, type_checking::{
        move_semantics::acknowledge_declared_object,
        typechecker::{global_expr, typecheck_expr},
    }, type_completion::templates::{
        add_templated_types, complete_function_template, restore_template_overwrites,
    }
};

pub mod binary_ops;

pub(crate) mod casting;
pub(crate) mod contract;
pub(crate) mod r#match;
pub(crate) mod move_semantics;
pub(crate) mod structured_initialization;
pub(crate) mod typechecker;

fn generate_safe_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: MIRFunctionPrototype,
    body: &CXExpr,
) -> CXResult<()> {
    lower_safe_fn(env, base_data, prototype, body)?;
    
    Ok(())
}

fn generate_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: MIRFunctionPrototype,
    body: &CXExpr,
) -> CXResult<()> {
    if prototype.contract.safe {
        return generate_safe_function(env, base_data, prototype, body);
    }
    
    env.push_scope(None, None);
    env.builder.start_function(prototype.clone());
    env.arg_vals.clear();

    for MIRParameter { name, _type } in prototype.params.iter() {
        let Some(name) = name else {
            continue;
        };

        if _type.is_memory_resident() {
            let alias = env.builder.new_register();
            env.builder.add_instruction(MIRInstruction::Alias {
                result: alias.clone(),
                value: MIRValue::Parameter {
                    name: name.clone(),
                    _type: _type.clone(),
                },
            });

            env.insert_symbol(
                name.as_string(),
                MIRValue::Register {
                    register: alias.clone(),
                    _type: _type.clone(),
                },
            );

            acknowledge_declared_object(env, name.to_string(), alias, _type.clone());
        } else {
            let region = env.builder.new_register();
            env.builder
                .add_instruction(MIRInstruction::CreateStackRegion {
                    result: region.clone(),
                    _type: _type.clone(),
                });

            acknowledge_declared_object(env, name.to_string(), region.clone(), _type.clone());

            env.builder.add_instruction(MIRInstruction::MemoryWrite {
                target: MIRValue::Register {
                    register: region.clone(),
                    _type: _type.clone(),
                },
                value: MIRValue::Parameter {
                    name: name.clone(),
                    _type: _type.clone(),
                },
            });

            env.arg_vals.push(MIRValue::Register {
                register: region.clone(),
                _type: _type.clone().mem_ref_to(),
            });

            env.insert_symbol(
                name.as_string(),
                MIRValue::Register {
                    register: region,
                    _type: _type.clone().mem_ref_to(),
                },
            );
        }
    }

    typecheck_expr(env, base_data, body, None)?;

    if !env.builder.current_block_closed() {
        if env.builder.current_prototype().return_type.is_unit() {
            env.builder.add_return(None);
        } else if env.builder.current_prototype().name.as_str() == "main" {
            env.builder.add_return(Some(MIRValue::IntLiteral {
                value: 0,
                signed: true,
                _type: CXIntegerType::I32,
            }));
        } else {
            return CXError::create_result(format!(
                "Function '{}' is missing a return statement",
                env.builder.current_prototype().name
            ));
        }
    }

    env.builder.finish_function();
    env.pop_scope();
    Ok(())
}

pub fn typecheck(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ast: &CXAST,
) -> CXResult<()> {
    for stmt in ast.function_stmts.iter() {
        match stmt {
            CXFunctionStmt::FunctionDefinition { prototype, body } => {
                let prototype = env.complete_prototype(base_data, None, prototype)?;
                generate_function(env, base_data, prototype.clone(), body)?;
            }

            CXFunctionStmt::DestructorDefinition { _type, body } => {
                let cx_type = env.complete_type(base_data, _type)?;

                let Some(prototype) = env.get_destructor(base_data, &cx_type) else {
                    unreachable!("Destructor prototype should not be missing: {}", _type);
                };

                generate_function(env, base_data, prototype.clone(), body)?;
            }

            _ => {}
        }
    }

    Ok(())
}

pub fn realize_deconstructor(
    env: &mut TypeEnvironment,
    origin: &CompilationUnit,
    _type: MIRType,
) -> CXResult<()> {
    let base_data = env.module_data.base_mappings.get(origin);

    generate_deconstructor(env, base_data.as_ref(), _type)
}

pub fn realize_fn_implementation(
    env: &mut TypeEnvironment,
    origin: &CompilationUnit,
    template_kind: &CXFunctionKind,
    input: &CXTemplateInput,
) -> CXResult<()> {
    let base_ast = env.module_data.naive_ast.get(origin);
    let base_data = env.module_data.base_mappings.get(origin);

    let template = &base_data
        .fn_data
        .get_template(&template_kind.into_key())
        .expect("Template not found");
    let body = base_ast
        .function_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXFunctionStmt::TemplatedFunction { prototype, body }
                if prototype.kind == template.resource.shell.kind =>
            {
                Some(body)
            }
            _ => None,
        })
        .expect("Function template body not found");

    let overwrites = add_templated_types(env, &template.resource.prototype, input);
    let prototype = complete_function_template(env, base_data.as_ref(), template)?;

    env.in_external_templated_function = true;
    generate_function(env, base_data.as_ref(), prototype.clone(), body)?;
    env.in_external_templated_function = false;

    restore_template_overwrites(env, overwrites);
    Ok(())
}

pub fn complete_base_globals<'a>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
) -> CXResult<()> {
    for name in base_data.global_variables.keys() {
        global_expr(env, base_data, name.as_str())?;
    }

    Ok(())
}

pub fn complete_base_functions(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
) -> CXResult<()> {
    for (_, cx_fn) in base_data.fn_data.standard_iter() {
        env.complete_prototype(base_data, cx_fn.external_module.as_ref(), &cx_fn.resource)?;
    }

    Ok(())
}
