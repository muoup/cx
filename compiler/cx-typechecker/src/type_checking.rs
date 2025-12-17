use cx_parsing_data::{
    ast::{CXAST, CXExpr, CXFunctionStmt},
    data::NaiveFnKind,
};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker_data::{
    function_map::CXFunctionKind,
    mir::{
        expression::{MIRInstruction, MIRValue},
        program::MIRBaseMappings,
        types::{CXFunctionPrototype, CXIntegerType, CXTemplateInput, TCParameter},
    },
};
use cx_util::{CXError, CXResult};

use crate::{
    environment::TypeEnvironment,
    type_checking::typechecker::{global_expr, typecheck_expr},
    type_completion::{
        complete_fn_prototype,
        templates::{add_templatedtype_s, restore_template_overwrites},
    },
};

pub(crate) mod binary_ops;
pub(crate) mod casting;
pub(crate) mod contract;
pub(crate) mod r#match;
pub(crate) mod structured_initialization;
pub(crate) mod typechecker;
pub(crate) mod move_semantics;

fn generate_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: CXFunctionPrototype,
    body: &CXExpr,
) -> CXResult<()> {
    env.push_scope(None, None);
    env.builder.start_function(prototype.clone());
    env.arg_vals.clear();

    for TCParameter { name, _type } in prototype.params.iter() {
        let Some(name) = name else {
            continue;
        };

        if _type.is_memory_resident() {
            env.insert_symbol(
                name.as_string(),
                MIRValue::Parameter { name: name.clone(), _type: _type.clone().mem_ref_to() },
            );
        } else {
            let region = env.builder.new_register();
            env.builder
                .add_instruction(MIRInstruction::CreateStackRegion {
                    result: region.clone(),
                    _type: _type.clone(),
                });

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
        } else if env.builder.current_prototype().mangle_name() == "main" {
            env.builder.add_return(Some(MIRValue::IntLiteral {
                value: 0,
                signed: true,
                _type: CXIntegerType::I32,
            }));
        } else {
            return CXError::create_result(format!(
                "Function '{}' is missing a return statement",
                env.builder.current_prototype().mangle_name()
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
                let Some(type_name) = cx_type.get_identifier() else {
                    unreachable!("Destructor type should be known: {}", _type);
                };

                let ident = CXFunctionKind::Destructor {
                    base_type: type_name.clone(),
                };

                let Some(prototype) = env.get_realized_func(&ident.into()) else {
                    unreachable!("Destructor prototype should not be missing: {}", _type);
                };

                generate_function(env, base_data, prototype.clone(), body)?;
            }

            _ => {}
        }
    }

    Ok(())
}

pub fn realize_fn_implementation(
    env: &mut TypeEnvironment,
    origin: &CompilationUnit,
    template_name: &NaiveFnKind,
    input: &CXTemplateInput,
) -> CXResult<()> {
    let base_ast = env.module_data.naive_ast.get(origin);
    let base_data = env.module_data.base_mappings.get(origin);

    let template = &base_data
        .fn_data
        .get_template(&template_name.into())
        .expect("Template not found")
        .resource;
    let body = base_ast
        .function_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXFunctionStmt::TemplatedFunction { prototype, body }
                if prototype.name == template.shell.name =>
            {
                Some(body)
            }
            _ => None,
        })
        .expect("Function template body not found");

    let overwrites = add_templatedtype_s(env, &template.prototype, input);
    let mut prototype = env.complete_prototype(base_data.as_ref(), None, &template.shell)?;

    prototype.apply_template_mangling();

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
        complete_fn_prototype(
            env,
            base_data,
            cx_fn.external_module.as_ref(),
            &cx_fn.resource,
        )?;
    }

    Ok(())
}
