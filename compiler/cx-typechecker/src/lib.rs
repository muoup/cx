use crate::environment::TCEnvironment;
use crate::expr_checking::typechecker::in_method_env;
use crate::type_completion::precontextualizing::{contextualize_fn_map, contextualize_globals, contextualize_type_map};
use crate::type_completion::templates::{add_templated_types, restore_template_overwrites};
use crate::type_completion::type_mapping::{contextualize_fn_prototype, contextualize_type};
use cx_parsing_data::parse::ast::{CXAST, CXGlobalStmt};
use cx_parsing_data::preparse::templates::CXFunctionTemplate;
use cx_pipeline_data::GlobalCompilationContext;
use cx_typechecker_data::ast::{TCBaseMappings, TCFunctionDef};
use cx_typechecker_data::cx_types::CXTemplateInput;
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_util::CXResult;

mod log;

pub mod expr_checking;
pub mod type_completion;

pub mod environment;

pub fn create_base_types(context: &GlobalCompilationContext, cx_ast: &CXAST) -> CXResult<TCBaseMappings> {
    let mut type_data = contextualize_type_map(&context.module_db, &cx_ast.type_map)
        .expect("Failed to contextualize type map");
    
    let fn_data = contextualize_fn_map(
        &context.module_db,
        &cx_ast.function_map,
        &mut type_data,
        &cx_ast.type_map,
    )
    .expect("Failed to contextualize function map");
    let global_variables = contextualize_globals(
        &context.module_db,
        &mut type_data,
        &cx_ast.type_map,
        &cx_ast,
    )
    .expect("Failed to contextualize global variables");

    Some(
        TCBaseMappings {
            type_data,
            fn_map: fn_data,
            global_variables,
        }
    )
}

pub fn typecheck(env: &mut TCEnvironment, ast: &CXAST) -> CXResult<()> {
    for stmt in ast.global_stmts.iter() {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                let prototype = contextualize_fn_prototype(env, prototype)?;
                let body = in_method_env(env, &prototype, body).unwrap_or_else(|| {
                    panic!("Failed to typecheck function body for {}", prototype.name)
                });

                env.declared_functions.push(TCFunctionDef {
                    prototype: prototype.clone(),
                    body: Box::new(body),
                })
            }

            CXGlobalStmt::DestructorDefinition { _type, body } => {
                let cx_type = contextualize_type(env, _type)?;
                let Some(type_name) = cx_type.get_identifier() else {
                    unreachable!("Destructor type should be known: {}", _type);
                };
                
                let ident = CXFunctionKind::Destructor {
                    base_type: type_name.clone(),
                };
                
                let Some(prototype) = env.get_func(&ident) else {
                    unreachable!("Destructor prototype should not be missing: {}", _type);
                };

                let body = in_method_env(env, &prototype, body)?;
                env.declared_functions.push(TCFunctionDef {
                    prototype,
                    body: Box::new(body),
                })
            }

            _ => {}
        }
    }

    Some(())
}

pub fn realize_fn_implementation(
    env: &mut TCEnvironment,
    structure_data: &TCBaseMappings,
    origin: &CXAST,
    template: &CXFunctionTemplate,
    input: &CXTemplateInput,
) -> CXResult<()> {
    let old_base = env.base_data;

    // SAFETY: The lifetimes will technically be unsound here, but the unsoundness only matters
    // if the reference here escapes the function, which we ensure it does not. The lifetime
    // of structure_data will not outlive env's underlying struct, but will outlive its existence
    // in the struct, which is sufficient for this constrained context.
    env.base_data = unsafe { std::mem::transmute(structure_data) };

    let overwrites = add_templated_types(env, &template.prototype, input);
    let mut prototype = contextualize_fn_prototype(env, &template.shell)?;
    
    let body = origin
        .global_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXGlobalStmt::TemplatedFunction {
                prototype, body, ..
            } if prototype.name == template.shell.name => Some(body),
            _ => None,
        })
        .unwrap_or_else(|| {
            println!("Available: {:#?}", origin.global_stmts);
            
            panic!("Function template body not found for {}", template.shell.name);
        })
        .as_ref()
        .clone();
   
    prototype.apply_template_mangling();
    
    let tc_body = in_method_env(env, &prototype, &body)?;
    env.declared_functions.push(TCFunctionDef {
        prototype: prototype.clone(),
        body: Box::new(tc_body.clone()),
    });

    restore_template_overwrites(env, overwrites);

    env.base_data = old_base;
    Some(())
}
