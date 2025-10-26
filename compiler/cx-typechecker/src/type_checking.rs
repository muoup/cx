use cx_parsing_data::{parse::ast::{CXFunctionStmt, CXAST}, preparse::templates::CXFunctionTemplate};
use cx_typechecker_data::{ast::{TCBaseMappings, TCFunctionDef}, cx_types::CXTemplateInput, function_map::CXFunctionKind};
use cx_util::CXResult;

use crate::{environment::TCEnvironment, type_checking::typechecker::{global_expr, in_method_env}, type_completion::{complete_prototype, templates::{add_templated_types, restore_template_overwrites}}};

pub(crate) mod binary_ops;
pub(crate) mod casting;
pub(crate) mod move_semantics;
pub(crate) mod structured_initialization;
pub(crate) mod typechecker;

pub fn typecheck(env: &mut TCEnvironment, ast: &CXAST) -> CXResult<()> {
    for stmt in ast.function_stmts.iter() {
        match stmt {
            CXFunctionStmt::FunctionDefinition { prototype, body } => {
                let prototype = env.complete_prototype(&prototype)?;
                
                let body = in_method_env(env, &prototype, body).unwrap_or_else(|| {
                    panic!("Failed to typecheck function body for {}", prototype.name)
                });

                env.declared_functions.push(TCFunctionDef {
                    prototype: prototype.clone(),
                    body: Box::new(body),
                })
            }

            CXFunctionStmt::DestructorDefinition { _type, body } => {
                let cx_type = env.complete_type(_type)?;
                let Some(type_name) = cx_type.get_identifier() else {
                    unreachable!("Destructor type should be known: {}", _type);
                };

                let ident = CXFunctionKind::Destructor {
                    base_type: type_name.clone(),
                };

                let Some(prototype) = env.get_realized_func(&ident.into()) else {
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
    let mut prototype = env.complete_prototype(&template.shell)?;

    let body = origin
        .function_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXFunctionStmt::TemplatedFunction {
                prototype, body, ..
            } if prototype.name == template.shell.name => Some(body),
            _ => None,
        })
        .unwrap_or_else(|| {
            println!("Available: {:#?}", origin.function_stmts);

            panic!(
                "Function template body not found for {}",
                template.shell.name
            );
        })
        .as_ref()
        .clone();

    prototype.apply_template_mangling();

    env.in_external_templated_function = true;
    let tc_body = in_method_env(env, &prototype, &body)?;
    env.declared_functions.push(TCFunctionDef {
        prototype,
        body: Box::new(tc_body.clone()),
    });
    env.in_external_templated_function = false;

    restore_template_overwrites(env, overwrites);

    // Restore the original base data to avoid lifetime issues. (See above safety comment.)
    env.base_data = old_base;
    Some(())
}

pub fn complete_base_globals<'a>(
    env: &mut TCEnvironment
) -> Option<()> {
    for name in env.base_data.global_variables.keys() {
        global_expr(env, name.as_str())?;
    }

    Some(())
}

pub fn complete_base_functions(
    env: &mut TCEnvironment
) -> Option<()> {
    for (_, cx_fn) in env.base_data.fn_data.standard_iter() {
        complete_prototype(env, env.base_data, cx_fn.external_module.as_ref(), &cx_fn.resource)?;
    }

    Some(())
}