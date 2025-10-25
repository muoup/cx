use cx_parsing_data::{parse::ast::{CXGlobalStmt, CXAST}, preparse::templates::CXFunctionTemplate};
use cx_typechecker_data::{ast::{TCBaseMappings, TCFunctionDef}, cx_types::CXTemplateInput, function_map::CXFunctionKind};
use cx_util::CXResult;

use crate::{environment::TCEnvironment, type_checking::typechecker::in_method_env, type_completion::templates::{add_templated_types, restore_template_overwrites}};

pub(crate) mod binary_ops;
pub(crate) mod casting;
pub(crate) mod move_semantics;
pub(crate) mod structured_initialization;
pub(crate) mod typechecker;

pub fn typecheck(env: &mut TCEnvironment, ast: &CXAST) -> CXResult<()> {
    for stmt in ast.global_stmts.iter() {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                let prototype = env.complete_prototype(&prototype)?;
                
                let body = in_method_env(env, &prototype, body).unwrap_or_else(|| {
                    panic!("Failed to typecheck function body for {}", prototype.name)
                });

                env.declared_functions.push(TCFunctionDef {
                    prototype: prototype.clone(),
                    body: Box::new(body),
                })
            }

            CXGlobalStmt::DestructorDefinition { _type, body } => {
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

            panic!(
                "Function template body not found for {}",
                template.shell.name
            );
        })
        .as_ref()
        .clone();

    prototype.apply_template_mangling();

    let tc_body = in_method_env(env, &prototype, &body)?;
    env.declared_functions.push(TCFunctionDef {
        prototype,
        body: Box::new(tc_body.clone()),
    });

    restore_template_overwrites(env, overwrites);

    // Restore the original base data to avoid lifetime issues. (See above safety comment.)
    env.base_data = old_base;
    Some(())
}