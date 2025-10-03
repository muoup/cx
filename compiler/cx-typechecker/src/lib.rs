use crate::environment::TCEnvironment;
use crate::templates::{add_templated_types, restore_template_overwrites};
use crate::type_mapping::contextualize_fn_prototype;
use crate::typechecker::in_method_env;
use cx_parsing_data::parse::ast::{CXAST, CXGlobalStmt};
use cx_parsing_data::preparse::templates::CXFunctionTemplate;
use cx_typechecker_data::ast::{TCBaseMappings, TCFunctionDef};
use cx_typechecker_data::cx_types::CXTemplateInput;
use cx_util::mangling::{mangle_destructor, mangle_template};

mod binary_ops;
mod casting;
mod typechecker;

pub(crate) mod structured_initialization;
pub(crate) mod templates;
pub(crate) mod variable_destruction;

pub mod environment;

mod log;
pub mod precontextualizing;
pub mod type_mapping;

pub fn typecheck(env: &mut TCEnvironment, ast: &CXAST) -> Option<()> {
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
                let destructor = mangle_destructor(_type);
                let Some(prototype) = env.get_func(&destructor) else {
                    unreachable!("Destructor prototype should not be missing: {}", destructor);
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
) -> Option<()> {
    let old_base = env.base_data;

    unsafe {
        env.base_data = std::mem::transmute::<&cx_typechecker_data::ast::TCBaseMappings, &cx_typechecker_data::ast::TCBaseMappings>(structure_data);
    }

    let overwrites = add_templated_types(env, &template.prototype, input);

    let template_name = template.shell.name.mangle();
    let mangled_name = mangle_template(&template_name, &input.args);
    let prototype = env.get_func(&mangled_name)?.clone();

    let body = origin
        .global_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXGlobalStmt::TemplatedFunction {
                prototype, body, ..
            } if prototype.name.mangle() == template_name => Some(body),
            _ => None,
        })
        .unwrap_or_else(|| {
            panic!("Function template body not found for {template_name}");
        })
        .as_ref()
        .clone();

    let tc_body = in_method_env(env, &prototype, &body)?;
    env.declared_functions.push(TCFunctionDef {
        prototype: prototype.clone(),
        body: Box::new(tc_body.clone()),
    });

    restore_template_overwrites(env, overwrites);

    unsafe {
        *(&mut env.base_data as *mut &_) = old_base;
    }
    Some(())
}
