use cx_parsing_data::{
    ast::{CXAST, CXFunctionStmt},
    data::NaiveFnKind,
};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker_data::{
    ast::{TCBaseMappings, TCFunctionDef},
    cx_types::CXTemplateInput,
    function_map::CXFunctionKind,
};
use cx_util::CXResult;

use crate::{
    environment::TCEnvironment,
    type_checking::typechecker::{global_expr, in_method_env},
    type_completion::{
        complete_fn_prototype,
        templates::{add_templated_types, restore_template_overwrites},
    },
};

pub(crate) mod binary_ops;
pub(crate) mod casting;
pub(crate) mod move_semantics;
pub(crate) mod structured_initialization;
pub(crate) mod typechecker;

pub fn typecheck(env: &mut TCEnvironment, base_data: &TCBaseMappings, ast: &CXAST) -> CXResult<()> {
    for stmt in ast.function_stmts.iter() {
        match stmt {
            CXFunctionStmt::FunctionDefinition { prototype, body } => {
                let prototype = env.complete_prototype(base_data, None, prototype)?;

                let body = in_method_env(env, base_data, &prototype, body).unwrap_or_else(|| {
                    panic!("Failed to typecheck function body for {}", prototype.name)
                });

                env.declared_functions.push(TCFunctionDef {
                    prototype: prototype.clone(),
                    body: Box::new(body),
                })
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

                let body = in_method_env(env, base_data, &prototype, body)?;
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

    let overwrites = add_templated_types(env, &template.prototype, input);
    let mut prototype = env.complete_prototype(base_data.as_ref(), None, &template.shell)?;

    prototype.apply_template_mangling();

    env.in_external_templated_function = true;
    let tc_body = in_method_env(env, base_data.as_ref(), &prototype, &body)?;
    env.declared_functions.push(TCFunctionDef {
        prototype,
        body: Box::new(tc_body.clone()),
    });
    env.in_external_templated_function = false;

    restore_template_overwrites(env, overwrites);
    Some(())
}

pub fn complete_base_globals<'a>(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
) -> Option<()> {
    for name in base_data.global_variables.keys() {
        global_expr(env, base_data, name.as_str())?;
    }

    Some(())
}

pub fn complete_base_functions(env: &mut TCEnvironment, base_data: &TCBaseMappings) -> Option<()> {
    for (_, cx_fn) in base_data.fn_data.standard_iter() {
        complete_fn_prototype(
            env,
            base_data,
            cx_fn.external_module.as_ref(),
            &cx_fn.resource,
        )?;
    }

    Some(())
}
