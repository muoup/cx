use crate::environment::TCEnvironment;
use crate::type_mapping::contextualize_fn_prototype;
use crate::typechecker::{in_method_env, typecheck_global_variable};
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_util::identifier::CXIdent;
use cx_data_ast::preparse::templates::CXFunctionTemplate;
use cx_data_typechecker::ast::{TCFunctionDef, TCGlobalVariable, TCStructureData};
use cx_data_typechecker::cx_types::CXTemplateInput;
use cx_util::mangling::{mangle_destructor, mangle_template};

mod casting;
mod typechecker;
mod binary_ops;

pub(crate) mod templates;
pub(crate) mod structured_initialization;
pub(crate) mod variable_destruction;

pub mod environment;

pub mod type_mapping;
pub mod precontextualizing;

pub fn typecheck(env: &mut TCEnvironment, ast: &CXAST) -> Option<()> {
    for (name, constant) in ast.enum_constants.iter() {
        env.global_variables.insert(
            name.clone(),
            TCGlobalVariable::UnaddressableConstant {
                name: CXIdent::from(name.clone()),
                val: *constant,
            }
        );
    }

    for stmt in ast.global_stmts.iter() {
        match stmt {
            CXGlobalStmt::GlobalVariable { name, type_, initializer } =>
                typecheck_global_variable(env, name.as_str(), type_, initializer)?,

            _ => ()
        }
    }

    for stmt in ast.global_stmts.iter() {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                let prototype = contextualize_fn_prototype(env, prototype)?;
                let body = in_method_env(env, &prototype, body)
                    .unwrap_or_else(|| panic!("Failed to typecheck function body for {}", prototype.name));

                env.declared_functions.push(
                    TCFunctionDef {
                        prototype: prototype.clone(),
                        body: Box::new(body),
                    }
                )
            },

            CXGlobalStmt::DestructorDefinition { _type, body } => {
                let destructor = mangle_destructor(_type);
                let Some(prototype) = env.get_func(&destructor).cloned() else {
                    unreachable!("Destructor prototype should not be missing: {}", destructor);
                };

                let body = in_method_env(env, &prototype, body)?;
                env.declared_functions.push(
                    TCFunctionDef {
                        prototype, body: Box::new(body),
                    }
                )
            },

            _ => {}
        }
    }

    Some(())
}

pub fn realize_fn_implementation(
    parent_env: &mut TCEnvironment,
    structure_data: &TCStructureData, origin: &CXAST,
    template: &CXFunctionTemplate, input: &CXTemplateInput
) -> Option<()> {
    let mut template_env = TCEnvironment::new(structure_data.clone());
    template_env.deconstructors = std::mem::take(&mut parent_env.deconstructors);

    let args = &template
        .prototype
        .types;

    for (name, _type) in args.iter().zip(&input.args) {
        template_env.type_data.insert_standard(name.clone(), _type.clone());
    }

    let template_name = template.shell.name.mangle();
    let mangled_name = mangle_template(&template_name, &input.args);
    let prototype = parent_env.fn_data.get(&mangled_name)?.clone();

    let body = origin.global_stmts.iter()
        .find_map(
            |stmt| match stmt {
                CXGlobalStmt::TemplatedFunction { prototype, body, .. }
                    if prototype.name.mangle() == template_name => Some(body),
                _ => None,
            }
        )
        .unwrap_or_else(|| {
            panic!("Function template body not found for {}", template_name);
        })
        .as_ref()
        .clone();

    let tc_body = in_method_env(&mut template_env, &prototype, &body)?;
    let function_def = TCFunctionDef {
        prototype, body: Box::new(tc_body.clone()),
    };

    parent_env.deconstructors = template_env.deconstructors;
    parent_env.global_variables.extend(template_env.global_variables);
    parent_env.declared_functions.push(function_def);

    Some(())
}