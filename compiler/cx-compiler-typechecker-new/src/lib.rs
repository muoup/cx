use std::collections::HashSet;
use cx_data_lexer::token::Token;
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::preparse::naive_types::CXNaiveTemplateInput;
use cx_data_typechecker::ast::{TCFunctionDef, TCStructureData, TCAST};
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXParameter, CXTemplateInput, CXType};
use cx_data_typechecker::{CXFnMap, CXTypeMap};
use cx_util::log_error;
use cx_util::mangling::mangle_destructor;
use cx_util::scoped_map::ScopedMap;
use crate::environment::TCEnvironment;
use crate::type_mapping::contextualize_fn_prototype;
use crate::typechecker::{cleanup_method_env, in_method_env, setup_method_env, typecheck_expr};

mod casting;
mod typechecker;
mod binary_ops;

pub(crate) mod templates;
pub(crate) mod structured_initialization;
pub(crate) mod variable_destruction;

pub mod environment;

pub mod type_mapping;
pub mod precontextualizing;

pub fn typecheck(env: &mut TCEnvironment, ast: &CXAST) -> Option<TCAST> {
    let mut statements = Vec::new();

    for stmt in ast.global_stmts.iter() {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                let prototype = contextualize_fn_prototype(env, prototype)?;
                let body = in_method_env(env, &prototype, body)?;

                statements.push(
                    TCFunctionDef {
                        prototype: prototype.clone(),
                        body: Box::new(body),
                    }
                )
            },

            CXGlobalStmt::DestructorDefinition { type_name, body } => {
                let Some(_type) = env.type_data.get(type_name) else {
                    log_error!("Destructor defined for unknown type: {type_name}");
                };

                let prototype = CXFunctionPrototype {
                    return_type: CXType::unit(),
                    name: CXIdent::from(mangle_destructor(type_name)),
                    params: vec![CXParameter {
                        name: Some(CXIdent::from("this")),
                        _type: _type.clone().pointer_to(),
                    }],
                    var_args: false,
                    needs_buffer: false,
                };

                let final_body = in_method_env(env, &prototype, body)?;

                env.fn_data.insert_standard(
                    prototype.name.to_string(),
                    prototype.clone()
                );
                statements.push(
                    TCFunctionDef {
                        prototype: prototype.clone(),
                        body: Box::new(final_body),
                    }
                )
            },

            _ => {}
        }
    }

    Some(
        TCAST {
            source_file: ast.file_path.clone(),

            type_map: CXTypeMap::new(),
            fn_map: CXFnMap::new(),
            destructors_required: Vec::new(),

            function_defs: statements,
        }
    )
}

pub fn realize_fn_prototype(env: &mut TCEnvironment, origin: &CXAST, input: &CXTemplateInput, prototype: &CXFunctionPrototype)
                            -> Option<TCFunctionDef> {
    let mut type_map = env.type_data.clone();
    let fn_map = env.fn_data.clone();

    let args = &env.fn_data
        .get_template(&prototype.name.as_string())
        .unwrap_or_else(|| {
            panic!("Template generator not found in type map for {}", &prototype.name.as_string())
        })
        .template
        .resource
        .prototype
        .types;

    for (name, _type) in args.iter().zip(&input.args) {
        type_map.insert_standard(name.clone(), _type.clone());
    }

    let mut env = TCEnvironment {
        type_data: type_map,
        fn_data: fn_map,

        requests: Vec::new(),
        deconstructors: HashSet::new(),

        current_function: None,
        symbol_table: ScopedMap::new(),
    };
    
    let prototype_name = prototype.name.as_string();

    let body = origin.global_stmts.iter()
        .find_map(
            |stmt| match stmt {
                CXGlobalStmt::TemplatedFunction { prototype, body, .. }
                    if prototype.name.as_string() == prototype_name => Some(body),
                _ => None,
            }
        )
        .unwrap_or_else(|| {
            panic!("Function template body not found for {}", prototype_name);
        })
        .as_ref()
        .clone();

    let tc_body = typecheck_expr(&mut env, &body)?;

    Some(TCFunctionDef { prototype: prototype.clone(), body: Box::new(tc_body) })
}