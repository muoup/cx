use cx_data_lexer::token::Token;
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::preparse::naive_types::CXNaiveTemplateInput;
use cx_data_typechecker::ast::{TCFunctionDef, IPTCAST};
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXParameter, CXTemplateInput, CXType};
use cx_data_typechecker::TCEnvironment;
use cx_util::log_error;
use cx_util::mangling::mangle_destructor;
use cx_util::scoped_map::ScopedMap;
use crate::type_mapping::contextualize_fn_prototype;
use crate::typechecker::{cleanup_method_env, setup_method_env, typecheck_expr};

mod casting;
mod typechecker;
mod binary_ops;

pub(crate) mod templates;
pub(crate) mod structured_initialization;

pub mod type_mapping;
pub mod precontextualizing;

pub fn typecheck(mut env: TCEnvironment, ast: &CXAST) -> Option<IPTCAST> {
    let mut statements = Vec::new();

    for stmt in ast.global_stmts.iter() {
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                let prototype = contextualize_fn_prototype(&mut env, prototype)?;
                setup_method_env(&mut env, &prototype);
                let tc_stmt = typecheck_expr(&mut env, body)?;
                cleanup_method_env(&mut env);

                statements.push(
                    TCFunctionDef {
                        prototype: prototype.clone(),
                        body: Box::new(tc_stmt),
                    }
                )
            },

            CXGlobalStmt::DestructorDefinition { type_name, body } => {
                let Some(_type) = env.type_map.get(type_name) else {
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

                setup_method_env(&mut env, &prototype);
                let tc_stmt = typecheck_expr(&mut env, body)?;
                cleanup_method_env(&mut env);

                statements.push(
                    TCFunctionDef {
                        prototype: prototype.clone(),
                        body: Box::new(tc_stmt),
                    }
                )
            },

            _ => {}
        }
    }

    Some(
        IPTCAST {
            source_file: ast.file_path.clone(),
            type_map: env.type_map,
            fn_map: env.fn_map,
            function_defs: statements,
        }
    )
}

pub fn realize_fn_prototype(origin: (&CXAST, &IPTCAST), input: &CXTemplateInput, prototype: &CXFunctionPrototype)
                            -> Option<TCFunctionDef> {
    let (cx, tc) = origin;
    
    let mut type_map = tc.type_map.clone();
    let fn_map = tc.fn_map.clone();
    
    let args = &tc.fn_map
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
        type_map, fn_map,

        current_function: None,
        symbol_table: ScopedMap::new(),
    };
    
    let prototype_name = prototype.name.as_string();

    let body = cx.global_stmts.iter()
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