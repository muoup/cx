use cx_parsing_data::{
    ast::{CXAST, CXExpr, CXFunctionStmt},
    data::CXFunctionKind,
};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker_data::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::{MIRBaseMappings, MIRFunction},
    types::{CXTemplateInput, MIRFunctionPrototype, MIRParameter, MIRType},
};
use cx_util::CXResult;

use crate::{
    environment::{TypeEnvironment, deconstruction::generate_deconstructor},
    type_checking::typechecker::{global_expr, typecheck_expr},
    type_completion::templates::{
        add_templated_types, complete_function_template, restore_template_overwrites,
    },
};

pub mod accumulation;
pub mod binary_ops;
pub mod casting;
pub mod r#match;
pub mod move_semantics;
pub mod structured_initialization;
pub mod typechecker;

fn typecheck_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: MIRFunctionPrototype,
    body: &CXExpr,
) -> CXResult<MIRFunction> {
    env.push_scope(None, None);

    for MIRParameter { name, _type } in prototype.params.iter() {
        let Some(name) = name else {
            continue;
        };

        env.insert_symbol(
            name.as_string(),
            MIRExpression {
                kind: MIRExpressionKind::Parameter(name.clone()),
                _type: _type.clone(),
            },
        );
    }

    let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
    env.pop_scope();

    Ok(MIRFunction {
        prototype,
        body: body_expr,
    })
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
                typecheck_function(env, base_data, prototype.clone(), body)?;
            }

            CXFunctionStmt::DestructorDefinition { _type, body } => {
                let cx_type = env.complete_type(base_data, _type)?;

                let Some(prototype) = env.get_destructor(base_data, &cx_type) else {
                    unreachable!("Destructor prototype should not be missing: {}", _type);
                };

                typecheck_function(env, base_data, prototype.clone(), body)?;
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
    typecheck_function(env, base_data.as_ref(), prototype.clone(), body)?;
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
