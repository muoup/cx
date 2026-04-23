use crate::{
    environment::symbols::templates::complete_function_template,
    type_checking::typechecker::add_implicit_return,
};
use cx_ast::{
    ast::{CXAST, CXExpr, CXFunctionStmt},
    data::CXFunctionKind,
};
use cx_mir::mir::{
    data::{MIRFunctionPrototype, MIRParameter, MIRTemplateInput},
    expression::{MIRExpression, MIRExpressionKind},
    program::{MIRBaseMappings, MIRFunction},
};
use cx_pipeline_data::CompilationUnit;
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    environment::symbols::templates::{add_templated_types, restore_template_overwrites},
    type_checking::typechecker::{global_expr, typecheck_expr},
};

pub mod binary_ops;
pub mod casting;
pub mod coercion;
pub mod r#match;
pub mod op;
pub mod result;
pub mod structured_initialization;
pub mod typechecker;

fn typecheck_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: MIRFunctionPrototype,
    body: &CXExpr,
) -> CXResult<()> {
    env.begin_function(prototype.clone());
    env.push_scope(false, false);
    env.set_scope_anchor(body);
    env.configure_merge_scope(body, "function exit", Some("fallthrough"), true);

    for MIRParameter { name, _type } in prototype.params.iter() {
        let Some(name) = name else {
            continue;
        };
        let ref_type = env.symbols.context.mem_ref_to(_type.clone());

        env.insert_symbol(
            name.as_string(),
            MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable(name.clone()),
                _type: ref_type,
            },
        );
        env.track_binding(name.as_string(), _type);
    }

    let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
    let with_implicit_return = add_implicit_return(env, body_expr)?;

    env.pop_scope()?;
    env.end_function();

    env.push_generated_function(MIRFunction {
        prototype,
        body: with_implicit_return,
    });

    Ok(())
}

pub fn typecheck(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ast: &CXAST,
) -> CXResult<()> {
    for stmt in ast.function_stmts.iter() {
        if let CXFunctionStmt::FunctionDefinition { prototype, body } = stmt {
            let prototype = env.complete_prototype(base_data, None, prototype)?;
            typecheck_function(env, base_data, prototype.clone(), body)?;
        }
    }

    Ok(())
}

pub fn realize_fn_implementation(
    env: &mut TypeEnvironment,
    origin: &CompilationUnit,
    template_kind: &CXFunctionKind,
    input: &MIRTemplateInput,
) -> CXResult<()> {
    let base_ast = env.source.module_data.naive_ast.get(origin);
    let base_data = env.source.module_data.base_mappings.get(origin);

    let template = &base_data
        .fn_data
        .get_template(&template_kind.into_key())
        .expect("Template not found");
    let body = base_ast
        .function_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXFunctionStmt::TemplatedFunction {
                prototype, body, ..
            } if prototype.kind == template.resource.shell.kind => Some(body),
            _ => None,
        })
        .expect("Function template body not found");

    let overwrites = add_templated_types(env, &template.resource.prototype, input)?;
    let prototype = complete_function_template(env, base_data.as_ref(), template)?;

    env.set_external_templated_function(true);
    typecheck_function(env, base_data.as_ref(), prototype.clone(), body)?;
    env.set_external_templated_function(false);

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
