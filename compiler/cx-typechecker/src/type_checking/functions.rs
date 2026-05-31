use crate::{
    environment::TypeEnvironment,
    environment::symbols::templates::{
        add_templated_types, complete_function_template, restore_template_overwrites,
    },
    type_checking::{
        globals::complete_base_globals,
        typechecker::{add_implicit_return, typecheck_expr},
        value::ensure_valid_allocation_type,
    },
};
use cx_ast::{
    ast::{CXASTStmt, expression::CXExpression, function::CXFunctionKind},
    symbols::UntypedSymbol,
};
use cx_mir::mir::{
    data::{MIRFunctionPrototype, MIRParameter, MIRTemplateInput},
    expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
    program::{EnvironmentNamespace, MIRFunction},
};
use cx_pipeline_data::CompilationUnit;
use cx_util::{
    CXResult,
    namespace::{NamespacePath, QualifiedName},
};

pub fn typecheck_function(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: MIRFunctionPrototype,
    body: &CXExpression,
) -> CXResult<()> {
    env.function.begin_function(prototype.clone());
    env.push_scope(false, false);
    env.function.set_scope_anchor(body);
    env.function
        .configure_merge_scope(body, "function exit", Some("fallthrough"), true);

    for MIRParameter { name, _type } in prototype.signature.params.iter() {
        let Some(name) = name else {
            continue;
        };
        ensure_valid_allocation_type(env, Some(body.token_range().clone()), "a parameter", _type)?;
        let ref_type = env.symbols.mem_ref_to(_type.clone());

        env.symbols.insert_value(
            QualifiedName::new_raw(name.clone()),
            MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable {
                    name: name.clone(),
                    location: SymbolValueOrigin::Local,
                },
                _type: ref_type,
            },
        );
        if _type.is_nocopy() {
            env.function
                .track_binding(name.as_string(), _type.is_nodrop());
        }
    }

    let body_expr = typecheck_expr(env, namespace, body, None)?.into_expression()?;
    let with_implicit_return = add_implicit_return(env, namespace, body_expr)?;

    env.pop_scope()?;
    env.function.end_function();

    env.push_generated_function(MIRFunction {
        prototype,
        body: with_implicit_return,
    });

    Ok(())
}

pub fn realize_fn_implementation(
    env: &mut TypeEnvironment,
    origin: &CompilationUnit,
    template_kind: &CXFunctionKind,
    input: &MIRTemplateInput,
) -> CXResult<()> {
    let base_ast = env.source.module_data.generation_ast.get(origin);
    let namespace = NamespacePath::from_slash_path(origin.identifier());
    let template_key = template_kind.into_key();
    let Some(UntypedSymbol::FunctionTemplate(template, _)) =
        env.symbols.global_registry.resolve(&template_key)
    else {
        panic!("Template not found");
    };
    let body = base_ast
        .definition_stmts
        .iter()
        .find_map(|stmt| match stmt {
            CXASTStmt::TemplatedFunction {
                prototype, body, ..
            } if prototype.kind == template.resource.shell.kind => Some(body),
            _ => None,
        })
        .expect("Function template body not found");

    let overwrites = add_templated_types(env, &template.resource.prototype, input)?;
    let prototype = complete_function_template(env, &namespace, &template)?;

    let old_external_template = env.items.in_external_templated_function;
    let old_external_origin = env.items.external_template_origin.clone();
    let external_origin = if origin.as_str() == env.source.compilation_unit.as_str() {
        None
    } else {
        Some(origin.identifier().to_string())
    };

    // FIXME: This looks like a mess
    env.set_external_templated_function(external_origin.is_some());
    env.set_external_template_origin(external_origin);
    let typecheck_result = typecheck_function(env, &namespace, prototype.clone(), body);
    env.set_external_templated_function(old_external_template);
    env.set_external_template_origin(old_external_origin);
    typecheck_result?;

    restore_template_overwrites(env, overwrites);
    Ok(())
}

pub fn complete_base_functions(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
) -> CXResult<()> {
    let _ = (env, namespace);
    Ok(())
}
