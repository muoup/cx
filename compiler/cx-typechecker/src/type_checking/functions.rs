use crate::{
    environment::TypeEnvironment,
    type_checking::{
        typechecker::{add_implicit_return, typecheck_expr},
        value::ensure_valid_allocation_type,
    },
};
use cx_ast::{
    ast::{expression::CXExpression, function::CXFunctionKind},
    symbols::UntypedSymbolKind,
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

    let body_expr = typecheck_expr(env, namespace, body, None)
        .and_then(|v| v.standard_ready_coerce(env, body.token_range()))?;
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

    let Some(UntypedSymbolKind::FunctionTemplate {
        input,
        definition,
        body,
    }) = env
        .symbols
        .global_registry
        .resolve(&template_key)
        .map(|sym| &sym.kind)
    else {
        unreachable!("Template not found");
    };

    env.push_scope(false, false);

    // let overwrites = add_templated_types(env, &template.resource.prototype, input)?;
    // let prototype = complete_function_template(env, &namespace, &template)?;

    let old_external_template = env.items.in_external_templated_function;
    let old_external_origin = env.items.external_template_origin.clone();
    let external_origin = if origin.as_str() == env.source.compilation_unit.as_str() {
        None
    } else {
        Some(origin.identifier().to_string())
    };

    // FIXME: This looks like a mess
    // env.set_external_templated_function(external_origin.is_some());
    // env.set_external_template_origin(external_origin);
    let typecheck_result = typecheck_function(env, &namespace, prototype.clone(), body);
    // env.set_external_templated_function(old_external_template);
    // env.set_external_template_origin(old_external_origin);
    typecheck_result?;

    env.pop_scope();

    // restore_template_overwrites(env, overwrites);
    Ok(())
}

pub fn complete_base_functions(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
) -> CXResult<()> {
    let _ = (env, namespace);
    Ok(())
}
