use crate::{
    environment::TypeEnvironment, symbol::completion::complete_prototype, type_checking::{
        typechecker::{add_implicit_return, typecheck_expr},
        value::ensure_valid_allocation_type,
    }
};
use cx_ast::{
    ast::{expression::CXExpression, function::CXFunctionKind},
    symbols::CXSymbolKind,
};
use cx_mir::{EnvironmentNamespace, mir::{
    data::{MIRFunction, MIRFunctionPrototype, MIRParameter, MIRTemplateInput},
    expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
}};
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

    env.items.push_generated_function(MIRFunction {
        prototype,
        body: with_implicit_return,
    });

    Ok(())
}

pub fn realize_fn_implementation(
    env: &mut TypeEnvironment,
    origin: &CompilationUnit,
    template_kind: &CXFunctionKind,
    _input: &MIRTemplateInput,
) -> CXResult<()> {
    let _base_ast = env.source.module_data.generation_ast.get(origin);
    let namespace = NamespacePath::from_slash_path(origin.identifier());
    let template_key = template_kind.into_key();

    let Some(CXSymbolKind::FunctionTemplate {
        input: _template_input,
        definition,
        body,
    }) = env
        .symbols
        .get_global_registry()
        .resolve(&template_key)
        .map(|sym| sym.kind)
    else {
        unreachable!("Template not found");
    };

    env.push_scope(false, false);

    // FIXME: This looks like a mess
    let prototype = complete_prototype(&mut env.symbols, &namespace, &definition)?;
    let typecheck_result = typecheck_function(env, &namespace, prototype, &body);
    typecheck_result?;

    env.pop_scope()?;
    Ok(())
}

pub fn complete_base_functions(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
) -> CXResult<()> {
    let _ = (env, namespace);
    Ok(())
}
