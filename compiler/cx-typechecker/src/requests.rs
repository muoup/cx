use cx_hir::{
    ast::function::HIRFunctionContract,
    symbols::{HIRSymbolData, HIRSymbolKind},
};
use cx_log::CXResult;
use cx_thir::type_context::THIRTypeContext;
use cx_thir::{
    NamespacePath,
    thir::{
        data::{
            THIRComptimeFnPrototype, THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter,
            THIRTemplateInput,
        },
        expression::{THIRExpression, THIRExpressionKind},
        r#type::THIRType,
    },
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode, module::QualifiedName};

use crate::{
    environment::{THIRFunctionGenRequest, TypeEnvironment},
    symbol::{
        name_mangling::base_mangle_templated_name,
        resolution::{apply_template_input, symbol_lexical_namespace},
    },
    type_checking::functions::{typecheck_comptime_function, typecheck_function},
};

pub fn fulfill_requests(env: &mut TypeEnvironment) -> CXResult<()> {
    while let Some(request) = env.items.pop_request() {
        match request {
            THIRFunctionGenRequest::TypeConstructor {
                symbol_name,
                debug_name,
                union_type,
                variant_type,
                variant_index,
            } => realize_tagged_union_constructor(
                env,
                symbol_name,
                debug_name,
                union_type,
                variant_type,
                variant_index,
            ),

            THIRFunctionGenRequest::Template {
                name,
                prototype,
                input,
            } => realize_fn_template(env, &name, prototype, &input)?,

            THIRFunctionGenRequest::Comptime {
                name,
                prototype,
                ref input,
            } => realize_comptime_fn_template(env, &name, prototype, &input)?,
        }
    }

    Ok(())
}

fn realize_tagged_union_constructor(
    env: &mut TypeEnvironment,
    symbol_name: String,
    debug_name: CXIdent,
    union_type: THIRType,
    variant_type: THIRType,
    variant_index: usize,
) {
    if env.items.request_fulfilled(symbol_name.as_str()) {
        return;
    }
    env.items.mark_request_fulfilled(symbol_name.clone());

    let param_name = CXIdent::new("value");
    let param_local_id = cx_thir::thir::expression::THIRLocalID::fresh();
    let prototype = THIRFnPrototype::new(
        symbol_name,
        LinkageMode::Static,
        THIRFnSignature {
            return_type: union_type.clone(),
            params: if variant_type.is_void() {
                Vec::new()
            } else {
                vec![THIRParameter {
                    name: Some(param_name.clone()),
                    local_id: param_local_id,
                    _type: variant_type.clone(),
                }]
            },
            var_args: false,
            contract: HIRFunctionContract::default(),
        },
    )
    .with_debug_name(debug_name);

    let value = if variant_type.is_void() {
        THIRExpression {
            token_range: TokenRange::internal(),
            _type: variant_type.clone(),
            kind: THIRExpressionKind::Unit,
        }
    } else {
        THIRExpression {
            token_range: TokenRange::internal(),
            _type: variant_type.clone(),
            kind: THIRExpressionKind::Move {
                name: param_name,
                local_id: param_local_id,
            },
        }
    };

    let constructed = THIRExpression {
        token_range: TokenRange::internal(),
        _type: union_type.clone(),
        kind: THIRExpressionKind::TaggedUnionInitializer {
            variant_index,
            value: Box::new(value),
            sum_type: union_type,
        },
    };
    let body = THIRExpression {
        token_range: TokenRange::internal(),
        _type: prototype.signature().return_type.clone(),
        kind: THIRExpressionKind::Return {
            value: Some(Box::new(constructed)),
            postcondition: None,
        },
    };

    env.items.push_generated_function(THIRFunction {
        prototype,
        body: Some(body),
    });
}

fn realize_fn_template(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    prototype: THIRFnPrototype,
    input: &THIRTemplateInput,
) -> CXResult<()> {
    let resolution = env
        .symbols
        .get_global_registry()
        .resolve(name)
        .unwrap_or_else(|| {
            unreachable!(
                "Expected function template '{}' to be present in the symbol registry",
                name
            )
        });

    let stmt = resolution
        .declarations()
        .iter()
        .find(|symbol| {
            matches!(
                symbol.kind,
                HIRSymbolKind::Function(HIRSymbolData::Template { .. })
            )
        })
        .expect("Expected function template declaration in the symbol registry");
    let HIRSymbolKind::Function(HIRSymbolData::Template {
        template_data: body,
        template_prototype: template,
        ..
    }) = &stmt.kind
    else {
        unreachable!("Expected template to be a function template");
    };

    let namespace = symbol_lexical_namespace(&name.namespace, &stmt);
    env.symbols.push_local_scope();
    let result = (|| {
        apply_template_input(env, template, input)
            .map_err(|err| env.complete_err(err, &TokenRange::internal()))?;

        if env.items.request_fulfilled(prototype.symbol_name()) {
            return Ok(());
        }
        env.items
            .mark_request_fulfilled(prototype.symbol_name().into());

        typecheck_function(env, &namespace, prototype, body)?;

        Ok(())
    })();
    env.symbols.pop_local_scope();

    result
}

fn realize_comptime_fn_template(
    env: &mut TypeEnvironment,
    lookup_identifier: &QualifiedName,
    mut prototype: THIRComptimeFnPrototype,
    input: &THIRTemplateInput,
) -> CXResult<()> {
    let instance_name = base_mangle_templated_name(
        &env.symbols,
        prototype.symbol_name(),
        input
            .args
            .iter()
            .map(|arg| env.symbols.resolve_type_id(*arg)),
    );

    if env.items.request_fulfilled(&instance_name) {
        return Ok(());
    }
    
    env.items.mark_request_fulfilled(instance_name.clone());

    let resolution = env
        .symbols
        .get_global_registry()
        .resolve(lookup_identifier)
        .unwrap_or_else(|| {
            unreachable!(
                "Expected comptime function '{}' to be present in the symbol registry",
                lookup_identifier
            )
        });

    let stmt = resolution
        .declarations()
        .iter()
        .find(|symbol| matches!(symbol.kind, HIRSymbolKind::ComptimeFunction(_)))
        .expect("Expected comptime function declaration in the symbol registry");

    let HIRSymbolKind::ComptimeFunction(HIRSymbolData::Template { template_data, template_prototype, .. }) = &stmt.kind else {
        unreachable!("Expected comptime function to be a template");
    };

    let namespace = NamespacePath::from(symbol_lexical_namespace(
        &lookup_identifier.namespace,
        &stmt,
    ));
    
    env.symbols.push_local_scope();
    let result = (|| -> CXResult<()> {
        apply_template_input(env, template_prototype, input)
            .map_err(|err| env.complete_err(err, &TokenRange::internal()))?;
        
        prototype.map_symbol_name(|_| instance_name.clone());
        typecheck_comptime_function(env, &namespace, prototype.clone(), template_data)?;
        Ok(())
    })();
    env.symbols.pop_local_scope();

    result
}
