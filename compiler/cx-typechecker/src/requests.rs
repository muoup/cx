use cx_hir::{ast::function::HIRFunctionContract, symbols::HIRSymbolKind};
use cx_log::CXResult;
use cx_thir::thir::{
    data::{THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter, THIRTemplateInput},
    expression::{THIRExpression, THIRExpressionKind},
    r#type::THIRType,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode, namespace::QualifiedName};

use crate::{
    environment::{THIRFunctionGenRequest, TypeEnvironment},
    symbol::resolution::{apply_template_input, symbol_lexical_namespace},
    type_checking::functions::typecheck_function,
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
            params: if variant_type.is_unit() {
                Vec::new()
            } else {
                vec![THIRParameter {
                    name: Some(param_name.clone()),
                    local_id: Some(param_local_id),
                    _type: variant_type.clone(),
                }]
            },
            var_args: false,
            contract: HIRFunctionContract::default(),
        },
    )
    .with_debug_name(debug_name);

    let value = if variant_type.is_unit() {
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

    env.items
        .push_generated_function(THIRFunction { prototype, body });
}

fn realize_fn_template(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    prototype: THIRFnPrototype,
    input: &THIRTemplateInput,
) -> CXResult<()> {
    let stmt = env
        .symbols
        .get_global_registry()
        .resolve(name)
        .unwrap_or_else(|| {
            unreachable!(
                "Expected function template '{}' to be present in the symbol registry",
                name
            )
        });

    let HIRSymbolKind::FunctionTemplate { template, body, .. } = &stmt.kind else {
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
