use cx_ast::{
    ast::{function::CXFunctionContract, modifiers::CXLinkageMode},
    symbols::CXSymbolKind,
};
use cx_log::CXResult;
use cx_mir::mir::{
    data::{
        MIRFunction, MIRFunctionPrototype, MIRFunctionSignature, MIRParameter, MIRTemplateInput,
    },
    expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
    r#type::MIRType,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    symbol::resolution::{apply_template_input, symbol_lexical_namespace},
    type_checking::functions::typecheck_function,
};

pub fn fulfill_requests(env: &mut TypeEnvironment) -> CXResult<()> {
    while let Some(request) = env.items.pop_request() {
        match request {
            MIRFunctionGenRequest::TypeConstructor {
                name,
                union_type,
                variant_type,
                variant_index,
            } => {
                realize_tagged_union_constructor(env, name, union_type, variant_type, variant_index)
            }

            MIRFunctionGenRequest::Template {
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
    name: String,
    union_type: MIRType,
    variant_type: MIRType,
    variant_index: usize,
) {
    if env.items.request_fulfilled(name.as_str()) {
        return;
    }
    env.items.mark_request_fulfilled(name.clone());

    let param_name = CXIdent::new("value");
    let prototype = MIRFunctionPrototype::new(
        name,
        CXLinkageMode::Static,
        MIRFunctionSignature {
            return_type: union_type.clone(),
            params: if variant_type.is_unit() {
                Vec::new()
            } else {
                vec![MIRParameter {
                    name: Some(param_name.clone()),
                    _type: variant_type.clone(),
                }]
            },
            var_args: false,
            contract: CXFunctionContract::default(),
        },
    );

    let value = if variant_type.is_unit() {
        MIRExpression {
            token_range: TokenRange::internal(),
            _type: variant_type.clone(),
            kind: MIRExpressionKind::Unit,
        }
    } else {
        let param_ref = MIRExpression {
            token_range: TokenRange::internal(),
            _type: env.symbols.mem_ref_to(variant_type.clone()),
            kind: MIRExpressionKind::Variable {
                name: param_name,
                location: SymbolValueOrigin::Local,
            },
        };

        MIRExpression {
            token_range: TokenRange::internal(),
            _type: variant_type.clone(),
            kind: MIRExpressionKind::RegionDuplicate {
                source: Box::new(param_ref),
            },
        }
    };
    let constructed = MIRExpression {
        token_range: TokenRange::internal(),
        _type: union_type.clone(),
        kind: MIRExpressionKind::ConstructTaggedUnion {
            variant_index,
            value: Box::new(value),
            sum_type: union_type,
        },
    };
    let body = MIRExpression {
        token_range: TokenRange::internal(),
        _type: prototype.signature().return_type.clone(),
        kind: MIRExpressionKind::Return {
            value: Some(Box::new(constructed)),
            postcondition: None,
        },
    };

    env.items
        .push_generated_function(MIRFunction { prototype, body });
}

fn realize_fn_template(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    prototype: MIRFunctionPrototype,
    input: &MIRTemplateInput,
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

    let CXSymbolKind::FunctionTemplate { template, body, .. } = &stmt.kind else {
        unreachable!("Expected template to be a function template");
    };

    let namespace = symbol_lexical_namespace(&name.namespace, &stmt);
    env.symbols.push_local_scope();
    let result = (|| {
        apply_template_input(env, template, input)?;

        if env.items.request_fulfilled(prototype.name()) {
            return Ok(());
        }
        env.items.mark_request_fulfilled(prototype.name().into());

        typecheck_function(env, &namespace, prototype, body)?;

        Ok(())
    })();
    env.symbols.pop_local_scope();

    result
}
