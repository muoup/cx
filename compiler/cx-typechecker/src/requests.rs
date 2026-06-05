use cx_ast::{
    ast::{function::CXFunctionContract, modifiers::CXLinkageMode},
    symbols::CXSymbolKind,
};
use cx_mir::{
    EnvironmentNamespace,
    mir::{
        data::{
            MIRFunction, MIRFunctionPrototype, MIRFunctionSignature, MIRParameter, MIRTemplateInput,
        },
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
        r#type::MIRType,
    },
};
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName};

use crate::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    symbol::{completion::complete_prototype, resolution::apply_template_input},
    type_checking::functions::typecheck_function,
};

pub fn fulfill_requests(
    env: &mut TypeEnvironment,
    _namespace: &EnvironmentNamespace,
) -> CXResult<()> {
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

            MIRFunctionGenRequest::Template { name, input } => {
                realize_fn_template(env, &name, &input)?
            }
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
    let param_name = CXIdent::new("value");
    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(name),
        linkage: CXLinkageMode::Static,
        signature: MIRFunctionSignature {
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
    };

    let value = if variant_type.is_unit() {
        MIRExpression {
            token_range: None,
            _type: variant_type.clone(),
            kind: MIRExpressionKind::Unit,
        }
    } else {
        let param_ref = MIRExpression {
            token_range: None,
            _type: env.symbols.mem_ref_to(variant_type.clone()),
            kind: MIRExpressionKind::Variable {
                name: param_name,
                location: SymbolValueOrigin::Local,
            },
        };

        MIRExpression {
            token_range: None,
            _type: variant_type.clone(),
            kind: MIRExpressionKind::RegionDuplicate {
                source: Box::new(param_ref),
            },
        }
    };
    let constructed = MIRExpression {
        token_range: None,
        _type: union_type.clone(),
        kind: MIRExpressionKind::ConstructTaggedUnion {
            variant_index,
            value: Box::new(value),
            sum_type: union_type,
        },
    };
    let body = MIRExpression {
        token_range: None,
        _type: prototype.signature.return_type.clone(),
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
    input: &MIRTemplateInput,
) -> CXResult<()> {
    let stmt = env.symbols
        .get_global_registry()
        .resolve(name)
        .expect("Expected template to be in the global registry");

    let CXSymbolKind::FunctionTemplate { template, definition, body } = &stmt.kind else {
        unreachable!("Expected template to be a function template");
    };

    let namespace = name.namespace.clone();
    env.push_scope(false, false);

    apply_template_input(&mut env.symbols, template, input)?;
    let prototype = complete_prototype(&mut env.symbols, &namespace, &definition)?;
    typecheck_function(env, &namespace, prototype, &body)?;

    env.pop_scope()?;
    Ok(())
}
