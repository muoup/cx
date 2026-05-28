use cx_ast::data::CXFunctionKind;
use cx_mir::mir::{data::MIRTemplateInput, r#type::MIRType};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    realize_fn_implementation,
};
use cx_util::CXResult;

struct TemplateRequestReceipt {
    module_origin: Option<String>,
    kind: CXFunctionKind,
    input: MIRTemplateInput,
}

pub(crate) fn fulfill_requests(job: &CompilationUnit, env: &mut TypeEnvironment) -> CXResult<()> {
    let mut requests_fulfilled = Vec::new();

    while let Some(request) = env.pop_function_generation_request() {
        match request {
            MIRFunctionGenRequest::Template {
                module_origin,
                kind,
                input,
            } => {
                let origin = match &module_origin {
                    Some(module) => env.resolve_compilation_unit(module.as_str()),
                    None => job.clone(),
                };

                if request_was_fulfilled(env, &requests_fulfilled, &module_origin, &kind, &input) {
                    continue;
                }

                requests_fulfilled.push(TemplateRequestReceipt {
                    module_origin: module_origin.clone(),
                    kind: kind.clone(),
                    input: input.clone(),
                });
                realize_fn_implementation(env, &origin, &kind, &input)?;
            }
            MIRFunctionGenRequest::TypeConstructor {
                name,
                union_type,
                variant_type,
                variant_index,
            } => {
                realize_tagged_union_constructor(
                    env,
                    name,
                    union_type,
                    variant_type,
                    variant_index,
                );
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
    use cx_ast::data::{CXFunctionContract, CXFunctionKind, CXLinkageMode};
    use cx_mir::mir::{
        data::{MIRFunctionPrototype, MIRParameter},
        expression::{MIRExpression, MIRExpressionKind},
        program::MIRFunction,
    };
    use cx_tokens::TokenRange;
    use cx_util::{identifier::CXIdent, namespace::QualifiedName};

    let param_name = CXIdent::new("value");
    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(name),
        source_prototype: cx_ast::data::CXFunctionPrototype {
            kind: CXFunctionKind::Standard(CXIdent::new("__tagged_union_variant_ctor")),
            params: Vec::new(),
            return_type: cx_ast::data::CXTypeKind::Identifier {
                name: QualifiedName::new_raw(CXIdent::new("__tagged_union")),
                predeclaration: cx_ast::data::PredeclarationType::None,
            }
            .to_type(),
            var_args: false,
            contract: CXFunctionContract::default(),
            linkage: CXLinkageMode::Static,
            range: TokenRange::default(),
        },
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
        linkage: CXLinkageMode::Static,
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
            _type: env.symbols.context.mem_ref_to(variant_type.clone()),
            kind: MIRExpressionKind::Variable(param_name),
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
        _type: prototype.return_type.clone(),
        kind: MIRExpressionKind::Return {
            value: Some(Box::new(constructed)),
            postcondition: None,
        },
    };

    env.push_generated_function(MIRFunction { prototype, body });
}

fn request_was_fulfilled(
    env: &TypeEnvironment,
    requests_fulfilled: &[TemplateRequestReceipt],
    module_origin: &Option<String>,
    kind: &CXFunctionKind,
    input: &MIRTemplateInput,
) -> bool {
    requests_fulfilled.iter().any(|receipt| {
        &receipt.module_origin == module_origin
            && &receipt.kind == kind
            && receipt.input.contextual_eq(input, &env.symbols.context)
    })
}
