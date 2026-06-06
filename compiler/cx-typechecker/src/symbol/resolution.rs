use cx_ast::{
    ast::{
        function::{CXFunctionContract, CXFunctionKind},
        modifiers::CXLinkageMode,
        template::CXTemplatePrototype,
    },
    symbols::{CXSymbol, CXSymbolKind},
};
use cx_util::{identifier::CXIdent, namespace::QualifiedName, CXError, CXResult};

use cx_mir::{
    mir::{
        data::{
            MIRFunctionPrototype, MIRFunctionSignature, MIRParameter, MIRTemplateInput,
            TemplateInfo,
        },
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
        name_mangling::{base_mangle_static_member, base_mangle_templated_name},
        r#type::MIRTypeKind,
    },
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
    EnvironmentNamespace,
};

use crate::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    symbol::{
        completion::{complete_prototype, complete_type},
        r#enum::resolve_enum_block,
    },
};

pub fn resolve_symbol(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbol: &CXSymbol,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        CXSymbolKind::Type(ty) => {
            let mut completed = complete_type(env, namespace, ty)?;
            if completed.debug_name.is_none() {
                completed.debug_name = Some(name.clone());
            }
            let id = env.symbols.generate_type_id(completed);
            Ok(MIRSymbol::Type(id))
        }

        CXSymbolKind::AddressableGlobal(name, ty) => {
            let ty = complete_type(env, &namespace, ty)?;
            let mem_ty = env.symbols.mem_ref_to(ty);
            Ok(MIRSymbol::Expression(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable {
                    name: name.clone(),
                    location: SymbolValueOrigin::Global,
                },
                _type: mem_ty,
            }))
        }

        CXSymbolKind::FunctionReference(prototype) => {
            let prototype_namespace = function_lexical_namespace(namespace, &prototype.kind);
            let prototype = complete_prototype(env, &prototype_namespace, prototype)?;

            Ok(MIRSymbol::Expression(MIRExpression {
                token_range: None,
                _type: MIRTypeKind::Function {
                    signature: Box::new(prototype.signature),
                }
                .into(),
                kind: MIRExpressionKind::FunctionReference {
                    name: prototype.name,
                },
            }))
        }

        CXSymbolKind::TypeConstructor {
            template: Some(template),
            union_type,
            variant_index,
        } => {
            let source = CXSymbol::new(
                symbol.visibility,
                CXSymbolKind::TypeConstructor {
                    template: None,
                    union_type: union_type.clone(),
                    variant_index: *variant_index,
                },
            );

            Ok(MIRSymbol::Template {
                input: template.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: namespace.clone(),
            })
        }

        CXSymbolKind::TypeConstructor {
            template: None,
            union_type,
            variant_index,
        } => resolve_type_constructor(env, namespace, name, union_type, *variant_index),

        CXSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => resolve_enum_block(env, namespace, *enum_block_idx).map(|b| {
            b.variant_expr(*variant_index)
                .expect("Expected enum variant to be in the global registry")
                .clone()
        }),

        CXSymbolKind::TypeTemplate {
            template: input,
            definition,
        } => {
            let source = CXSymbol::new(symbol.visibility, CXSymbolKind::Type(definition.clone()));

            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: namespace.clone(),
            })
        }

        CXSymbolKind::FunctionTemplate {
            template: input,
            definition,
            ..
        } => {
            let source = CXSymbol::new(
                symbol.visibility,
                CXSymbolKind::FunctionReference(definition.clone()),
            );

            Ok(MIRSymbol::Template {
                input: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: namespace.clone(),
            })
        }
    }
}

fn resolve_type_constructor(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &CXIdent,
    union_type: &cx_ast::ast::types::CXType,
    variant_index: usize,
) -> CXResult<MIRSymbol> {
    let union_type = complete_type(env, namespace, union_type)?;
    let variants = union_type
        .aggregate_fields(&env.symbols)
        .ok_or_else(|| CXError::create_boxed("Type constructor target is not a tagged union"))?;
    let Some((_, variant_type)) = variants.get(variant_index).cloned() else {
        return CXError::create_result(format!(
            "Type constructor variant index {} is out of bounds",
            variant_index
        ));
    };

    let mangled_name = base_mangle_static_member(&env.symbols, name.as_str(), &union_type);
    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(mangled_name.clone()),
        linkage: CXLinkageMode::Static,
        signature: MIRFunctionSignature {
            return_type: union_type.clone(),
            params: if variant_type.is_unit() {
                Vec::new()
            } else {
                vec![MIRParameter {
                    name: Some(CXIdent::new("value")),
                    _type: variant_type.clone(),
                }]
            },
            var_args: false,
            contract: CXFunctionContract::default(),
        },
    };

    if !env.items.has_type_constructor_request(&mangled_name) {
        env.items
            .push_request(MIRFunctionGenRequest::TypeConstructor {
                name: mangled_name,
                union_type,
                variant_type,
                variant_index,
            });
    }

    Ok(MIRSymbol::Expression(MIRExpression {
        token_range: None,
        _type: MIRTypeKind::Function {
            signature: Box::new(prototype.signature),
        }
        .into(),
        kind: MIRExpressionKind::FunctionReference {
            name: prototype.name,
        },
    }))
}

pub fn apply_template(
    env: &mut TypeEnvironment,
    symbol: &MIRSymbol,
    template_input: MIRTemplateInput,
) -> CXResult<Option<MIRSymbol>> {
    let MIRSymbol::Template {
        input,
        name,
        source,
        namespace,
    } = symbol
    else {
        return Ok(None);
    };

    let is_function = matches!(&source.kind, CXSymbolKind::FunctionReference(_));
    if input.types.len() != template_input.args.len() {
        return CXError::create_result(format!(
            "Template '{}' expects {} arguments, found {}",
            name,
            input.types.len(),
            template_input.args.len()
        ));
    }

    env.symbols.push_local_scope();
    let result = (|| {
        apply_template_input(env, input, &template_input)?;
        resolve_symbol(env, namespace, name, source)
    })();
    env.symbols.pop_local_scope();

    let mut result = result?;

    if is_function {
        env.items.push_request(MIRFunctionGenRequest::Template {
            name: QualifiedName::new(namespace.clone(), name.clone()),
            input: template_input.clone(),
        });
    }

    attach_template_metadata(env, &mut result, namespace, template_input);
    Ok(Some(result))
}

pub fn symbol_lexical_namespace(
    namespace: &EnvironmentNamespace,
    symbol: &CXSymbol,
) -> EnvironmentNamespace {
    match &symbol.kind {
        CXSymbolKind::FunctionReference(prototype)
        | CXSymbolKind::FunctionTemplate {
            definition: prototype,
            ..
        } => function_lexical_namespace(namespace, &prototype.kind),
        _ => namespace.clone(),
    }
}

fn function_lexical_namespace(
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> EnvironmentNamespace {
    match kind {
        CXFunctionKind::MemberFunction { .. } | CXFunctionKind::StaticMemberFunction { .. } => {
            namespace
                .parent_and_name()
                .map(|(parent, _)| parent)
                .unwrap_or_else(|| namespace.clone())
        }
        CXFunctionKind::Standard(_) => namespace.clone(),
    }
}

pub fn apply_template_input(
    env: &mut TypeEnvironment,
    prototype: &CXTemplatePrototype,
    input: &MIRTemplateInput,
) -> CXResult<()> {
    for (param, arg) in prototype.types.iter().zip(input.args.iter()) {
        env.symbols
            .insert_local_type(param.as_string(), arg.clone())?;
    }

    Ok(())
}

fn attach_template_metadata(
    env: &mut TypeEnvironment,
    symbol: &mut MIRSymbol,
    _namespace: &EnvironmentNamespace,
    input: MIRTemplateInput,
) {
    let MIRSymbol::Type(id) = symbol else {
        return;
    };

    let mut ty = env.symbols.resolve_type_id(*id).clone();
    ty.template_info = Some(Box::new(TemplateInfo {
        base_name: ty.strong_identifier.clone(),
        template_input: input.clone(),
    }));
    ty.strong_identifier.as_mut().map(|base| {
        base.name =
            base_mangle_templated_name(&env.symbols, base.name.as_str(), input.args.as_slice())
                .into()
    });

    env.symbols.overwrite_type_id(*id, ty);
}
