use cx_ast::{
    ast::{
        function::{CXFunctionContract, CXFunctionKind},
        modifiers::CXLinkageMode,
        template::CXTemplatePrototype,
        types::CXType,
    },
    symbols::{CXSymbol, CXSymbolKind},
};
use cx_log::{CXRawResult, CXResult};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use cx_mir::{
    EnvironmentNamespace,
    mir::{
        contextual_eq::TypeContextEqual,
        data::{
            MIRFunctionPrototype, MIRFunctionSignature, MIRParameter, MIRTemplateInput,
            TemplateInfo,
        },
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
        global::{MIRGlobalVarKind, MIRGlobalVariable},
        name_mangling::{base_mangle_member, base_mangle_templated_name},
    },
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
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
    evaluation_namespace: &EnvironmentNamespace,
    symbol_namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbol: &CXSymbol,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        CXSymbolKind::DuplicateDefinition(definitions) => resolve_duplicate_definition(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            symbol.visibility,
            definitions,
        ),

        CXSymbolKind::Type(ty) => {
            let completed = complete_type(env, symbol_namespace, ty)?;
            let id = env.symbols.generate_type_id(completed);
            Ok(MIRSymbol::Type(id))
        }

        CXSymbolKind::AddressableGlobal(name, ty) => {
            let ty = complete_type(env, symbol_namespace, ty)?;

            if evaluation_namespace != symbol_namespace {
                env.items.push_generated_global(MIRGlobalVariable {
                    is_mutable: false,
                    linkage: CXLinkageMode::Extern,
                    kind: MIRGlobalVarKind::Variable {
                        name: name.clone(),
                        _type: ty.clone(),
                        initializer: None,
                    },
                });
            }

            Ok(MIRSymbol::Expression(MIRExpression {
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::Variable {
                    name: name.clone(),
                    location: SymbolValueOrigin::Global,
                },
                _type: env.symbols.mem_ref_to(ty),
            }))
        }

        CXSymbolKind::FunctionReference(prototype) => {
            let prototype_namespace = function_lexical_namespace(symbol_namespace, &prototype.kind);
            let prototype = complete_prototype(env, &prototype_namespace, prototype)?;

            Ok(MIRSymbol::FunctionReference(prototype))
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
                template_prototype: template.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: symbol_namespace.clone(),
            })
        }

        CXSymbolKind::TypeConstructor {
            template: None,
            union_type,
            variant_index,
        } => resolve_type_constructor(env, symbol_namespace, name, union_type, *variant_index),

        CXSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => resolve_enum_block(env, symbol_namespace, *enum_block_idx).map(|b| {
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
                template_prototype: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: symbol_namespace.clone(),
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
                template_prototype: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: symbol_namespace.clone(),
            })
        }
    }
}

fn resolve_duplicate_definition(
    env: &mut TypeEnvironment,
    evaluation_namespace: &EnvironmentNamespace,
    symbol_namespace: &EnvironmentNamespace,
    name: &CXIdent,
    visibility: cx_ast::ast::modifiers::VisibilityMode,
    definitions: &[CXSymbolKind],
) -> CXResult<MIRSymbol> {
    let Some((first, rest)) = definitions.split_first() else {
        return crate::log::internal_type_error(format!(
            "Duplicate symbol declaration '{}' has no definitions",
            name
        ));
    };

    let first = resolve_symbol(
        env,
        evaluation_namespace,
        symbol_namespace,
        name,
        &CXSymbol::new(visibility, first.clone()),
    )?;

    for definition in rest {
        let candidate = resolve_symbol(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            &CXSymbol::new(visibility, definition.clone()),
        )?;

        if !mir_symbols_equivalent(env, &first, &candidate) {
            return crate::log::internal_type_error(format!(
                "Duplicate symbol declaration '{}' resolves to incompatible definitions",
                name
            ));
        }
    }

    Ok(first)
}

fn mir_symbols_equivalent(env: &TypeEnvironment, left: &MIRSymbol, right: &MIRSymbol) -> bool {
    match (left, right) {
        (MIRSymbol::Type(left), MIRSymbol::Type(right)) => env.type_eq(
            env.symbols.resolve_type_id(*left),
            env.symbols.resolve_type_id(*right),
        ),

        (MIRSymbol::FunctionReference(left), MIRSymbol::FunctionReference(right)) => {
            left.linkage() == right.linkage() && left.contextual_eq(right, &env.symbols)
        }

        (MIRSymbol::Expression(left), MIRSymbol::Expression(right)) => {
            env.type_eq(&left._type, &right._type)
                && format!("{:?}", left.kind) == format!("{:?}", right.kind)
        }

        _ => false,
    }
}

fn resolve_type_constructor(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &CXIdent,
    union_type: &CXType,
    variant_index: usize,
) -> CXResult<MIRSymbol> {
    let union_type = complete_type(env, namespace, union_type)?;
    let variants = union_type.aggregate_fields(&env.symbols).ok_or_else(|| {
        crate::log::type_error_msg("Type constructor target is not a tagged union")
    })?;
    let Some((_, variant_type)) = variants.get(variant_index).cloned() else {
        return crate::log::internal_type_error(format!(
            "Type constructor variant index {} is out of bounds",
            variant_index
        ));
    };

    let prototype = MIRFunctionPrototype::new(
        base_mangle_member(&env.symbols, name.as_str(), &union_type),
        CXLinkageMode::Static,
        MIRFunctionSignature {
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
    )
    .with_debug_name(name.clone());

    env.items
        .push_request(MIRFunctionGenRequest::TypeConstructor {
            name: prototype.name().to_owned(),
            union_type,
            variant_type,
            variant_index,
        });

    Ok(MIRSymbol::FunctionReference(prototype))
}

pub fn apply_template(
    env: &mut TypeEnvironment,
    symbol: &MIRSymbol,
    template_input: MIRTemplateInput,
) -> CXRawResult<Option<MIRSymbol>> {
    let MIRSymbol::Template {
        template_prototype: input,
        name,
        source,
        namespace,
    } = symbol
    else {
        return Ok(None);
    };

    if input.types.len() != template_input.args.len() {
        return env.log_error_base(format!(
            "Template '{}' expects {} arguments, found {}",
            name,
            input.types.len(),
            template_input.args.len()
        ));
    }

    env.symbols.push_local_scope();
    let result = (|| {
        apply_template_input(env, input, &template_input)?;
        resolve_symbol(env, namespace, namespace, name, source)
    })();
    env.symbols.pop_local_scope();

    let mut symbol = result?;
    attach_template_metadata(env, &mut symbol, namespace, template_input.clone());

    if let MIRSymbol::FunctionReference(prototype) = &symbol
        && let Some(name) = prototype.lookup_identifier().cloned()
    {
        env.items.push_request(MIRFunctionGenRequest::Template {
            name,
            prototype: prototype.clone(),
            input: template_input,
        });
    }

    Ok(Some(symbol))
}

pub fn symbol_lexical_namespace(
    namespace: impl Into<EnvironmentNamespace>,
    symbol: &CXSymbol,
) -> EnvironmentNamespace {
    let namespace = namespace.into();
    match &symbol.kind {
        CXSymbolKind::FunctionReference(prototype)
        | CXSymbolKind::FunctionTemplate {
            definition: prototype,
            ..
        } => function_lexical_namespace(&namespace, &prototype.kind),
        _ => namespace.clone(),
    }
}

fn function_lexical_namespace(
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> EnvironmentNamespace {
    match kind {
        CXFunctionKind::AssociatedFunction { .. } => namespace
            .parent_and_name()
            .map(|(parent, _)| parent)
            .unwrap_or_else(|| namespace.as_namespace_path().clone())
            .into(),
        CXFunctionKind::Standard(_) => namespace.clone(),
    }
}

pub fn apply_template_input(
    env: &mut TypeEnvironment,
    prototype: &CXTemplatePrototype,
    input: &MIRTemplateInput,
) -> CXResult<()> {
    for (param, arg) in prototype.types.iter().zip(input.args.iter()) {
        env.symbols.insert_local_type_id(param.as_string(), *arg)?;
    }

    Ok(())
}

fn attach_template_metadata(
    env: &mut TypeEnvironment,
    symbol: &mut MIRSymbol,
    _namespace: &EnvironmentNamespace,
    input: MIRTemplateInput,
) {
    match symbol {
        MIRSymbol::Type(id) => {
            let mut ty = env.symbols.resolve_type_id(*id).clone();
            ty.template_info = Some(Box::new(TemplateInfo {
                base_name: ty.lookup_identifier.clone(),
                template_input: input.clone(),
            }));
            ty.strong_identifier = ty.strong_identifier.as_ref().map(|base| {
                base_mangle_templated_name(
                    &env.symbols,
                    base.as_str(),
                    input
                        .args
                        .iter()
                        .map(|arg| env.symbols.resolve_type_id(*arg)),
                )
            });
            env.symbols.overwrite_type_id(*id, ty);
        }

        MIRSymbol::FunctionReference(prototype) if prototype.lookup_identifier().is_some() => {
            prototype.map_symbol_name(|name| {
                base_mangle_templated_name(
                    &env.symbols,
                    name,
                    input
                        .args
                        .iter()
                        .map(|arg| env.symbols.resolve_type_id(*arg)),
                )
            });
        }

        _ => (),
    }
}
