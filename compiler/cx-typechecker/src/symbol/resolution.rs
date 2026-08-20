use cx_hir::{
    ast::{
        function::{HIRFunctionContract, HIRFunctionKind},
        template::HIRTemplatePrototype,
        types::{HIRType, HIRTypeKind},
    },
    symbols::{HIRSymbol, HIRSymbolKind},
};
use cx_log::{
    CXRawResult, CXResult,
    error::{CXMaybeRawErr, CXMaybeRawResult},
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use cx_thir::{
    EnvironmentNamespace,
    symbol::MIRSymbol,
    thir::{
        contextual_eq::TypeContextEqual,
        data::{
            THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter, THIRTemplateInput,
            TemplateInfo,
        },
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind},
        global::THIRGlobalVariable,
    },
    type_context::THIRTypeContext,
};

use crate::{
    environment::{THIRFunctionGenRequest, TypeEnvironment},
    symbol::{
        completion::{complete_comptime_prototype, complete_prototype, complete_type},
        r#enum::resolve_enum_block,
        name_mangling::{base_mangle_member, base_mangle_templated_name},
    },
};

pub fn resolve_symbol(
    env: &mut TypeEnvironment,
    evaluation_namespace: &EnvironmentNamespace,
    symbol_namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbol: &HIRSymbol,
) -> CXResult<MIRSymbol> {
    resolve_symbol_inner(
        env,
        evaluation_namespace,
        symbol_namespace,
        name,
        symbol,
        true,
    )
}

// pub(crate) fn resolve_symbol_without_implicit_array_decay(
//     env: &mut TypeEnvironment,
//     evaluation_namespace: &EnvironmentNamespace,
//     symbol_namespace: &EnvironmentNamespace,
//     name: &CXIdent,
//     symbol: &HIRSymbol,
// ) -> CXResult<MIRSymbol> {
//     resolve_symbol_inner(
//         env,
//         evaluation_namespace,
//         symbol_namespace,
//         name,
//         symbol,
//         false,
//     )
// }

fn resolve_symbol_inner(
    env: &mut TypeEnvironment,
    evaluation_namespace: &EnvironmentNamespace,
    symbol_namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbol: &HIRSymbol,
    decay_implicit_array: bool,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        HIRSymbolKind::DuplicateDefinition(definitions) => resolve_duplicate_definition(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            symbol.visibility,
            definitions,
        ),

        HIRSymbolKind::Type(ty) => {
            let completed = complete_type(env, symbol_namespace, ty)?;
            let id = env.symbols.generate_type_id(completed);
            Ok(MIRSymbol::Type(id))
        }

        HIRSymbolKind::AddressableGlobal {
            name,
            _type,
            symbol_naming,
        } => {
            let ty = complete_type(env, symbol_namespace, _type)?;
            let symbol_name = CXIdent::new(crate::symbol::completion::completed_symbol_name(
                env,
                cx_util::namespace::QualifiedName::new(symbol_namespace.clone(), name.clone()),
                *symbol_naming,
            ));

            if evaluation_namespace != symbol_namespace {
                env.items.push_generated_global(THIRGlobalVariable {
                    name: symbol_name.clone(),
                    _type: ty.clone(),
                 
                    is_mutable: false,
                    linkage: LinkageMode::Extern,
                    initializer: None,
                });
            }

            let global = THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::GlobalVariable {
                    symbol: symbol_name.clone(),
                },
                _type: env.symbols.mem_ref_to(ty.clone()),
            };
            let expression = if decay_implicit_array
                && matches!(_type.kind, HIRTypeKind::ImplicitSizedArray(_))
            {
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::TypeConversion {
                        operand: Box::new(global),
                        conversion: THIRCoercion::ReinterpretBits,
                    },
                    _type: ty,
                }
            } else {
                global
            };

            Ok(MIRSymbol::Expression(expression))
        }

        HIRSymbolKind::FunctionReference(prototype) => {
            let prototype_namespace = function_lexical_namespace(symbol_namespace, &prototype.kind);
            let prototype = complete_prototype(env, &prototype_namespace, prototype)?;

            env.items.push_generated_function(THIRFunction {
                prototype: prototype.clone(),
                body: None,
            });

            Ok(MIRSymbol::FunctionReference(prototype))
        }

        HIRSymbolKind::ComptimeFunction { definition, body } => {
            let prototype_namespace =
                function_lexical_namespace(symbol_namespace, &definition.kind);
            let prototype = complete_comptime_prototype(env, &prototype_namespace, definition)?;

            Ok(MIRSymbol::ComptimeFunctionReference {
                prototype,
                namespace: prototype_namespace,
                body: body.clone(),
                template_bindings: Vec::new(),
            })
        }

        HIRSymbolKind::TypeConstructor {
            template: Some(template),
            union_type,
            variant_index,
        } => {
            let source = HIRSymbol::new(
                symbol.visibility,
                HIRSymbolKind::TypeConstructor {
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

        HIRSymbolKind::TypeConstructor {
            template: None,
            union_type,
            variant_index,
        } => resolve_type_constructor(env, symbol_namespace, name, union_type, *variant_index),

        HIRSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => resolve_enum_block(env, symbol_namespace, *enum_block_idx).map(|b| {
            b.variant_expr(*variant_index)
                .expect("Expected enum variant to be in the global registry")
                .clone()
        }),

        HIRSymbolKind::TypeTemplate {
            template: input,
            definition,
        } => {
            let source = HIRSymbol::new(symbol.visibility, HIRSymbolKind::Type(definition.clone()));

            Ok(MIRSymbol::Template {
                template_prototype: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: symbol_namespace.clone(),
            })
        }

        HIRSymbolKind::FunctionTemplate {
            template: input,
            definition,
            ..
        } => {
            let source = HIRSymbol::new(
                symbol.visibility,
                HIRSymbolKind::FunctionReference(definition.clone()),
            );

            Ok(MIRSymbol::Template {
                template_prototype: input.clone(),
                name: name.clone(),
                source: Box::new(source),
                namespace: symbol_namespace.clone(),
            })
        }

        HIRSymbolKind::ComptimeFunctionTemplate {
            template: input,
            definition,
            body,
        } => {
            let source = HIRSymbol::new(
                symbol.visibility,
                HIRSymbolKind::ComptimeFunction {
                    definition: definition.clone(),
                    body: body.clone(),
                },
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
    visibility: cx_hir::ast::modifiers::VisibilityMode,
    definitions: &[HIRSymbolKind],
) -> CXResult<MIRSymbol> {
    let Some((first, rest)) = definitions.split_first() else {
        return crate::log::internal_type_error(format!(
            "Duplicate symbol declaration '{}' has no definitions",
            name
        ));
    };

    let first = resolve_symbol_inner(
        env,
        evaluation_namespace,
        symbol_namespace,
        name,
        &HIRSymbol::new(visibility, first.clone()),
        false,
    )?;

    for definition in rest {
        let candidate = resolve_symbol_inner(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            &HIRSymbol::new(visibility, definition.clone()),
            false,
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
            let compatible_linkage = left.linkage() == right.linkage()
                || (left.linkage() != LinkageMode::Static
                    && right.linkage() != LinkageMode::Static);
            compatible_linkage
                && left.symbol_name() == right.symbol_name()
                && left
                    .signature()
                    .contextual_eq(right.signature(), &env.symbols)
        }

        (
            MIRSymbol::ComptimeFunctionReference {
                prototype: left, ..
            },
            MIRSymbol::ComptimeFunctionReference {
                prototype: right, ..
            },
        ) => left.lookup_identifier() == right.lookup_identifier(),

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
    union_type: &HIRType,
    variant_index: usize,
) -> CXResult<MIRSymbol> {
    let range = union_type.range().clone();
    let union_type = complete_type(env, namespace, union_type)?;
    let variants = union_type
        .aggregate_fields(&env.symbols)
        .ok_or_else(|| env.error(&range, "Type constructor target is not a tagged union"))?;
    let Some((_, variant_type)) = variants.get(variant_index).cloned() else {
        return crate::log::internal_type_error(format!(
            "Type constructor variant index {} is out of bounds",
            variant_index
        ));
    };

    let prototype = THIRFnPrototype::new(
        base_mangle_member(&env.symbols, name.as_str(), &union_type),
        LinkageMode::Static,
        THIRFnSignature {
            return_type: union_type.clone(),
            params: if variant_type.is_void() {
                Vec::new()
            } else {
                vec![THIRParameter {
                    name: Some(CXIdent::new("value")),
                    local_id: Some(cx_thir::thir::expression::THIRLocalID::fresh()),
                    _type: variant_type.clone(),
                }]
            },
            var_args: false,
            contract: HIRFunctionContract::default(),
        },
    )
    .with_debug_name(name.clone());

    env.items
        .push_request(THIRFunctionGenRequest::TypeConstructor {
            symbol_name: prototype.symbol_name().to_owned(),
            debug_name: name.clone(),
            union_type,
            variant_type,
            variant_index,
        });

    Ok(MIRSymbol::FunctionReference(prototype))
}

pub fn apply_template(
    env: &mut TypeEnvironment,
    symbol: &MIRSymbol,
    template_input: THIRTemplateInput,
) -> CXMaybeRawResult<Option<MIRSymbol>> {
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
        return env
            .log_error_base(format!(
                "Template '{}' expects {} arguments, found {}",
                name,
                input.types.len(),
                template_input.args.len()
            ))
            .map_err(CXMaybeRawErr::from);
    }

    env.symbols.push_local_scope();
    let result = (|| -> CXMaybeRawResult<MIRSymbol> {
        apply_template_input(env, input, &template_input).map_err(CXMaybeRawErr::from)?;
        resolve_symbol(env, namespace, namespace, name, source).map_err(CXMaybeRawErr::from)
    })();
    env.symbols.pop_local_scope();

    let mut symbol = result?;
    if let MIRSymbol::ComptimeFunctionReference {
        template_bindings, ..
    } = &mut symbol
    {
        *template_bindings = input
            .types
            .iter()
            .cloned()
            .zip(template_input.args.iter().copied())
            .collect();
    }
    attach_template_metadata(env, &mut symbol, namespace, template_input.clone());

    if let MIRSymbol::FunctionReference(prototype) = &symbol
        && let Some(name) = prototype.lookup_identifier().cloned()
    {
        env.items.push_request(THIRFunctionGenRequest::Template {
            name,
            prototype: prototype.clone(),
            input: template_input,
        });
    }

    Ok(Some(symbol))
}

pub fn symbol_lexical_namespace(
    namespace: impl Into<EnvironmentNamespace>,
    symbol: &HIRSymbol,
) -> EnvironmentNamespace {
    let namespace = namespace.into();
    match &symbol.kind {
        HIRSymbolKind::FunctionReference(prototype)
        | HIRSymbolKind::FunctionTemplate {
            definition: prototype,
            ..
        } => function_lexical_namespace(&namespace, &prototype.kind),
        HIRSymbolKind::ComptimeFunction { definition, .. }
        | HIRSymbolKind::ComptimeFunctionTemplate { definition, .. } => {
            function_lexical_namespace(&namespace, &definition.kind)
        }
        _ => namespace.clone(),
    }
}

fn function_lexical_namespace(
    namespace: &EnvironmentNamespace,
    kind: &HIRFunctionKind,
) -> EnvironmentNamespace {
    match kind {
        HIRFunctionKind::AssociatedFunction { .. } => namespace
            .parent_and_name()
            .map(|(parent, _)| parent)
            .unwrap_or_else(|| namespace.as_namespace_path().clone())
            .into(),
        HIRFunctionKind::Standard(_) => namespace.clone(),
    }
}

pub fn apply_template_input(
    env: &mut TypeEnvironment,
    prototype: &HIRTemplatePrototype,
    input: &THIRTemplateInput,
) -> CXRawResult<()> {
    for (param, arg) in prototype.types.iter().zip(input.args.iter()) {
        env.symbols.insert_local_type_id(param.as_string(), *arg)?;
    }

    Ok(())
}

fn attach_template_metadata(
    env: &mut TypeEnvironment,
    symbol: &mut MIRSymbol,
    _namespace: &EnvironmentNamespace,
    input: THIRTemplateInput,
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
