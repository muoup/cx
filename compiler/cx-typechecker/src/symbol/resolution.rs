use cx_hir::{
    ast::{
        function::{HIRFunctionContract, HIRFunctionKind},
        template::HIRTemplatePrototype,
        types::{HIRTagKind, HIRType, HIRTypeKind},
    },
    symbols::{HIRSymbol, HIRSymbolData, HIRSymbolKind, HIRTypeSymbol, SymbolResolution},
};
use cx_log::{
    CXRawResult, CXResult,
    error::{CXMaybeRawErr, CXMaybeRawResult},
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode, namespace::QualifiedName};

use cx_thir::{
    EnvironmentNamespace,
    symbol::MIRSymbol,
    thir::{
        contextual_eq::TypeContextEqual,
        data::{
            THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter, THIRTemplateInput,
            TemplateInfo,
        },
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind, THIRLocalID},
        global::THIRGlobalVariable,
    },
    type_context::THIRTypeContext,
};

use crate::{
    environment::{THIRFunctionGenRequest, TypeEnvironment},
    symbol::{
        completion::{
            complete_comptime_prototype, complete_prototype, complete_type, complete_type_symbol,
        },
        r#enum::resolve_enum_block,
        name_mangling::{base_mangle_member, base_mangle_templated_name},
    },
};

pub fn resolve_symbol(
    env: &mut TypeEnvironment,
    evaluation_namespace: &EnvironmentNamespace,
    symbol_namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbols: &SymbolResolution,
) -> CXResult<MIRSymbol> {
    let Some((first, rest)) = symbols.declarations().split_first() else {
        return env.log_error(
            TokenRange::internal(),
            format!("Symbol '{}' has no declarations", name),
        );
    };
    let decay_implicit_array = rest.is_empty();
    let resolved = resolve_symbol_inner(
        env,
        evaluation_namespace,
        symbol_namespace,
        name,
        first,
        decay_implicit_array,
    )?;

    for declaration in rest {
        let candidate = resolve_symbol_inner(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            declaration,
            false,
        )?;
        if !mir_symbols_equivalent(env, &resolved, &candidate) {
            return env.log_error(
                symbol_range(declaration),
                format!("Symbol '{}' has incompatible declarations", name),
            );
        }
    }

    Ok(resolved)
}

fn symbol_range(symbol: &HIRSymbol) -> TokenRange {
    match &symbol.kind {
        HIRSymbolKind::Type(ty) => ty.definition.range.clone(),
        HIRSymbolKind::Function(data) => data.base().range.clone(),
        HIRSymbolKind::TypeConstructor(data) => data.base().union_type.range.clone(),
        HIRSymbolKind::ComptimeFunction(data) => data.base().range.clone(),

        HIRSymbolKind::EnumIdent { .. } => TokenRange::internal(),
        HIRSymbolKind::AddressableGlobal { _type, .. } => _type.range().clone(),
    }
}

fn resolve_symbol_inner(
    env: &mut TypeEnvironment,
    evaluation_namespace: &EnvironmentNamespace,
    symbol_namespace: &EnvironmentNamespace,
    name: &CXIdent,
    symbol: &HIRSymbol,
    decay_implicit_array: bool,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        HIRSymbolKind::Type(data) => {
            let lookup_identifier =
                QualifiedName::new(symbol_namespace.as_namespace_path().clone(), name.clone());

            match data {
                HIRSymbolData::Standard(standard) => {
                    let completed =
                        complete_type_symbol(env, symbol_namespace, &lookup_identifier, standard)?;
                    let id = env.symbols.generate_type_id(completed);
                    Ok(MIRSymbol::Type(id))
                }

                HIRSymbolData::Template {
                    base,
                    template,
                    template_prototype,
                } => {
                    let source = HIRSymbol::new(
                        symbol.visibility,
                        HIRSymbolKind::Type(HIRTypeSymbol {
                            definition: base.clone(),
                            template: None,
                            tag: data.tag,
                        }),
                    );

                    Ok(MIRSymbol::Template {
                        template_prototype: template_prototype.clone(),
                        name: name.clone(),
                        source: Box::new(source),
                        namespace: symbol_namespace.clone(),
                    })
                }
            }
        }

        HIRSymbolKind::Function(data) => {
            let prototype_namespace = function_lexical_namespace(symbol_namespace, &data.kind);

            match data {
                HIRSymbolData::Standard(prototype) => {
                    complete_prototype(env, &prototype_namespace, prototype)
                        .map(MIRSymbol::FunctionReference)
                }
                HIRSymbolData::Template {
                    base,
                    template,
                    template_prototype,
                } => Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(HIRSymbol::new(
                        symbol.visibility,
                        HIRSymbolKind::Function(HIRSymbolData::Standard(base.clone())),
                    )),
                    namespace: prototype_namespace.clone(),
                }),
            }
        }

        HIRSymbolKind::ComptimeFunction(data) => {
            let prototype_namespace =
                function_lexical_namespace(symbol_namespace, &definition.kind);

            match data {
                HIRSymbolData::Standard(standard) => {
                    complete_comptime_prototype(env, &prototype_namespace, standard).map(
                        |prototype| MIRSymbol::ComptimeFunctionReference {
                            prototype,
                            namespace: prototype_namespace.clone(),
                            template_bindings: Vec::new(),
                        },
                    )
                }
                HIRSymbolData::Template {
                    base,
                    template,
                    template_prototype,
                } => Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(HIRSymbol::new(
                        symbol.visibility,
                        HIRSymbolKind::ComptimeFunction(HIRSymbolData::Standard(base.clone())),
                    )),
                    namespace: prototype_namespace.clone(),
                }),
            }
        }

        HIRSymbolKind::TypeConstructor(data) => match data {
            HIRSymbolData::Standard(standard) => resolve_type_constructor(
                env,
                symbol_namespace,
                name,
                &standard.union_type,
                standard.variant_index,
            ),

            HIRSymbolData::Template {
                base,
                template,
                template_prototype,
            } => {
                let source = HIRSymbol::new(
                    symbol.visibility,
                    HIRSymbolKind::TypeConstructor(HIRSymbolData::Standard(base.clone()))
                );

                Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(source),
                    namespace: symbol_namespace.clone(),
                })
            }
        },

        HIRSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => resolve_enum_block(env, symbol_namespace, *enum_block_idx).map(|b| {
            b.variant_expr(*variant_index)
                .expect("Expected enum variant to be in the global registry")
                .clone()
        }),

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
                env.items.push_generated_global(
                    THIRGlobalVariable {
                        name: symbol_name.clone(),
                        _type: ty.clone(),

                        is_mutable: false,
                        linkage: LinkageMode::Extern,
                        initializer: None,
                    },
                    false,
                );
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
    }
}

#[derive(Clone, Copy)]
pub(crate) enum TypeSymbolQuery {
    Standard,
    Tag(HIRTagKind),
    ImplicitTag,
}

pub(crate) fn resolve_type_symbol<'a>(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    query: TypeSymbolQuery,
    resolution: &'a SymbolResolution,
) -> CXMaybeRawResult<&'a HIRSymbol> {
    let declarations = resolution.declarations();
    let types = declarations
        .iter()
        .filter_map(|symbol| match &symbol.kind {
            HIRSymbolKind::Type(ty) => Some((symbol, ty)),
            _ => None,
        })
        .collect::<Vec<_>>();

    if types.len() != declarations.len() || types.is_empty() {
        return env
            .log_error_base(format!("Symbol '{name}' is not a type"))
            .map_err(|err| err.into());
    }

    let expected_tag = match query {
        TypeSymbolQuery::Standard => None,
        TypeSymbolQuery::Tag(tag) => Some(tag),
        TypeSymbolQuery::ImplicitTag => types[0].1.tag,
    };
    if types.iter().any(|(_, ty)| ty.tag != expected_tag) {
        return env
            .log_error_base(format!("Symbol '{name}' has incompatible tag declarations"))
            .map_err(|err| err.into());
    }

    if expected_tag.is_some() && types.len() > 1 {
        return env
            .log_error_base(format!("Symbol '{name}' has multiple type definitions"))
            .map_err(|err| err.into());
    }

    let first = types[0];
    for candidate in &types[1..] {
        if !type_declarations_equivalent(env, name, first.1, candidate.1)? {
            return env
                .log_error_base(format!("Symbol '{name}' has multiple type definitions"))
                .map_err(|err| err.into());
        }
    }

    Ok(first.0)
}

fn type_declarations_equivalent(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    left: &HIRTypeSymbol,
    right: &HIRTypeSymbol,
) -> CXMaybeRawResult<bool> {
    if left.tag != right.tag || left.template != right.template {
        return Ok(false);
    }

    if left.template.is_some() {
        let mut left_definition = left.definition.clone();
        let mut right_definition = right.definition.clone();
        left_definition.range = TokenRange::internal();
        right_definition.range = TokenRange::internal();
        return Ok(left_definition == right_definition);
    }

    let namespace = EnvironmentNamespace::from(&name.namespace);
    let left = complete_type(env, &namespace, &left.definition)?;
    let right = complete_type(env, &namespace, &right.definition)?;
    Ok(env.type_eq(&left, &right))
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
                    local_id: THIRLocalID::fresh(),
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
        resolve_symbol_inner(env, namespace, namespace, name, source, true)
            .map_err(CXMaybeRawErr::from)
    })();
    env.symbols.pop_local_scope();

    let mut symbol = result?;
    if let MIRSymbol::ComptimeFunctionReference {
        prototype,
        template_bindings,
        ..
    } = &mut symbol
    {
        *template_bindings = input
            .types
            .iter()
            .cloned()
            .zip(template_input.args.iter().copied())
            .collect();

        // Templated comptime functions instantiate per template input.
        if let Some(lookup_identifier) = prototype.lookup_identifier().cloned() {
            env.items.push_request(THIRFunctionGenRequest::Comptime {
                lookup_identifier,
                prototype: prototype.clone(),
                input: Some(template_input.clone()),
            });
        }
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
        HIRSymbolKind::Function(prototype)
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
