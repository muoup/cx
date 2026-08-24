use cx_hir::{
    ast::{
        function::{HIRFunctionContract, HIRFunctionKind},
        template::HIRTemplatePrototype,
        types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
    },
    symbols::{HIRFunctionSymbol, HIRSymbol, HIRSymbolData, HIRSymbolKind, SymbolResolution},
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
            complete_comptime_prototype, complete_prototype, complete_type, complete_type_id,
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
    let tags = symbols.tag_kinds();
    let first_tag = tags.and_then(|tags| tags.first()).copied();
    let decay_implicit_array = rest.is_empty();
    let resolved = resolve_symbol_inner(
        env,
        evaluation_namespace,
        symbol_namespace,
        name,
        first,
        first_tag,
        decay_implicit_array,
    )?;

    for (index, declaration) in rest.iter().enumerate() {
        let candidate = resolve_symbol_inner(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            declaration,
            tags.and_then(|tags| tags.get(index + 1)).copied(),
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
        HIRSymbolKind::Type(data) => data.base().range.clone(),
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
    tag: Option<HIRTagKind>,
    decay_implicit_array: bool,
) -> CXResult<MIRSymbol> {
    match &symbol.kind {
        HIRSymbolKind::Type(data) => match data {
            HIRSymbolData::Standard { base: standard, .. } => {
                let completed = complete_type_id(env, symbol_namespace, &standard)?;
                Ok(MIRSymbol::Type(completed))
            }

            HIRSymbolData::Template {
                base,
                template_data: _,
                template_prototype,
            } => {
                let source = HIRSymbol::new(
                    symbol.visibility,
                    HIRSymbolKind::Type(HIRSymbolData::Standard { base: base.clone() }),
                );

                Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(source),
                    namespace: symbol_namespace.clone(),
                    tag,
                })
            }
        },

        HIRSymbolKind::Function(data) => {
            let prototype_namespace =
                function_lexical_namespace(symbol_namespace, &data.base().kind);

            match data {
                HIRFunctionSymbol::Standard {
                    base: prototype, ..
                } => {
                    let prototype = complete_prototype(env, &prototype_namespace, prototype)?;
                    env.items.push_generated_function(THIRFunction {
                        prototype: prototype.clone(),
                        body: None,
                    });
                    Ok(MIRSymbol::FunctionReference(prototype))
                }
                HIRFunctionSymbol::Template {
                    base,
                    template_prototype,
                    ..
                } => Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(HIRSymbol::new(
                        symbol.visibility,
                        HIRSymbolKind::Function(HIRFunctionSymbol::Standard { base: base.clone() }),
                    )),
                    namespace: symbol_namespace.clone(),
                    tag: None,
                }),
            }
        }

        HIRSymbolKind::ComptimeFunction(data) => {
            let prototype_namespace =
                function_lexical_namespace(symbol_namespace, &data.base().kind);

            match data {
                HIRSymbolData::Standard { base: standard, .. } => {
                    let prototype =
                        complete_comptime_prototype(env, &prototype_namespace, standard)?;

                    Ok(MIRSymbol::ComptimeFunctionReference {
                        prototype,
                        namespace: prototype_namespace.clone(),
                        template_bindings: Vec::new(),
                    })
                }
                HIRSymbolData::Template {
                    base,
                    template_prototype,
                    ..
                } => Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(HIRSymbol::new(
                        symbol.visibility,
                        HIRSymbolKind::ComptimeFunction(HIRSymbolData::Standard {
                            base: base.clone(),
                        }),
                    )),
                    namespace: symbol_namespace.clone(),
                    tag: None,
                }),
            }
        }

        HIRSymbolKind::TypeConstructor(data) => match data {
            HIRSymbolData::Standard { base: standard, .. } => resolve_type_constructor(
                env,
                symbol_namespace,
                name,
                &standard.union_type,
                standard.variant_index,
            ),

            HIRSymbolData::Template {
                base,
                template_data: _,
                template_prototype,
            } => {
                let source = HIRSymbol::new(
                    symbol.visibility,
                    HIRSymbolKind::TypeConstructor(HIRSymbolData::Standard { base: base.clone() }),
                );

                Ok(MIRSymbol::Template {
                    template_prototype: template_prototype.clone(),
                    name: name.clone(),
                    source: Box::new(source),
                    namespace: symbol_namespace.clone(),
                    tag: None,
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
    WildcardTag,
}

pub(crate) struct ResolvedTypeSymbol<'a> {
    pub symbol: &'a HIRSymbol,
    pub tag: Option<HIRTagKind>,
}

pub(crate) fn resolve_type_symbol<'a>(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    query: TypeSymbolQuery,
    resolution: &'a SymbolResolution,
) -> CXMaybeRawResult<ResolvedTypeSymbol<'a>> {
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

    let declared_tags = resolution.tag_kinds();
    let tag = match query {
        TypeSymbolQuery::Standard => {
            if declared_tags.is_some() {
                return env
                    .log_error_base(format!("Symbol '{name}' has incompatible tag declarations"))
                    .map_err(|err| err.into());
            }
            None
        }
        TypeSymbolQuery::Tag(expected) => {
            let Some(tags) = declared_tags else {
                return env
                    .log_error_base(format!("Symbol '{name}' is not a tagged type"))
                    .map_err(|err| err.into());
            };
            if tags.iter().any(|tag| *tag != expected) {
                return env
                    .log_error_base(format!("Symbol '{name}' has incompatible tag declarations"))
                    .map_err(|err| err.into());
            }
            Some(expected)
        }
        TypeSymbolQuery::WildcardTag => {
            let Some((first, rest)) = declared_tags.and_then(|tags| tags.split_first()) else {
                return env
                    .log_error_base(format!("Symbol '{name}' is not a tagged type"))
                    .map_err(|err| err.into());
            };
            if rest.iter().any(|tag| tag != first) {
                return env
                    .log_error_base(format!("Symbol '{name}' has incompatible tag declarations"))
                    .map_err(|err| err.into());
            }
            Some(*first)
        }
    };

    if let Some(tag) = tag {
        let first_data = types[0].1;
        if types
            .iter()
            .skip(1)
            .any(|(_, data)| !type_template_kinds_equivalent(first_data, data))
        {
            return env
                .log_error_base(format!("Symbol '{name}' has incompatible tag declarations"))
                .map_err(|err| err.into());
        }

        let concrete = types
            .iter()
            .filter(|(_, data)| !is_forward_type_declaration(name, data.base(), tag))
            .collect::<Vec<_>>();
        if concrete.len() > 1 {
            return env
                .log_error_base(format!("Symbol '{name}' has multiple type definitions"))
                .map_err(|err| err.into());
        }

        let symbol = concrete.first().map_or(types[0].0, |candidate| candidate.0);
        return Ok(ResolvedTypeSymbol {
            symbol,
            tag: Some(tag),
        });
    }

    let first = types[0];
    for candidate in &types[1..] {
        if !type_declarations_equivalent(env, name, first.1, candidate.1)? {
            return env
                .log_error_base(format!("Symbol '{name}' has multiple type definitions"))
                .map_err(|err| err.into());
        }
    }

    Ok(ResolvedTypeSymbol {
        symbol: first.0,
        tag: None,
    })
}

fn type_declarations_equivalent(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    left: &HIRSymbolData<HIRType, ()>,
    right: &HIRSymbolData<HIRType, ()>,
) -> CXMaybeRawResult<bool> {
    match (left, right) {
        (
            HIRSymbolData::Template {
                base: left,
                template_prototype: left_template,
                ..
            },
            HIRSymbolData::Template {
                base: right,
                template_prototype: right_template,
                ..
            },
        ) => {
            let mut left = left.clone();
            let mut right = right.clone();
            left.range = TokenRange::internal();
            right.range = TokenRange::internal();
            return Ok(left_template == right_template && left == right);
        }
        (HIRSymbolData::Standard { .. }, HIRSymbolData::Standard { .. }) => {}
        _ => return Ok(false),
    }

    let namespace = EnvironmentNamespace::from(&name.namespace);
    let left = complete_type(env, &namespace, left.base())?;
    let right = complete_type(env, &namespace, right.base())?;
    Ok(env.type_eq(&left, &right))
}

fn type_template_kinds_equivalent(
    left: &HIRSymbolData<HIRType, ()>,
    right: &HIRSymbolData<HIRType, ()>,
) -> bool {
    match (left, right) {
        (HIRSymbolData::Standard { .. }, HIRSymbolData::Standard { .. }) => true,
        (
            HIRSymbolData::Template {
                template_prototype: left,
                ..
            },
            HIRSymbolData::Template {
                template_prototype: right,
                ..
            },
        ) => left == right,
        _ => false,
    }
}

fn is_forward_type_declaration(name: &QualifiedName, ty: &HIRType, tag: HIRTagKind) -> bool {
    matches!(
        &ty.kind,
        HIRTypeKind::Identifier {
            name: definition_name,
            lookup: HIRTypeLookup::Tag(definition_tag),
            template_input: None,
        } if *definition_tag == tag
            && definition_name.namespace.is_root()
            && definition_name.name == name.name
    )
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
        tag,
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
        resolve_symbol_inner(env, namespace, namespace, name, source, *tag, true)
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
        env.items.push_request(THIRFunctionGenRequest::Comptime {
            name: prototype.lookup_identifier().clone(),
            prototype: prototype.clone(),
            input: template_input.clone(),
        });
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
        HIRSymbolKind::Function(data) => function_lexical_namespace(&namespace, &data.base().kind),
        HIRSymbolKind::ComptimeFunction(data) => {
            function_lexical_namespace(&namespace, &data.base().kind)
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
