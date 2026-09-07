use cx_hir::{
    ast::{
        function::{HIRFunctionContract, HIRFunctionKind},
        types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
    },
    symbols::{HIRSymbol, HIRSymbolData, HIRSymbolKind},
};
use cx_log::{CXResult, error::CXMaybeRawResult};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::{
    symbol::MIRSymbol,
    thir::{
        contextual_eq::TypeContextEqual,
        data::{THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter},
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind, THIRLocalID},
        global::THIRGlobalVariable,
        name_mangling::mangle_template_name,
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
    },
};

pub fn resolve_symbol(
    env: &mut TypeEnvironment,
    evaluation_namespace: &NamespacePath,
    symbol_namespace: &NamespacePath,
    name: &CXIdent,
    symbols: &[HIRSymbol],
) -> CXResult<MIRSymbol> {
    let Some((first, rest)) = symbols.split_first() else {
        return env.log_error(
            TokenRange::internal(),
            format!("Symbol '{}' has no declarations", name),
        );
    };
    if symbols.iter().any(HIRSymbol::is_type) {
        return super::completion::complete_named_type(
            env,
            &QualifiedName::new(symbol_namespace.clone(), name.clone()),
            symbols,
        );
    }
    let decay_implicit_array = rest.is_empty();
    let resolved = resolve_symbol_inner(
        env,
        evaluation_namespace,
        symbol_namespace,
        name,
        first,
        first.tag,
        decay_implicit_array,
    )?;

    for declaration in rest {
        let candidate = resolve_symbol_inner(
            env,
            evaluation_namespace,
            symbol_namespace,
            name,
            declaration,
            declaration.tag,
            false,
        )?;
        if !mir_symbols_equivalent(env, &resolved, &candidate) {
            return env.log_error(
                symbol_range(declaration),
                format!("Symbol '{}' has incompatible declarations", name),
            );
        }
    }

    if let MIRSymbol::FunctionReference(prototype) = &resolved {
        env.items.push_generated_function(THIRFunction {
        require_explicit_return: env.require_explicit_return(),
            prototype: prototype.clone(),
            body: None,
        });
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

pub(crate) fn resolve_symbol_inner(
    env: &mut TypeEnvironment,
    evaluation_namespace: &NamespacePath,
    symbol_namespace: &NamespacePath,
    name: &CXIdent,
    symbol: &HIRSymbol,
    tag: Option<HIRTagKind>,
    decay_implicit_array: bool,
) -> CXResult<MIRSymbol> {
    let mut source = symbol.clone();
    let template = match &mut source.kind {
        HIRSymbolKind::Type(data) => data.template_prototype.take(),
        HIRSymbolKind::Function(data) => data.template_prototype.take(),
        HIRSymbolKind::ComptimeFunction(data) => data.template_prototype.take(),
        HIRSymbolKind::TypeConstructor(data) => data.template_prototype.take(),
        _ => None,
    };
    if let Some(prototype) = template {
        return Ok(MIRSymbol::Template {
            template_prototype: prototype,
            name: name.clone(),
            source: Box::new(source),
            namespace: symbol_namespace.clone(),
            tag,
        });
    }
    let namespace = symbol_lexical_namespace(symbol_namespace, symbol);
    match &symbol.kind {
        HIRSymbolKind::Type(data) => {
            complete_type_id(env, symbol_namespace, data.base()).map(MIRSymbol::Type)
        }
        HIRSymbolKind::Function(data) => {
            let prototype = complete_prototype(env, &namespace, data.base())?;
            Ok(MIRSymbol::FunctionReference(prototype))
        }
        HIRSymbolKind::ComptimeFunction(data) => {
            let prototype = complete_comptime_prototype(env, &namespace, data.base())?;
            Ok(MIRSymbol::ComptimeFunctionReference {
                prototype,
                input: cx_thir::thir::data::THIRTemplateInput { args: Vec::new() },
            })
        }
        HIRSymbolKind::TypeConstructor(data) => resolve_type_constructor(
            env,
            &namespace,
            name,
            &data.base().union_type,
            data.base().variant_index,
        ),

        HIRSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => resolve_enum_block(env, symbol_namespace, *enum_block_idx, *variant_index),

        HIRSymbolKind::AddressableGlobal {
            name,
            _type,
            symbol_naming,
        } => {
            let ty = complete_type(env, symbol_namespace, _type)?;
            let symbol_name = CXIdent::new(cx_thir::thir::name_mangling::mangle_rootable_name(
                env.symbols.get_global_registry(),
                &QualifiedName::new(symbol_namespace.clone(), name.clone()),
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

pub(crate) fn resolve_type_symbol<'a>(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    declarations: &'a [HIRSymbol],
) -> CXMaybeRawResult<&'a HIRSymbol> {
    let Some(first) = declarations.first() else {
        return env
            .log_error_base(format!("Type '{name}' has no declarations"))
            .map_err(Into::into);
    };
    let mut definition = None;
    for symbol in declarations {
        let (HIRSymbolKind::Type(data), HIRSymbolKind::Type(first_data)) =
            (&symbol.kind, &first.kind)
        else {
            return env
                .log_error_base(format!("Symbol '{name}' is not a type"))
                .map_err(Into::into);
        };
        if symbol.tag != first.tag || !type_template_kinds_equivalent(first_data, data) {
            return env
                .log_error_base(format!("Symbol '{name}' has incompatible tag declarations"))
                .map_err(Into::into);
        }
        if let Some(tag) = first.tag {
            if !is_forward_type_declaration(name, data.base(), tag)
                && definition.replace(symbol).is_some()
            {
                return env
                    .log_error_base(format!("Symbol '{name}' has multiple type definitions"))
                    .map_err(Into::into);
            }
        } else if !std::ptr::eq(symbol, first)
            && !type_declarations_equivalent(env, name, first_data, data)?
        {
            return env
                .log_error_base(format!("Symbol '{name}' has multiple type definitions"))
                .map_err(Into::into);
        }
    }
    Ok(definition.unwrap_or(first))
}

fn type_declarations_equivalent(
    env: &mut TypeEnvironment,
    name: &QualifiedName,
    left: &HIRSymbolData<HIRType, ()>,
    right: &HIRSymbolData<HIRType, ()>,
) -> CXMaybeRawResult<bool> {
    if left.template_prototype != right.template_prototype {
        return Ok(false);
    }
    if left.template_prototype.is_some() {
        let mut left = left.base.clone();
        let mut right = right.base.clone();
        left.range = TokenRange::internal();
        right.range = TokenRange::internal();
        return Ok(left == right);
    }

    let namespace = name.namespace.clone();
    let left = complete_type(env, &namespace, left.base())?;
    let right = complete_type(env, &namespace, right.base())?;
    Ok(env.type_eq(&left, &right))
}

fn type_template_kinds_equivalent(
    left: &HIRSymbolData<HIRType, ()>,
    right: &HIRSymbolData<HIRType, ()>,
) -> bool {
    left.template_prototype == right.template_prototype
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
                && match (&left.kind, &right.kind) {
                    (
                        THIRExpressionKind::GlobalVariable { symbol: left },
                        THIRExpressionKind::GlobalVariable { symbol: right },
                    ) => left == right,
                    (
                        THIRExpressionKind::IntLiteral(left),
                        THIRExpressionKind::IntLiteral(right),
                    ) => left == right,
                    _ => false,
                }
        }

        _ => false,
    }
}

fn resolve_type_constructor(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
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

    let mut symbol_name = cx_namespace::mangling::mangle_namespace_symbol(
        &union_type
            .lookup_identifier()
            .expect("named union constructor")
            .clone()
            .child(name.clone()),
    );
    if let Some(info) = &union_type.template_info {
        symbol_name = mangle_template_name(&env.symbols, symbol_name, &info.template_input);
    }
    let prototype = THIRFnPrototype::new(
        symbol_name,
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

pub fn symbol_lexical_namespace(namespace: &NamespacePath, symbol: &HIRSymbol) -> NamespacePath {
    let is_member = match &symbol.kind {
        HIRSymbolKind::Function(data) => {
            matches!(data.base().kind, HIRFunctionKind::AssociatedFunction { .. })
        }
        HIRSymbolKind::ComptimeFunction(data) => {
            matches!(data.base().kind, HIRFunctionKind::AssociatedFunction { .. })
        }
        HIRSymbolKind::TypeConstructor(_) => true,
        _ => false,
    };
    if is_member {
        namespace
            .clone()
            .parent()
            .unwrap_or_else(NamespacePath::root)
    } else {
        namespace.clone()
    }
}
