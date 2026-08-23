use std::collections::HashSet;

use cx_hir::ast::{
    function::{HIRComptimeFnPrototype, HIRFunctionKind, HIRFunctionPrototype},
    modifiers::{HIRSymbolNameScheme, HIRTypeQualifiers, VisibilityMode},
    template::HIRTemplateInput,
    types::{
        HIRAggregateAttributes, HIRField, HIRMoveSemantics, HIRType, HIRTypeKind,
        PredeclarationType,
    },
};
use cx_hir::symbols::{HIRSymbol, HIRSymbolKind};
use cx_log::{
    CXRawResult, CXResult,
    error::{CXMaybeRawErr, CXMaybeRawResult},
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use cx_thir::{
    symbol::MIRSymbol,
    thir::{
        data::{
            THIRComptimeFnPrototype, THIRComptimeParameter, THIRComptimeValueType, THIRFnPrototype,
            THIRFnSignature, THIRParameter, THIRTemplateInput, THIRTypeAttributes,
        },
        expression::THIRLocalID,
        r#type::{THIRField, THIRMoveSemantics, THIRType, THIRTypeID, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};

use crate::{
    EnvironmentNamespace,
    environment::{SymbolLookupKind, TypeEnvironment},
    symbol::{
        name_mangling::mangle_qualified_name,
        resolution::{apply_template, resolve_duplicate_type_symbol, resolve_symbol},
    },
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        typechecker::typecheck_expr,
    },
};

pub fn complete_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    input: &HIRTemplateInput,
) -> CXResult<THIRTemplateInput> {
    let args = input
        .params
        .iter()
        .map(|param| complete_type_id(env, namespace, param))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(THIRTemplateInput { args })
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &HIRType,
) -> CXResult<THIRType> {
    let id = complete_type_id(env, namespace, ty)?;

    let Some(completed) = env.symbols.try_resolve_type_id(id).cloned() else {
        return env.log_error(ty.range(), format!("Type '{}' is incomplete", ty));
    };

    Ok(completed)
}

pub fn complete_type_id(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &HIRType,
) -> CXResult<THIRTypeID> {
    match &ty.kind {
        HIRTypeKind::Identifier {
            name,
            predeclaration,
            template_input,
        } => {
            let id = complete_identifier_type(
                env,
                namespace,
                name,
                *predeclaration,
                template_input,
            )
                .map_err(|err| env.complete_maybe_err(err, ty.range()))?;

            Ok(apply_type_specifiers(env, id, ty.specifiers))
        }

        _ => {
            let completed = complete_type_inner(env, namespace, ty)?;

            Ok(env.symbols.generate_type_id(completed))
        }
    }
}

fn complete_type_inner(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &HIRType,
) -> CXResult<THIRType> {
    let mut completed = match &ty.kind {
        HIRTypeKind::Identifier {
            name,
            predeclaration,
            template_input,
        } => {
            let id = complete_identifier_type(
                env,
                namespace,
                name,
                *predeclaration,
                template_input,
            )
                .map_err(|err| env.complete_maybe_err(err, ty.range()))?;

            let Some(completed) = env.symbols.try_resolve_type_id(id).cloned() else {
                return env.log_error(ty.range(), format!("Type '{}' is incomplete", ty));
            };

            completed
        }

        HIRTypeKind::ExplicitSizedArray(inner, size) => {
            let id = complete_type_id(env, namespace, inner)?;
            ensure_valid_type_id_component(env, ty.range(), id, "an array element", true)?;

            let size = typecheck_expr(env, namespace, size, None)
                .and_then(|v| v.standard_ready_coerce(env, size.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;
            let integer_type = env.get_intrinsic_type("int");
            let size = implicit_cast(env, size, &integer_type)?;
            THIRTypeKind::Array {
                inner_type: id,
                length: Box::new(size),
            }
            .into()
        }

        HIRTypeKind::ImplicitSizedArray(inner) => {
            let id = complete_type_id(env, namespace, inner)?;
            ensure_valid_type_id_component(env, ty.range(), id, "a pointer target", true)?;

            THIRTypeKind::PointerTo { inner_type: id }.into()
        }

        HIRTypeKind::MemoryReference { inner_type } => {
            let inner_type = complete_type_id(env, namespace, inner_type)?;
            ensure_valid_type_id_component(
                env,
                ty.range(),
                inner_type,
                "a reference target",
                false,
            )?;

            THIRTypeKind::MemoryReference {
                inner_type,
                bitfield: None,
            }
            .into()
        }

        HIRTypeKind::PointerTo { inner_type } => {
            let inner_type = complete_type_id(env, namespace, inner_type)?;
            ensure_valid_type_id_component(env, ty.range(), inner_type, "a pointer target", false)?;

            THIRTypeKind::PointerTo { inner_type }.into()
        }

        HIRTypeKind::Structured {
            name,
            attributes,
            fields,
        } => make_aggregate_type(
            env,
            namespace,
            ty,
            name.clone(),
            Some(attributes),
            fields,
            |fields| THIRTypeKind::Structured { fields },
        )?,

        HIRTypeKind::Union { name, fields } => {
            make_aggregate_type(env, namespace, ty, name.clone(), None, fields, |variants| {
                THIRTypeKind::Union { variants }
            })?
        }

        HIRTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
        } => make_aggregate_type(
            env,
            namespace,
            ty,
            Some(name.clone()),
            Some(attributes),
            variants,
            |variants| THIRTypeKind::TaggedUnion { variants },
        )?,

        HIRTypeKind::FunctionPointer { prototype } => {
            let prototype = complete_prototype(env, namespace, prototype)?;
            THIRTypeKind::Function {
                signature: Box::new(prototype.signature().clone()),
            }
            .into()
        }
    };

    completed.specifiers = ty.specifiers;
    Ok(completed)
}

pub fn ensure_valid_type_id_component(
    env: &TypeEnvironment,
    range: &TokenRange,
    ty: THIRTypeID,
    context: &str,
    enforce_allocatable: bool,
) -> CXResult<()> {
    let Some(ty) = env.symbols.try_resolve_type_id(ty) else {
        return env.log_error(range, format!("{} type is incomplete", context));
    };

    ensure_valid_type_component(env, range, ty, context, enforce_allocatable)
}

pub fn ensure_valid_type_component(
    env: &TypeEnvironment,
    range: &TokenRange,
    ty: &THIRType,
    context: &str,
    enforce_allocatable: bool,
) -> CXResult<()> {
    match &ty.kind {
        THIRTypeKind::Unreachable => env.log_error(
            range,
            format!("{} type component cannot be 'unreachable'", context),
        ),

        THIRTypeKind::Function { .. }
        | THIRTypeKind::Str
        | THIRTypeKind::Undefined
        | THIRTypeKind::Void
            if enforce_allocatable =>
        {
            env.log_error(
                range,
                format!(
                    "{} type is unsized and cannot be directly allocated",
                    context
                ),
            )
        }

        _ => Ok(()),
    }
}

fn apply_type_specifiers(
    env: &mut TypeEnvironment,
    id: THIRTypeID,
    specifiers: HIRTypeQualifiers,
) -> THIRTypeID {
    if specifiers == 0 {
        return id;
    }

    let Some(mut ty) = env.symbols.try_resolve_type_id(id).cloned() else {
        return id;
    };

    ty.specifiers |= specifiers;

    env.symbols.generate_type_id(ty)
}

pub fn complete_prototype(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &HIRFunctionPrototype,
) -> CXResult<THIRFnPrototype> {
    let return_type_id = complete_type_id(env, namespace, &prototype.return_type)?;
    let mut params = complete_explicit_parameters(env, namespace, prototype)?;

    let return_type = env.symbols.resolve_type_id(return_type_id).clone();

    if !return_type.is_unreachable() && !return_type.is_void() {
        ensure_valid_type_id_component(
            env,
            &prototype.range,
            return_type_id,
            "a function return value",
            true,
        )?;
    }

    // If we have legacy int main(void)-like syntax, we treat it as main with no parameters
    if params.len() == 1 {
        let first_param = &params[0];

        if first_param._type.is_void() && first_param.name.is_none() {
            params.clear();
        }
    }

    let lookup_identifier = function_lookup_identifier(namespace, &prototype.kind);
    let debug_name = lookup_identifier.name.clone();
    let symbol_name =
        completed_function_name(env, namespace, &prototype.kind, prototype.symbol_naming)?;

    Ok(THIRFnPrototype::new(
        symbol_name,
        prototype.linkage,
        THIRFnSignature {
            return_type,
            params,
            var_args: prototype.var_args,
            contract: prototype.contract.clone(),
        },
    ))
    .map(|prototype| {
        prototype
            .with_lookup_identifier(lookup_identifier)
            .with_debug_name(debug_name)
    })
}

pub fn complete_comptime_prototype(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &HIRComptimeFnPrototype,
) -> CXResult<THIRComptimeFnPrototype> {
    let return_type = THIRComptimeValueType {
        expr: prototype.return_type.expr,
        params: prototype
            .return_type
            .params
            .iter()
            .map(|param| complete_type(env, namespace, param))
            .collect::<CXResult<Vec<_>>>()?,
        _type: complete_type(env, namespace, &prototype.return_type._type)?,
    };
    let params = prototype
        .params
        .iter()
        .map(|param| {
            Ok(THIRComptimeParameter {
                name: param.name.clone(),
                local_id: THIRLocalID::fresh(),
                value_type: THIRComptimeValueType {
                    expr: param.value_type.expr,
                    params: param
                        .value_type
                        .params
                        .iter()
                        .map(|param| complete_type(env, namespace, param))
                        .collect::<CXResult<Vec<_>>>()?,
                    _type: complete_type(env, namespace, &param.value_type._type)?,
                },
            })
        })
        .collect::<CXResult<Vec<_>>>()?;

    let lookup_identifier = function_lookup_identifier(namespace, &prototype.kind);
    let debug_name = lookup_identifier.name.clone();
    let symbol_name = completed_comptime_symbol_name(env, &lookup_identifier);

    Ok(
        THIRComptimeFnPrototype::new(symbol_name, return_type, params)
            .with_lookup_identifier(lookup_identifier)
            .with_debug_name(debug_name),
    )
}

fn completed_comptime_symbol_name(
    env: &TypeEnvironment,
    lookup_identifier: &QualifiedName,
) -> String {
    crate::symbol::name_mangling::mangle_qualified_name(
        env.symbols.get_global_registry(),
        lookup_identifier,
    )
}

fn function_lookup_identifier(
    namespace: &EnvironmentNamespace,
    kind: &HIRFunctionKind,
) -> QualifiedName {
    let QualifiedName {
        namespace: relative_namespace,
        name,
    } = kind.into_key();

    QualifiedName::new(namespace.join(&relative_namespace), name)
}

fn complete_explicit_parameters(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &HIRFunctionPrototype,
) -> CXResult<Vec<THIRParameter>> {
    prototype
        .params
        .iter()
        .map(|param| {
            let completed = complete_type(env, namespace, &param._type)?;
            let _type = if let Some(inner) = env.symbols.array_inner(&completed) {
                env.symbols.pointer_to(inner.clone())
            } else {
                completed
            };
            Ok(THIRParameter {
                name: param.name.clone(),
                local_id: THIRLocalID::fresh(),
                _type,
            })
        })
        .collect::<CXResult<Vec<_>>>()
}

fn complete_identifier_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    predeclaration: PredeclarationType,
    template_input: &Option<HIRTemplateInput>,
) -> CXMaybeRawResult<THIRTypeID> {
    let Some(lookup) = env.lookup_symbol(namespace, name)? else {
        return env
            .log_error_base(format!("Type not found: {}", name))
            .map_err(|err| err.into());
    };

    let resolved_name = lookup.resolved_name;

    let symbol = match lookup.kind {
        SymbolLookupKind::Resolved(symbol) => {
            return complete_resolved_type_lookup(env, namespace, name, symbol, template_input);
        }
        SymbolLookupKind::Untyped(symbol) => symbol,
    };

    let symbol_kind = match &symbol.kind {
        HIRSymbolKind::DuplicateDefinition(definitions) => {
            resolve_duplicate_type_symbol(env, name, predeclaration, definitions)?
        }

        _ => &symbol.kind,
    };

    let cacheable = match symbol_kind {
        HIRSymbolKind::Type(_) => {
            predeclaration == PredeclarationType::None
                && !matches!(&symbol.kind, HIRSymbolKind::DuplicateDefinition(_))
        }
        HIRSymbolKind::TagType { tag, .. } => {
            *tag == predeclaration || predeclaration == PredeclarationType::None
        }
        _ => false,
    } && template_input.is_none();
    if cacheable
        && let Some(MIRSymbol::Type(id)) = env.symbols.get_preresolved_symbol(&resolved_name)
    {
        return Ok(*id);
    }

    match symbol_kind {
        HIRSymbolKind::Type(definition)
            if predeclaration == PredeclarationType::None =>
        {
            if template_input.is_some() {
                return env
                    .log_error_base(format!("Type '{name}' does not accept template arguments"))
                    .map_err(|err| err.into());
            }

            let dummy_type =
                THIRType::from(THIRTypeKind::Undefined).with_strong_identifier(CXIdent::from(
                    mangle_qualified_name(env.symbols.get_global_registry(), &resolved_name),
                ));
            let prereserved_id = env.symbols.reserve_type_id();
            if cacheable {
                env.symbols
                    .insert_type_symbol(resolved_name.clone(), prereserved_id);
            }
            env.symbols.overwrite_type_id(prereserved_id, dummy_type);

            let completed = complete_type(
                env,
                &EnvironmentNamespace::from(&resolved_name.namespace),
                definition,
            )?;

            env.symbols.overwrite_type_id(prereserved_id, completed);
            Ok(prereserved_id)
        }

        HIRSymbolKind::TagType { definition, tag }
            if *tag == predeclaration || predeclaration == PredeclarationType::None =>
        {
            if template_input.is_some() {
                return env
                    .log_error_base(format!("Type '{name}' does not accept template arguments"))
                    .map_err(|err| err.into());
            }

            let dummy_type =
                THIRType::from(THIRTypeKind::Undefined).with_strong_identifier(CXIdent::from(
                    mangle_qualified_name(env.symbols.get_global_registry(), &resolved_name),
                ));
            let prereserved_id = env.symbols.reserve_type_id();
            if cacheable {
                env.symbols
                    .insert_type_symbol(resolved_name.clone(), prereserved_id);
            }
            env.symbols.overwrite_type_id(prereserved_id, dummy_type);

            if is_self_predeclaration(definition, &resolved_name) {
                return Ok(prereserved_id);
            }

            let completed = complete_type(
                env,
                &EnvironmentNamespace::from(&resolved_name.namespace),
                definition,
            )?;

            env.symbols.overwrite_type_id(prereserved_id, completed);
            Ok(prereserved_id)
        }

        HIRSymbolKind::TypeTemplate { .. } if predeclaration == PredeclarationType::None => {
            let type_symbol = HIRSymbol::new(symbol.visibility, symbol_kind.clone());
            let mir_symbol = resolve_symbol(
                env,
                namespace,
                &EnvironmentNamespace::from(&resolved_name.namespace),
                &resolved_name.name,
                &type_symbol,
            )?;

            complete_template_type_lookup(env, namespace, name, &mir_symbol, template_input)
        }

        HIRSymbolKind::TagTypeTemplate { tag, .. }
            if *tag == predeclaration || predeclaration == PredeclarationType::None =>
        {
            let type_symbol = HIRSymbol::new(symbol.visibility, symbol_kind.clone());
            let mir_symbol = resolve_symbol(
                env,
                namespace,
                &EnvironmentNamespace::from(&resolved_name.namespace),
                &resolved_name.name,
                &type_symbol,
            )?;

            complete_template_type_lookup(env, namespace, name, &mir_symbol, template_input)
        }

        HIRSymbolKind::DuplicateDefinition(_) => {
            let mir_symbol = resolve_symbol(
                env,
                namespace,
                &EnvironmentNamespace::from(&resolved_name.namespace),
                &resolved_name.name,
                &symbol,
            )?;

            complete_resolved_type_lookup(env, namespace, name, mir_symbol, template_input)
        }

        _ => env
            .log_error_base(format!("Symbol '{name}' is not a type"))
            .map_err(|err| err.into()),
    }
}

fn is_self_predeclaration(definition: &HIRType, name: &QualifiedName) -> bool {
    let HIRTypeKind::Identifier {
        name: definition_name,
        predeclaration,
        template_input: None,
    } = &definition.kind
    else {
        return false;
    };

    *predeclaration != PredeclarationType::None
        && definition_name.namespace.is_root()
        && definition_name.name == name.name
}

fn complete_resolved_type_lookup(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    symbol: MIRSymbol,
    template_input: &Option<HIRTemplateInput>,
) -> CXMaybeRawResult<THIRTypeID> {
    match symbol {
        MIRSymbol::Type(id) => {
            if template_input.is_some() {
                env.log_error_base(format!("Type '{name}' does not accept template arguments"))
                    .map_err(|e| e.into())
            } else {
                Ok(id)
            }
        }
        MIRSymbol::Template { .. } => {
            complete_template_type_lookup(env, namespace, name, &symbol, template_input)
        }

        _ => env
            .log_error_base(format!("Symbol '{name}' is not a type"))
            .map_err(|err| err.into()),
    }
}

fn complete_template_type_lookup(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    mir_symbol: &MIRSymbol,
    template_input: &Option<HIRTemplateInput>,
) -> CXMaybeRawResult<THIRTypeID> {
    let Some(input) = template_input else {
        return env
            .log_error_base(format!("Type '{name}' requires template arguments"))
            .map_err(|e| e.into());
    };
    let input = complete_template_input(env, namespace, input)?;
    let Some(symbol) = apply_template(env, mir_symbol, input)? else {
        return env
            .log_error_base("Failed to apply template arguments".to_string())
            .map_err(|e| e.into());
    };

    match symbol {
        MIRSymbol::Type(id) => Ok(id),
        MIRSymbol::Template { .. } => env
            .log_error_base(format!(
                "Template arguments did not resolve type '{name}' to a concrete type"
            ))
            .map_err(|e| e.into()),
        _ => env
            .log_error_base(format!("Symbol '{name}' is not a type"))
            .map_err(|err| err.into()),
    }
}

fn make_aggregate_type<F>(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &HIRType,
    name: Option<CXIdent>,
    attributes: Option<&HIRAggregateAttributes>,
    fields: &[HIRField],
    kind_ctor: F,
) -> CXResult<THIRType>
where
    F: FnOnce(Vec<THIRField>) -> THIRTypeKind,
{
    let fields = fields
        .iter()
        .map(|field| complete_field(env, namespace, field))
        .collect::<CXResult<Vec<_>>>()?;
    ensure_aggregate_fields_complete(env, &fields)
        .map_err(|err| env.complete_err(err, ty.range()))?;
    let move_attributes = resolve_aggregate_move_attributes(env, namespace, attributes)
        .map_err(|err| env.complete_maybe_err(err, ty.range()))?;
    let unsafe_move = attributes
        .map(|attributes| attributes.unsafe_move)
        .unwrap_or(false);
    ensure_aggregate_move_restrictions(env, move_attributes, unsafe_move, &fields)
        .map_err(|err| env.complete_err(err, ty.range()))?;

    let (strong_identifier, lookup_identifier) = name
        .as_ref()
        .map(|name| {
            let lookup_identifier = QualifiedName::new(namespace.clone(), name.clone());
            let strong_identifier =
                mangle_qualified_name(env.symbols.get_global_registry(), &lookup_identifier);
            (Some(strong_identifier), Some(lookup_identifier))
        })
        .unwrap_or((None, None));

    if let Some(strong_identifier) = &strong_identifier {
        ensure_aggregate_fields_not_recursive(env, &fields, strong_identifier)
            .map_err(|err| env.complete_err(err, ty.range()))?;
    }

    Ok(THIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        attributes: THIRTypeAttributes {
            semantics: move_attributes,
            unsafe_move,
            ..Default::default()
        },
        strong_identifier,
        lookup_identifier,
        template_info: None,
        kind: kind_ctor(fields),
    })
}

fn ensure_aggregate_fields_not_recursive(
    env: &TypeEnvironment,
    fields: &[THIRField],
    aggregate_identifier: &str,
) -> CXRawResult<()> {
    for field in fields {
        let mut visited = HashSet::new();
        if type_contains_by_value(env, field.ty(), aggregate_identifier, &mut visited) {
            let name = field.name().unwrap_or("<anonymous>");
            return env.log_error_base(format!("Aggregate field '{}' has recursive type", name));
        }
    }

    Ok(())
}

fn ensure_aggregate_fields_complete(
    env: &TypeEnvironment,
    fields: &[THIRField],
) -> CXRawResult<()> {
    for field in fields {
        let id = field.ty();
        if !env.symbols.contains(id) {
            let name = field.name().unwrap_or("<anonymous>");
            return env.log_error_base(format!("Aggregate field '{}' has incomplete type", name));
        }
        if env.symbols.resolve_type_id(id).is_unreachable() {
            let name = field.name().unwrap_or("<anonymous>");
            return env.log_error_base(format!(
                "Aggregate field '{}' cannot have type 'unreachable'",
                name
            ));
        }
    }

    Ok(())
}

fn ensure_aggregate_move_restrictions(
    env: &TypeEnvironment,
    aggregate_attributes: THIRMoveSemantics,
    aggregate_unsafe_move: bool,
    fields: &[THIRField],
) -> CXRawResult<()> {
    for field in fields {
        let field_type = env.symbols.resolve_type_id(field.ty());
        let field_attributes = owned_move_attributes(env, field_type);
        let name = field.name().unwrap_or("<anonymous>");

        if field_attributes.is_nodrop() && !aggregate_attributes.is_nodrop() {
            return env.log_error_base(format!(
                "Aggregate containing nodrop field '{}' must also be marked as @nodrop",
                name
            ));
        }

        if field_attributes.is_nocopy() && !aggregate_attributes.is_nocopy() {
            return env.log_error_base(format!(
                "Aggregate containing nocopy field '{}' must also be marked as @nodrop",
                name
            ));
        }

        if owned_unsafe_move(env, field_type) && !aggregate_unsafe_move {
            return env.log_error_base(format!(
                "Aggregate containing unsafe_move field '{}' must also be marked as @unsafe_move",
                name
            ));
        }
    }

    Ok(())
}

fn type_contains_by_value(
    env: &TypeEnvironment,
    id: THIRTypeID,
    aggregate_identifier: &str,
    visited: &mut HashSet<THIRTypeID>,
) -> bool {
    if !visited.insert(id) {
        return false;
    }

    let Some(ty) = env.symbols.try_resolve_type_id(id) else {
        return false;
    };

    if ty.strong_identifier() == Some(aggregate_identifier) {
        return true;
    }

    match &ty.kind {
        THIRTypeKind::Structured { fields }
        | THIRTypeKind::Union { variants: fields }
        | THIRTypeKind::TaggedUnion { variants: fields } => fields
            .iter()
            .any(|field| type_contains_by_value(env, field.ty(), aggregate_identifier, visited)),
        THIRTypeKind::Array { inner_type, .. } => {
            type_contains_by_value(env, *inner_type, aggregate_identifier, visited)
        }
        THIRTypeKind::PointerTo { .. }
        | THIRTypeKind::MemoryReference { .. }
        | THIRTypeKind::Function { .. } => false,
        THIRTypeKind::Void
        | THIRTypeKind::Integer { .. }
        | THIRTypeKind::Float { .. }
        | THIRTypeKind::Opaque { .. }
        | THIRTypeKind::Undefined
        | THIRTypeKind::Unreachable
        | THIRTypeKind::Str => false,
    }
}

fn resolve_aggregate_move_attributes(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    attributes: Option<&HIRAggregateAttributes>,
) -> CXMaybeRawResult<THIRMoveSemantics> {
    let Some(attributes) = attributes else {
        return Ok(THIRMoveSemantics::default());
    };

    let move_attributes = match attributes.semantics {
        HIRMoveSemantics::POD => THIRMoveSemantics::POD,
        HIRMoveSemantics::Nocopy => THIRMoveSemantics::Nocopy,
        HIRMoveSemantics::Nodrop => THIRMoveSemantics::Nodrop,
    };

    if let Some(param_name) = &attributes.copy_traits {
        let name = QualifiedName::new_raw(CXIdent::new(param_name.as_str()));
        let Some(symbol) = env
            .get_symbol(namespace, &name)
            .map_err(CXMaybeRawErr::from)?
        else {
            return env
                .log_error_base(format!(
                    "copy_traits target '{}' is not a valid type",
                    param_name
                ))
                .map_err(|e| e.into());
        };
        let Some(id) = symbol.as_type_id() else {
            return env
                .log_error_base(format!("copy_traits target '{}' is not a type", param_name))
                .map_err(|e| e.into());
        };
        let source_attributes = owned_move_attributes(env, env.symbols.resolve_type_id(id));

        return Ok(source_attributes);
    }

    Ok(move_attributes)
}

fn complete_field(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    field: &HIRField,
) -> CXResult<THIRField> {
    match field {
        HIRField::Standard { name, _type } => {
            let id = complete_type_id(env, namespace, _type)?;

            if !env.symbols.contains(id) {
                return env.log_error(
                    _type.range(),
                    format!("Aggregate field '{}' has incomplete type", name),
                );
            }

            Ok(THIRField::standard(name.clone(), id))
        }

        HIRField::Bitfield {
            name,
            integer_type,
            width,
        } => {
            let id = complete_type_id(env, namespace, integer_type)?;
            if !env.symbols.contains(id) {
                let name = name.as_deref().unwrap_or("<anonymous>");
                return env.log_error(
                    integer_type.range(),
                    format!("Bitfield '{}' has incomplete type", name),
                );
            }

            Ok(THIRField::Bitfield {
                name: name.clone(),
                integer_type_id: id,
                width: *width,
            })
        }
    }
}

fn owned_move_attributes(env: &TypeEnvironment, ty: &THIRType) -> THIRMoveSemantics {
    match &ty.kind {
        THIRTypeKind::Structured { .. }
        | THIRTypeKind::Union { .. }
        | THIRTypeKind::TaggedUnion { .. } => ty.attributes.semantics,
        THIRTypeKind::Array { inner_type, .. } => {
            owned_move_attributes(env, env.symbols.resolve_type_id(*inner_type))
        }
        _ => THIRMoveSemantics::default(),
    }
}

fn owned_unsafe_move(env: &TypeEnvironment, ty: &THIRType) -> bool {
    match &ty.kind {
        THIRTypeKind::Structured { .. }
        | THIRTypeKind::Union { .. }
        | THIRTypeKind::TaggedUnion { .. } => ty.attributes.unsafe_move,
        THIRTypeKind::Array { inner_type, .. } => {
            owned_unsafe_move(env, env.symbols.resolve_type_id(*inner_type))
        }
        _ => false,
    }
}

fn completed_function_name(
    env: &TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &HIRFunctionKind,
    symbol_naming: HIRSymbolNameScheme,
) -> CXResult<String> {
    if symbol_naming == HIRSymbolNameScheme::Unmangled {
        return Ok(kind.into_key().name.to_string());
    }

    let name = match kind {
        HIRFunctionKind::Standard(name) => mangle_qualified_name(
            env.symbols.get_global_registry(),
            &QualifiedName::new(namespace.clone(), name.clone()),
        ),
        HIRFunctionKind::AssociatedFunction {
            namespace: associated_namespace,
            name,
        } => cx_util::namespace::mangle_namespace_symbol(&QualifiedName::new(
            namespace.child(associated_namespace.clone()),
            name.clone(),
        )),
    };

    Ok(name)
}

pub(crate) fn completed_symbol_name(
    env: &TypeEnvironment,
    name: QualifiedName,
    symbol_naming: HIRSymbolNameScheme,
) -> String {
    match symbol_naming {
        HIRSymbolNameScheme::Namespaced => {
            mangle_qualified_name(env.symbols.get_global_registry(), &name)
        }
        HIRSymbolNameScheme::Unmangled => name.name.to_string(),
    }
}
