use cx_ast::ast::{
    function::{CXFunctionKind, CXFunctionPrototype, CXReceiverMode},
    modifiers::{CXTypeQualifiers, VisibilityMode},
    template::CXTemplateInput,
    types::{CXField, CXStructAttributes, CXType, CXTypeKind, PredeclarationType},
};
use cx_ast::symbols::{CXSymbol, CXSymbolKind};
use cx_log::{CXError, CXResult};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use cx_mir::{
    mir::{
        data::{
            MIRFunctionPrototype, MIRFunctionSignature, MIRMoveAttributes, MIRParameter,
            MIRTemplateInput,
        },
        name_mangling::{base_mangle_member, base_mangle_standard},
        r#type::{MIRField, MIRType, MIRTypeId, MIRTypeKind},
    },
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};

use crate::{
    EnvironmentNamespace,
    environment::TypeEnvironment,
    symbol::resolution::{apply_template, resolve_symbol},
    type_checking::{constexpr::constexpr_evaluate, typechecker::typecheck_expr},
};

pub fn complete_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    input: &CXTemplateInput,
) -> CXResult<MIRTemplateInput> {
    let args = input
        .params
        .iter()
        .map(|param| complete_type_id(env, namespace, param))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args })
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
) -> CXResult<MIRType> {
    let id = complete_type_id(env, namespace, ty)?;
    let Some(completed) = env.symbols.try_resolve_type_id(id).cloned() else {
        return type_completion_error(env, ty.range(), format!("Type '{}' is incomplete", ty));
    };

    Ok(completed)
}

pub fn complete_type_id(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
) -> CXResult<MIRTypeId> {
    match &ty.kind {
        CXTypeKind::Identifier {
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
                ty.range(),
            )?;
            apply_type_specifiers(env, id, ty.specifiers)
        }

        _ => {
            let completed = complete_type_value(env, namespace, ty)?;
            Ok(env.symbols.generate_type_id(completed))
        }
    }
}

fn complete_type_value(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
) -> CXResult<MIRType> {
    let mut completed = match &ty.kind {
        CXTypeKind::Identifier {
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
                ty.range(),
            )?;
            env.symbols
                .try_resolve_type_id(id)
                .cloned()
                .ok_or_else(|| {
                    crate::typecheck_error!(env, ty.range().cloned(), "Type '{}' is incomplete", ty)
                })?
        }

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = complete_type_id(env, namespace, inner)?;
            let size = typecheck_expr(env, namespace, size, None)
                .and_then(|v| v.standard_ready_coerce(env, size.token_range()))
                .and_then(|v| constexpr_evaluate(env, v))
                .and_then(|v| {
                    v.get_integer().ok_or_else(|| {
                        CXError::create_boxed("Array size must be an integer literal")
                    })
                })?;
            MIRTypeKind::Array {
                inner_type,
                length: size as usize,
            }
            .into()
        }

        CXTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = complete_type_id(env, namespace, inner)?;

            MIRTypeKind::PointerTo { inner_type }.into()
        }

        CXTypeKind::MemoryReference { inner_type } => {
            let inner_type = complete_type_id(env, namespace, inner_type)?;

            MIRTypeKind::MemoryReference {
                inner_type,
                bitfield: None,
            }
            .into()
        }

        CXTypeKind::PointerTo { inner_type } => {
            let inner_type = complete_type_id(env, namespace, inner_type)?;

            MIRTypeKind::PointerTo { inner_type }.into()
        }

        CXTypeKind::Structured {
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
            |fields| MIRTypeKind::Structured { fields },
        )?,

        CXTypeKind::Union { name, fields } => {
            make_aggregate_type(env, namespace, ty, name.clone(), None, fields, |variants| {
                MIRTypeKind::Union { variants }
            })?
        }

        CXTypeKind::TaggedUnion {
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
            |variants| MIRTypeKind::TaggedUnion { variants },
        )?,

        CXTypeKind::FunctionPointer { prototype } => {
            let prototype = complete_prototype(env, namespace, prototype)?;
            MIRTypeKind::Function {
                signature: Box::new(prototype.signature().clone()),
            }
            .into()
        }
    };

    completed.specifiers = ty.specifiers;
    Ok(completed)
}

fn apply_type_specifiers(
    env: &mut TypeEnvironment,
    id: MIRTypeId,
    specifiers: CXTypeQualifiers,
) -> CXResult<MIRTypeId> {
    if specifiers == 0 {
        return Ok(id);
    }

    let Some(mut ty) = env.symbols.try_resolve_type_id(id).cloned() else {
        return Ok(id);
    };
    ty.specifiers |= specifiers;

    Ok(env.symbols.generate_type_id(ty))
}

pub fn complete_prototype(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let return_type = complete_type(env, &namespace, &prototype.return_type)?;
    let mut params = complete_explicit_parameters(env, namespace, prototype)?;

    if let Some(receiver) = complete_receiver_parameter(env, namespace, &prototype.kind)? {
        params.insert(0, receiver);
    }

    // If we have legacy int main(void)-like syntax, we treat it as main with no parameters
    if params.len() == 1 {
        let first_param = &params[0];

        if first_param._type.is_unit() && first_param.name.is_none() {
            params.clear();
        }
    }

    let lookup_identifier = function_lookup_identifier(namespace, &prototype.kind);
    let debug_name = lookup_identifier.name.clone();
    let symbol_name = completed_function_name(env, &namespace, &prototype.kind)?;

    Ok(MIRFunctionPrototype::new(
        symbol_name,
        prototype.linkage,
        MIRFunctionSignature {
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

fn function_lookup_identifier(
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> QualifiedName {
    let QualifiedName {
        namespace: relative_namespace,
        name,
    } = kind.into_key();

    QualifiedName::new(namespace.join(&relative_namespace), name)
}

fn complete_receiver_parameter(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> CXResult<Option<MIRParameter>> {
    let CXFunctionKind::MemberFunction {
        member_type,
        receiver,
        ..
    } = kind
    else {
        return Ok(None);
    };

    let receiver_base = member_type.as_type().add_specifier(receiver.specifiers);
    let receiver_type = complete_type(env, namespace, &receiver_base)?;
    let receiver_type = match receiver.mode {
        CXReceiverMode::ByMove => receiver_type,
        CXReceiverMode::ByRef => env.symbols.mem_ref_to(receiver_type),
        CXReceiverMode::None => return Ok(None),
    };

    Ok(Some(MIRParameter {
        name: Some(CXIdent::new("this")),
        _type: receiver_type,
    }))
}

fn complete_explicit_parameters(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &CXFunctionPrototype,
) -> CXResult<Vec<MIRParameter>> {
    prototype
        .params
        .iter()
        .map(|param| {
            Ok(MIRParameter {
                name: param.name.clone(),
                _type: complete_type(env, &namespace, &param._type)?,
            })
        })
        .collect::<CXResult<Vec<_>>>()
}

fn complete_identifier_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    predeclaration: PredeclarationType,
    template_input: &Option<CXTemplateInput>,
    range: Option<&cx_tokens::TokenRange>,
) -> CXResult<MIRTypeId> {
    if name.namespace.is_root()
        && let Some(_ty) = env.symbols.get_local_symbol(name)
        && let Some(id) = _ty.as_type_id()
    {
        return Ok(id);
    }

    if let Some(_ty) = env.symbols.get_preresolved_symbol(name) {
        return Ok(_ty.as_type_id().unwrap()); // unfailable
    }

    let candidates = env
        .symbols
        .get_global_registry()
        .resolve_qualified_aliases(namespace, name);

    let mut resolved = Vec::new();
    for candidate in candidates.iter().cloned() {
        let lookup = if let Some(_ty) = env.symbols.get_preresolved_symbol(&candidate) {
            Some(TypeLookup::Resolved(_ty.as_type_id().unwrap()))
        } else {
            env.symbols
                .get_global_registry()
                .resolve(&candidate)
                .map(TypeLookup::Untyped)
        };

        let Some(lookup) = lookup else {
            continue;
        };

        if name.namespace.is_root() && candidate.namespace == *namespace {
            return complete_identifier_type_lookup(
                env,
                namespace,
                name,
                candidate,
                lookup,
                template_input,
                range,
            );
        }

        resolved.push((candidate, lookup));
    }

    let Some((resolved_name, lookup)) = resolved.pop() else {
        if predeclaration != PredeclarationType::None {
            let id = env.symbols.reserve_type_id();
            let reserve_name = candidates.first().cloned().unwrap_or_else(|| name.clone());
            env.symbols.insert_type_symbol(reserve_name, id);

            return Ok(id);
        }

        return type_completion_error(env, range, format!("Type not found: {name}"));
    };

    if !resolved.is_empty() {
        let mut candidates = resolved
            .iter()
            .map(|(name, _)| name.as_flat_name())
            .collect::<Vec<_>>();
        candidates.push(resolved_name.as_flat_name());
        return Err(crate::typecheck_error!(
            env,
            range.cloned(),
            "Type '{name}' is ambiguous; candidates: {}",
            candidates.join(", ")
        ));
    }

    complete_identifier_type_lookup(
        env,
        namespace,
        name,
        resolved_name,
        lookup,
        template_input,
        range,
    )
}

fn complete_identifier_type_lookup(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    resolved_name: QualifiedName,
    lookup: TypeLookup,
    template_input: &Option<CXTemplateInput>,
    range: Option<&cx_tokens::TokenRange>,
) -> CXResult<MIRTypeId> {
    if let TypeLookup::Resolved(id) = lookup {
        return Ok(id);
    }

    let TypeLookup::Untyped(symbol) = lookup else {
        unreachable!("resolved lookup was handled above")
    };

    match &symbol.kind {
        CXSymbolKind::Type(definition) => {
            if template_input.is_some() {
                return type_completion_error(
                    env,
                    range,
                    format!("Type '{name}' does not accept template arguments"),
                );
            }

            let prereserved_id = env.symbols.reserve_type_id();
            env.symbols
                .insert_type_symbol(resolved_name.clone(), prereserved_id);

            let mut completed = complete_type_value(env, &resolved_name.namespace, definition)?;
            if completed.debug_name.is_none() {
                completed.debug_name = Some(resolved_name.name.clone());
            }

            env.symbols.overwrite_type_id(prereserved_id, completed);
            Ok(prereserved_id)
        }

        CXSymbolKind::TypeTemplate { .. } => {
            let mir_symbol = resolve_symbol(
                env,
                namespace,
                &resolved_name.namespace,
                &resolved_name.name,
                &symbol,
            )?;
            let Some(input) = template_input else {
                return type_completion_error(
                    env,
                    range,
                    format!("Template type '{name}' requires explicit template arguments"),
                );
            };
            let input = complete_template_input(env, namespace, input)?;
            let symbol = apply_template(env, &mir_symbol, input)?.ok_or_else(|| {
                crate::typecheck_error!(
                    env,
                    range.cloned(),
                    "Type '{name}' does not accept template arguments"
                )
            })?;

            match symbol {
                MIRSymbol::Type(id) => Ok(id),
                MIRSymbol::Template { .. } => type_completion_error(
                    env,
                    range,
                    format!("Template type '{name}' requires explicit template arguments"),
                ),
                _ => type_completion_error(env, range, format!("Symbol '{name}' is not a type")),
            }
        }

        _ => type_completion_error(env, range, format!("Symbol '{name}' is not a type")),
    }
}

enum TypeLookup {
    Resolved(MIRTypeId),
    Untyped(CXSymbol),
}

fn make_aggregate_type<F>(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ty: &CXType,
    name: Option<CXIdent>,
    attributes: Option<&CXStructAttributes>,
    fields: &[CXField],
    kind_ctor: F,
) -> CXResult<MIRType>
where
    F: FnOnce(Vec<MIRField>) -> MIRTypeKind,
{
    let fields = fields
        .iter()
        .map(|field| complete_field(env, namespace, field))
        .collect::<CXResult<Vec<_>>>()?;
    ensure_aggregate_fields_complete(env, &fields)?;
    let move_attributes = resolve_aggregate_move_attributes(env, namespace, attributes)?;
    ensure_aggregate_move_restrictions(env, move_attributes, &fields)?;

    let (strong_identifier, lookup_identifier) = name
        .as_ref()
        .map(|name| {
            let lookup_identifier = QualifiedName::new(namespace.clone(), name.clone());
            let strong_identifier =
                base_mangle_standard(env.symbols.get_global_registry(), &lookup_identifier);
            (Some(strong_identifier), Some(lookup_identifier))
        })
        .unwrap_or((None, None));

    Ok(MIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        move_attributes,
        strong_identifier,
        lookup_identifier,
        debug_name: name,
        template_info: None,
        kind: kind_ctor(fields),
    })
}

fn resolve_aggregate_move_attributes(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    attributes: Option<&CXStructAttributes>,
) -> CXResult<MIRMoveAttributes> {
    let Some(attributes) = attributes else {
        return Ok(MIRMoveAttributes::default());
    };

    let mut move_attributes = MIRMoveAttributes {
        nocopy: attributes.nocopy || attributes.nodrop,
        nodrop: attributes.nodrop,
    };

    if let Some(param_name) = &attributes.copy_traits {
        let name = QualifiedName::new_raw(CXIdent::new(param_name.as_str()));
        let Some(symbol) = env.get_symbol(namespace, &name)? else {
            return type_completion_error(
                env,
                None,
                format!("copy_traits target '{}' is not a valid type", param_name),
            );
        };
        let Some(id) = symbol.as_type_id() else {
            return type_completion_error(
                env,
                None,
                format!("copy_traits target '{}' is not a type", param_name),
            );
        };
        let source_attributes = owned_move_attributes(env, env.symbols.resolve_type_id(id));

        move_attributes.nocopy |= source_attributes.nocopy;
        move_attributes.nodrop |= source_attributes.nodrop;
    }

    Ok(move_attributes)
}

fn complete_field(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    field: &CXField,
) -> CXResult<MIRField> {
    match field {
        CXField::Standard { name, _type } => {
            let id = complete_type_id(env, namespace, _type)?;
            if !env.symbols.contains(id) {
                return type_completion_error(
                    env,
                    _type.range(),
                    format!("Aggregate field '{}' has incomplete type", name),
                );
            }

            Ok(MIRField::standard(name.clone(), id))
        }

        CXField::Bitfield {
            name,
            integer_type,
            width,
        } => {
            let id = complete_type_id(env, namespace, integer_type)?;
            if !env.symbols.contains(id) {
                let name = name.as_deref().unwrap_or("<anonymous>");
                return type_completion_error(
                    env,
                    integer_type.range(),
                    format!("Aggregate field '{}' has incomplete type", name),
                );
            }

            Ok(MIRField::Bitfield {
                name: name.clone(),
                integer_type_id: id,
                width: *width,
            })
        }
    }
}

fn ensure_aggregate_fields_complete(env: &TypeEnvironment, fields: &[MIRField]) -> CXResult<()> {
    for field in fields {
        let id = field.ty();
        if !env.symbols.contains(id) {
            let name = field.name().unwrap_or("<anonymous>");
            return type_completion_error(
                env,
                None,
                format!("Aggregate field '{}' has incomplete type", name),
            );
        }
    }

    Ok(())
}

fn ensure_aggregate_move_restrictions(
    env: &TypeEnvironment,
    aggregate_attributes: MIRMoveAttributes,
    fields: &[MIRField],
) -> CXResult<()> {
    for field in fields {
        let field_type = env.symbols.resolve_type_id(field.ty());
        let field_attributes = owned_move_attributes(env, field_type);
        let name = field.name().unwrap_or("<anonymous>");

        if field_attributes.nocopy && !aggregate_attributes.nocopy {
            return type_completion_error(
                env,
                None,
                format!("Copyable aggregate cannot contain nocopy field '{}'", name),
            );
        }

        if field_attributes.nodrop && !aggregate_attributes.nodrop {
            return type_completion_error(
                env,
                None,
                format!(
                    "Aggregate containing nodrop field '{}' must also be nodrop",
                    name
                ),
            );
        }
    }

    Ok(())
}

fn owned_move_attributes(env: &TypeEnvironment, ty: &MIRType) -> MIRMoveAttributes {
    match &ty.kind {
        MIRTypeKind::Structured { .. }
        | MIRTypeKind::Union { .. }
        | MIRTypeKind::TaggedUnion { .. } => ty.move_attributes,
        MIRTypeKind::Array { inner_type, .. } => {
            owned_move_attributes(env, env.symbols.resolve_type_id(*inner_type))
        }
        _ => MIRMoveAttributes::default(),
    }
}

fn type_completion_error<T>(
    env: &TypeEnvironment,
    range: Option<&cx_tokens::TokenRange>,
    message: impl Into<String>,
) -> CXResult<T> {
    Err(crate::log::type_error_for_optional_range(
        env.source.tokens,
        env.source.compilation_unit.as_path(),
        range,
        message.into(),
        Vec::new()
    ))
}

fn completed_function_name(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> CXResult<String> {
    let name = match kind {
        CXFunctionKind::Standard(name) => base_mangle_standard(
            env.symbols.get_global_registry(),
            &QualifiedName::new(namespace.clone(), name.clone()),
        ),
        CXFunctionKind::MemberFunction {
            member_type, name, ..
        }
        | CXFunctionKind::StaticMemberFunction { member_type, name } => {
            let member_type = complete_type(env, namespace, &member_type.as_type())?;
            base_mangle_member(&env.symbols, name.as_str(), &member_type)
        }
    };

    Ok(name)
}
