use std::collections::HashSet;

use cx_ast::ast::{
    function::{CXFunctionKind, CXFunctionPrototype},
    modifiers::{CXTypeQualifiers, VisibilityMode},
    template::CXTemplateInput,
    types::{CXField, CXStructAttributes, CXType, CXTypeKind, PredeclarationType},
};
use cx_ast::symbols::CXSymbolKind;
use cx_log::{CXResult, CXUnspannedError};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use cx_mir::{
    mir::{
        data::{
            MIRFunctionPrototype, MIRFunctionSignature, MIRMoveAttributes, MIRParameter,
            MIRTemplateInput,
        },
        name_mangling::{mangle_namespace_symbol, mangle_qualified_name},
        r#type::{MIRField, MIRType, MIRTypeId, MIRTypeKind},
    },
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};

use crate::{
    EnvironmentNamespace,
    environment::{SymbolLookup, SymbolLookupKind, TypeEnvironment},
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
            let Some(completed) = env.symbols.try_resolve_type_id(id).cloned() else {
                return type_completion_error(
                    env,
                    ty.range(),
                    format!("Type '{}' is incomplete", ty),
                );
            };

            completed
        }

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = complete_type_id(env, namespace, inner)?;
            let size = typecheck_expr(env, namespace, size, None)
                .and_then(|v| v.standard_ready_coerce(env, size.token_range()))
                .and_then(|v| constexpr_evaluate(env, v))
                .and_then(|v| {
                    v.get_integer().ok_or_else(|| {
                        crate::typecheck_error!(
                            env,
                            size.token_range(),
                            "Array size must be an integer literal"
                        )
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
    let return_type = complete_type(env, namespace, &prototype.return_type)?;
    let mut params = complete_explicit_parameters(env, namespace, prototype)?;

    // If we have legacy int main(void)-like syntax, we treat it as main with no parameters
    if params.len() == 1 {
        let first_param = &params[0];

        if first_param._type.is_unit() && first_param.name.is_none() {
            params.clear();
        }
    }

    let lookup_identifier = function_lookup_identifier(namespace, &prototype.kind);
    let debug_name = lookup_identifier.name.clone();
    let symbol_name = completed_function_name(env, namespace, &prototype.kind)?;

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
                _type: complete_type(env, namespace, &param._type)?,
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
    range: Option<&TokenRange>,
) -> CXResult<MIRTypeId> {
    let Some(lookup) = env.lookup_symbol(namespace, name, range)? else {
        if predeclaration != PredeclarationType::None && name.namespace.is_root() {
            let id = env.symbols.reserve_type_id();
            env.symbols.insert_type_symbol(name.clone(), id);
            return Ok(id);
        }

        return type_completion_error(env, range, format!("Type not found: {name}"));
    };

    complete_identifier_type_lookup(env, namespace, name, lookup, template_input, range)
}

fn complete_identifier_type_lookup(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    lookup: SymbolLookup,
    template_input: &Option<CXTemplateInput>,
    range: Option<&cx_tokens::TokenRange>,
) -> CXResult<MIRTypeId> {
    let resolved_name = lookup.resolved_name;
    if let SymbolLookupKind::Resolved(symbol) = lookup.kind {
        return complete_resolved_type_lookup(env, namespace, name, symbol, template_input, range);
    }

    let SymbolLookupKind::Untyped(symbol) = lookup.kind else {
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

            let dummy_type =
                MIRType::from(MIRTypeKind::Undefined).with_strong_identifier(CXIdent::from(
                    mangle_qualified_name(env.symbols.get_global_registry(), &resolved_name),
                ));
            let prereserved_id = env.symbols.reserve_type_id();
            env.symbols
                .insert_type_symbol(resolved_name.clone(), prereserved_id);
            env.symbols.overwrite_type_id(prereserved_id, dummy_type);

            let completed = complete_type_value(env, &resolved_name.namespace, definition)?;

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
            complete_template_type_lookup(env, namespace, name, &mir_symbol, template_input, range)
        }

        CXSymbolKind::DuplicateDefinition(_) => {
            let mir_symbol = resolve_symbol(
                env,
                namespace,
                &resolved_name.namespace,
                &resolved_name.name,
                &symbol,
            )?;
            complete_resolved_type_lookup(env, namespace, name, mir_symbol, template_input, range)
        }

        _ => type_completion_error(env, range, format!("Symbol '{name}' is not a type")),
    }
}

fn complete_resolved_type_lookup(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    symbol: MIRSymbol,
    template_input: &Option<CXTemplateInput>,
    range: Option<&cx_tokens::TokenRange>,
) -> CXResult<MIRTypeId> {
    match symbol {
        MIRSymbol::Type(id) => {
            if template_input.is_some() {
                type_completion_error(
                    env,
                    range,
                    format!("Type '{name}' does not accept template arguments"),
                )
            } else {
                Ok(id)
            }
        }
        MIRSymbol::Template { .. } => {
            complete_template_type_lookup(env, namespace, name, &symbol, template_input, range)
        }
        _ => type_completion_error(env, range, format!("Symbol '{name}' is not a type")),
    }
}

fn complete_template_type_lookup(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    mir_symbol: &MIRSymbol,
    template_input: &Option<CXTemplateInput>,
    range: Option<&cx_tokens::TokenRange>,
) -> CXResult<MIRTypeId> {
    let Some(input) = template_input else {
        return type_completion_error(
            env,
            range,
            format!("Template type '{name}' requires explicit template arguments"),
        );
    };
    let input = complete_template_input(env, namespace, input)?;
    let Some(symbol) = apply_template(env, mir_symbol, input)? else {
        return type_completion_error(
            env,
            range,
            format!("Type '{name}' does not accept template arguments"),
        );
    };

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
                mangle_qualified_name(env.symbols.get_global_registry(), &lookup_identifier);
            (Some(strong_identifier), Some(lookup_identifier))
        })
        .unwrap_or((None, None));

    if let Some(strong_identifier) = &strong_identifier {
        ensure_aggregate_fields_not_recursive(env, &fields, strong_identifier)?;
    }

    Ok(MIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        move_attributes,
        strong_identifier,
        lookup_identifier,
        template_info: None,
        kind: kind_ctor(fields),
    })
}

fn ensure_aggregate_fields_not_recursive(
    env: &TypeEnvironment,
    fields: &[MIRField],
    aggregate_identifier: &str,
) -> CXResult<()> {
    for field in fields {
        let mut visited = HashSet::new();
        if type_contains_by_value(env, field.ty(), aggregate_identifier, &mut visited) {
            let name = field.name().unwrap_or("<anonymous>");
            return type_completion_error(
                env,
                None,
                format!("Aggregate field '{}' has recursive type", name),
            );
        }
    }

    Ok(())
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

fn type_contains_by_value(
    env: &TypeEnvironment,
    id: MIRTypeId,
    aggregate_identifier: &str,
    visited: &mut HashSet<MIRTypeId>,
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
        MIRTypeKind::Structured { fields }
        | MIRTypeKind::Union { variants: fields }
        | MIRTypeKind::TaggedUnion { variants: fields } => fields
            .iter()
            .any(|field| type_contains_by_value(env, field.ty(), aggregate_identifier, visited)),
        MIRTypeKind::Array { inner_type, .. } => {
            type_contains_by_value(env, *inner_type, aggregate_identifier, visited)
        }
        MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. }
        | MIRTypeKind::Function { .. } => false,
        MIRTypeKind::Unit
        | MIRTypeKind::Integer { .. }
        | MIRTypeKind::Float { .. }
        | MIRTypeKind::Opaque { .. }
        | MIRTypeKind::Undefined
        | MIRTypeKind::Str => false,
    }
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
        let Some(symbol) = env.get_symbol(namespace, &name, None)? else {
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
    if let Some(range) = range {
        return Err(crate::log::produce_typecheck_error(
            env.source.tokens,
            env.source.compilation_unit.as_path(),
            range,
            message.into(),
            Vec::new(),
        ));
    }

    CXUnspannedError::result("TYPE ERROR", message)
}

fn completed_function_name(
    env: &TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> CXResult<String> {
    let name = match kind {
        CXFunctionKind::Standard(name) => mangle_qualified_name(
            env.symbols.get_global_registry(),
            &QualifiedName::new(namespace.clone(), name.clone()),
        ),
        CXFunctionKind::AssociatedFunction {
            namespace: associated_namespace,
            name,
        } => mangle_namespace_symbol(&QualifiedName::new(
            namespace.child(associated_namespace.clone()),
            name.clone(),
        )),
    };

    Ok(name)
}
