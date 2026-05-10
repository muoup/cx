use std::sync::Arc;

use cx_ast::ast::{CXExpression, VisibilityMode};
use cx_ast::data::{
    CXField, CXStructAttributes, CXTemplateInput, CXType, CXTypeKind, PredeclarationType,
};
use cx_mir::mir::data::{MIRMoveAttributes, MIRTemplateInput, MIRType, MIRTypeId, MIRTypeKind};
use cx_mir::mir::program::MIRBaseMappings;
use cx_mir::mir::r#type::MIRField;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

use crate::environment::functions::completion::{complete_type, int_complete_fn_prototype};
use crate::environment::symbols::templates::instantiate_type_template;
use crate::log_typecheck_error;
use crate::type_checking::constexpr::constexpr_evaluate;
use crate::type_checking::typechecker::typecheck_expr;
use crate::{environment::TypeEnvironment, log::TypeError};

pub(crate) enum ResolvedBaseData<'a> {
    Local(&'a MIRBaseMappings),
    External(Arc<MIRBaseMappings>),
}

impl<'a> ResolvedBaseData<'a> {
    pub fn as_ref(&self) -> &MIRBaseMappings {
        match self {
            ResolvedBaseData::Local(base_data) => base_data,
            ResolvedBaseData::External(base_data) => base_data.as_ref(),
        }
    }
}

pub(crate) fn base_data_from_module<'a>(
    env: &mut TypeEnvironment,
    base_data: &'a MIRBaseMappings,
    external_module: Option<&String>,
) -> ResolvedBaseData<'a> {
    match external_module {
        Some(module) => {
            let arc = env
                .source
                .module_data
                .base_mappings
                .get(&env.resolve_compilation_unit(module));

            ResolvedBaseData::External(arc.clone())
        }
        None => ResolvedBaseData::Local(base_data),
    }
}

pub(crate) fn internal_complete_template_input(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    expr: &CXExpression,
    input: &CXTemplateInput,
) -> CXResult<MIRTemplateInput> {
    let _ty = input
        .params
        .iter()
        .map(|param| complete_type(env, base_data, external_module, expr, param))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args: _ty })
}

fn construct_type(ty: &CXType, kind: MIRTypeKind) -> MIRType {
    MIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        move_attributes: MIRMoveAttributes::default(),
        strong_identifier: None,
        debug_name: None,
        template_info: None,
        kind,
    }
}

fn complete_type_id(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    ty: &CXType,
) -> CXResult<MIRTypeId> {
    let completed = int_complete_type(env, base_data, expr, ty)?;
    Ok(env.intern_type(completed))
}

fn make_named_type(
    ty: &CXType,
    name: CXIdent,
    template_info: Option<Box<cx_mir::mir::data::TemplateInfo>>,
    attributes: MIRMoveAttributes,
    kind: MIRTypeKind,
) -> MIRType {
    MIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        move_attributes: attributes,
        strong_identifier: Some(name),
        debug_name: None,
        template_info,
        kind,
    }
}

fn named_predeclaration_type(
    env: &mut TypeEnvironment,
    ty: &CXType,
    name: &CXIdent,
    _predeclaration: PredeclarationType,
) -> MIRType {
    let id = env.get_or_create_named_type_id(name.as_str());
    env.symbols.context.register_identifier(name.clone(), id);

    let mir_type = MIRType {
        visibility: VisibilityMode::Private,
        specifiers: ty.specifiers,
        move_attributes: MIRMoveAttributes::default(),
        strong_identifier: Some(name.clone()),
        debug_name: None,
        template_info: None,
        kind: MIRTypeKind::Undefined,
    };

    env.add_type(name.to_string(), mir_type.clone());
    mir_type
}

fn ensure_complete_value_type(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    field_name: &str,
    field_type: &MIRType,
) -> CXResult<()> {
    match &field_type.kind {
        MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. }
        | MIRTypeKind::Integer { .. }
        | MIRTypeKind::Float { .. }
        | MIRTypeKind::Unit
        | MIRTypeKind::Opaque { .. } => Ok(()),
        MIRTypeKind::Function { .. } | MIRTypeKind::Str => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Field '{}' uses non-allocatable type {} by value",
            field_name,
            field_type.display_with(&env.symbols.context)
        ),
        MIRTypeKind::Undefined => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Field '{}' uses incomplete type {} by value",
            field_name,
            field_type.display_with(&env.symbols.context)
        ),
        MIRTypeKind::Array { inner_type, .. } => {
            let inner_type = env
                .symbols
                .context
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0))
                .clone();
            ensure_complete_value_type(env, expr, field_name, &inner_type)
        }
        MIRTypeKind::Structured { .. }
        | MIRTypeKind::Union { .. }
        | MIRTypeKind::TaggedUnion { .. } => {
            if let Some(name) = field_type.get_name()
                && let Some(id) = env.get_named_type_id(name.as_str())
                && !env.has_complete_named_type_definition(id)
            {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Field '{}' uses incomplete type {} by value",
                    field_name,
                    field_type.display_with(&env.symbols.context)
                );
            }

            Ok(())
        }
    }
}

fn complete_field(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    field: &CXField,
) -> CXResult<MIRField> {
    match field {
        CXField::Standard { name, _type } => {
            let field_type_id = complete_type_id(env, base_data, expr, _type)?;
            let resolved_field_type = env
                .symbols
                .context
                .get(field_type_id)
                .unwrap_or_else(|| panic!("Unknown type id {}", field_type_id.0))
                .clone();
            ensure_complete_value_type(env, expr, name, &resolved_field_type)?;
            Ok(MIRField::standard(name.clone(), field_type_id))
        }
        CXField::Bitfield {
            name,
            integer_type,
            width,
        } => {
            if name.is_some() && *width == 0 {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Named bitfield cannot have width 0"
                );
            }

            let integer_type_id = complete_type_id(env, base_data, expr, integer_type)?;
            let resolved_integer_type = env
                .symbols
                .context
                .get(integer_type_id)
                .unwrap_or_else(|| panic!("Unknown type id {}", integer_type_id.0))
                .clone();
            let MIRTypeKind::Integer { _type, .. } = resolved_integer_type.kind else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Bitfield '{}' must have an integer type, found {}",
                    name.as_deref().unwrap_or("<anonymous>"),
                    resolved_integer_type.display_with(&env.symbols.context)
                );
            };
            let max_width = _type.bytes() * 8;
            if *width > max_width {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Bitfield '{}' has width {}, but its storage type only has {} bits",
                    name.as_deref().unwrap_or("<anonymous>"),
                    width,
                    max_width
                );
            }

            Ok(MIRField::Bitfield {
                name: name.clone(),
                integer_type_id,
                width: *width,
            })
        }
    }
}

fn complete_named_aggregate<F>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    ty: &CXType,
    name: CXIdent,
    template_info: Option<Box<cx_mir::mir::data::TemplateInfo>>,
    attributes: MIRMoveAttributes,
    raw_fields: &[CXField],
    aggregate_kind: &str,
    kind_ctor: F,
) -> CXResult<MIRType>
where
    F: Fn(Vec<MIRField>) -> MIRTypeKind,
{
    let type_id = env.get_or_create_named_type_id(name.as_str());
    env.symbols
        .context
        .register_identifier(name.clone(), type_id);

    let provisional = make_named_type(
        ty,
        name.clone(),
        template_info.clone(),
        attributes,
        kind_ctor(Vec::new()),
    );
    env.add_type(name.to_string(), provisional.clone());

    if env.has_complete_named_type_definition(type_id) {
        return Ok(env
            .get_named_type_definition(type_id)
            .cloned()
            .unwrap_or(provisional));
    }

    if env.is_type_defining(type_id) {
        return Ok(provisional);
    }

    env.mark_type_defining(type_id);

    let result = (|| {
        let fields = raw_fields
            .iter()
            .map(|field| complete_field(env, base_data, expr, field))
            .collect::<CXResult<Vec<_>>>()?;

        validate_linear_hierarchy(env, aggregate_kind, &fields, &attributes)?;

        Ok(fields)
    })();

    match result {
        Ok(fields) => {
            let completed = make_named_type(
                ty,
                name.clone(),
                template_info,
                attributes,
                kind_ctor(fields),
            );
            env.finish_type_definition(type_id, completed.clone());
            env.add_type(name.to_string(), completed.clone());
            Ok(completed)
        }
        Err(err) => {
            env.abort_type_definition(type_id);
            Err(err)
        }
    }
}

fn ensure_named_identifier_completed(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    ty: &CXType,
    name: &CXIdent,
) -> CXResult<Option<MIRType>> {
    let Some(inner) = base_data.type_data.get_standard(&name.as_string()) else {
        return Ok(None);
    };

    let completed = match &inner.resource.kind {
        CXTypeKind::Structured {
            name: Some(struct_name),
            attributes,
            fields,
        } => {
            let (nocopy, nodrop) = resolve_copy_traits(env, attributes);
            complete_named_aggregate(
                env,
                base_data,
                expr,
                ty,
                struct_name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                fields.as_slice(),
                "struct",
                |fields| MIRTypeKind::Structured { fields },
            )?
        }
        CXTypeKind::Union {
            name: Some(union_name),
            fields,
        } => complete_named_aggregate(
            env,
            base_data,
            expr,
            ty,
            union_name.clone(),
            None,
            MIRMoveAttributes::default(),
            fields.as_slice(),
            "union",
            |variants| MIRTypeKind::Union { variants },
        )?,
        CXTypeKind::TaggedUnion {
            name: union_name,
            attributes,
            variants,
        } => {
            let (nocopy, nodrop) = resolve_copy_traits(env, attributes);
            complete_named_aggregate(
                env,
                base_data,
                expr,
                ty,
                union_name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                variants.as_slice(),
                "enum union",
                |variants| MIRTypeKind::TaggedUnion { variants },
            )?
        }
        CXTypeKind::Identifier {
            name: identifier_name,
            predeclaration,
        } if identifier_name == name && *predeclaration != PredeclarationType::None => {
            named_predeclaration_type(env, ty, identifier_name, *predeclaration)
        }
        _ => complete_type(
            env,
            base_data,
            inner.external_module.as_ref(),
            expr,
            &inner.resource,
        )?
        .with_specifier(ty.specifiers),
    };

    Ok(Some(completed))
}

pub(crate) fn int_complete_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    ty: &CXType,
) -> CXResult<MIRType> {
    match &ty.kind {
        CXTypeKind::Identifier {
            name,
            predeclaration,
        } => {
            if let Some(existing) = env.get_realized_type(&name.as_string()) {
                if let Some(id) = env.get_named_type_id(name.as_str())
                    && !env.has_complete_named_type_definition(id)
                    && let Some(completed) =
                        ensure_named_identifier_completed(env, base_data, expr, ty, name)?
                {
                    return Ok(completed.with_specifier(ty.specifiers));
                }

                return Ok(existing.with_specifier(ty.specifiers));
            }

            if let Some(completed) =
                ensure_named_identifier_completed(env, base_data, expr, ty, name)?
            {
                return Ok(completed.with_specifier(ty.specifiers));
            }

            if base_data
                .type_data
                .get_template(&name.as_string())
                .is_some()
            {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Template type '{}' requires explicit template arguments",
                    name,
                );
            }

            if *predeclaration != PredeclarationType::None {
                return Ok(named_predeclaration_type(env, ty, name, *predeclaration));
            }

            let range = crate::log::identifier_range_for_name(
                env.source.tokens,
                expr.token_range(),
                name.as_str(),
            );
            log_typecheck_error!(env, Some(range), "Type not found: {name}")
        }

        CXTypeKind::TemplatedIdentifier { name, input, .. } => {
            instantiate_type_template(env, base_data, input, name.as_str())
                .map(|completed| completed.with_specifier(ty.specifiers))
        }

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type_id = complete_type_id(env, base_data, expr, inner)?;
            let inner_type = env
                .symbols
                .context
                .get(inner_type_id)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type_id.0))
                .clone();

            let Some(size) = typecheck_expr(env, base_data, size.as_ref(), None)
                .and_then(|v| constexpr_evaluate(env, v.into_expression()))?
                .get_integer()
            else {
                return log_typecheck_error!(
                    env,
                    Some(size.token_range()),
                    "Invalid size expression, expected integer result"
                );
            };

            ensure_complete_value_type(env, expr, "<array element>", &inner_type)?;

            Ok(construct_type(
                ty,
                MIRTypeKind::Array {
                    length: size as usize,
                    inner_type: inner_type_id,
                },
            ))
        }

        CXTypeKind::ImplicitSizedArray(inner) => {
            let inner_type_id = complete_type_id(env, base_data, expr, inner)?;
            Ok(construct_type(
                ty,
                MIRTypeKind::PointerTo {
                    inner_type: inner_type_id,
                },
            ))
        }

        CXTypeKind::MemoryReference { inner_type } => {
            let inner_type_id = complete_type_id(env, base_data, expr, inner_type)?;
            Ok(construct_type(
                ty,
                MIRTypeKind::MemoryReference {
                    inner_type: inner_type_id,
                    bitfield: None,
                },
            ))
        }

        CXTypeKind::PointerTo { inner_type, .. } => {
            let inner_type_id = complete_type_id(env, base_data, expr, inner_type)?;
            Ok(construct_type(
                ty,
                MIRTypeKind::PointerTo {
                    inner_type: inner_type_id,
                },
            ))
        }

        CXTypeKind::FunctionPointer { prototype } => {
            let prototype = int_complete_fn_prototype(env, base_data, prototype)?;
            Ok(construct_type(
                ty,
                MIRTypeKind::Function {
                    signature: Box::new(prototype.signature()),
                },
            ))
        }

        CXTypeKind::Structured {
            name: Some(name),
            attributes,
            fields,
        } => {
            let (nocopy, nodrop) = resolve_copy_traits(env, attributes);
            complete_named_aggregate(
                env,
                base_data,
                expr,
                ty,
                name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                fields.as_slice(),
                "struct",
                |fields| MIRTypeKind::Structured { fields },
            )
        }

        CXTypeKind::Structured {
            name: None,
            attributes,
            fields,
        } => {
            let completed_fields = fields
                .iter()
                .map(|field| complete_field(env, base_data, expr, field))
                .collect::<CXResult<Vec<_>>>()?;

            let (nocopy, nodrop) = resolve_copy_traits(env, attributes);
            validate_linear_hierarchy(
                env,
                "struct",
                &completed_fields,
                &MIRMoveAttributes { nocopy, nodrop },
            )?;

            Ok(MIRType {
                visibility: VisibilityMode::Private,
                specifiers: ty.specifiers,
                move_attributes: MIRMoveAttributes { nocopy, nodrop },
                strong_identifier: None,
                debug_name: None,
                template_info: None,
                kind: MIRTypeKind::Structured {
                    fields: completed_fields,
                },
            })
        }

        CXTypeKind::Union {
            name: Some(name),
            fields,
        } => complete_named_aggregate(
            env,
            base_data,
            expr,
            ty,
            name.clone(),
            None,
            MIRMoveAttributes::default(),
            fields.as_slice(),
            "union",
            |variants| MIRTypeKind::Union { variants },
        ),

        CXTypeKind::Union { name: None, fields } => {
            let completed_fields = fields
                .iter()
                .map(|field| complete_field(env, base_data, expr, field))
                .collect::<CXResult<Vec<_>>>()?;

            Ok(MIRType {
                visibility: VisibilityMode::Private,
                specifiers: ty.specifiers,
                move_attributes: MIRMoveAttributes::default(),
                strong_identifier: None,
                debug_name: None,
                template_info: None,
                kind: MIRTypeKind::Union {
                    variants: completed_fields,
                },
            })
        }

        CXTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
        } => {
            let (nocopy, nodrop) = resolve_copy_traits(env, attributes);
            complete_named_aggregate(
                env,
                base_data,
                expr,
                ty,
                name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                variants.as_slice(),
                "enum union",
                |variants| MIRTypeKind::TaggedUnion { variants },
            )
        }
    }
}

fn resolve_copy_traits(env: &TypeEnvironment, attributes: &CXStructAttributes) -> (bool, bool) {
    let mut nocopy = attributes.nocopy || attributes.nodrop;
    let mut nodrop = attributes.nodrop;

    if let Some(param_name) = &attributes.copy_traits
        && let Some(resolved_type) = env.get_realized_type(param_name)
        && let Some(src_attrs) = resolved_type.struct_attributes()
    {
        nocopy = nocopy || src_attrs.nocopy;
        nodrop = nodrop || src_attrs.nodrop;
    }

    (nocopy, nodrop)
}

fn validate_linear_hierarchy(
    env: &mut TypeEnvironment,
    aggregate_kind: &str,
    members: &[MIRField],
    attributes: &MIRMoveAttributes,
) -> CXResult<()> {
    let aggregate_is_nocopy = attributes.nocopy || attributes.nodrop;

    for member in members {
        let Some((member_name, member_id)) = member.standard_parts() else {
            continue;
        };
        let member_type = env
            .symbols
            .context
            .get(member_id)
            .unwrap_or_else(|| panic!("Unknown type id {}", member_id.0));
        let Some(member_attributes) = member_type.struct_attributes() else {
            continue;
        };

        if member_attributes.nodrop && !attributes.nodrop {
            return Err(Box::new(TypeError {
                compilation_unit: env.source.compilation_unit.as_path().to_owned(),
                token_start: 0,
                token_end: 0,
                byte_start: 0,
                byte_end: 1,
                message: format!(
                    "{} must be declared @nodrop because member '{}' is of a @nodrop type",
                    aggregate_kind, member_name
                ),
                notes: Vec::new(),
            }));
        }

        if member_attributes.nocopy && !aggregate_is_nocopy {
            return Err(Box::new(TypeError {
                compilation_unit: env.source.compilation_unit.as_path().to_owned(),
                token_start: 0,
                token_end: 0,
                byte_start: 0,
                byte_end: 1,
                message: format!(
                    "{} must be declared @nocopy because member '{}' is of a @nocopy type",
                    aggregate_kind, member_name
                ),
                notes: Vec::new(),
            }));
        }
    }

    Ok(())
}
