use std::sync::Arc;

use cx_ast::ast::{CXExpr, VisibilityMode};
use cx_ast::data::{CXStructAttributes, CXTemplateInput, CXType, CXTypeKind, PredeclarationType};
use cx_mir::mir::program::MIRBaseMappings;
use cx_mir::mir::data::{
    MIRAggregateContents, MIRAggregateKind, MIRNamedTypeDefinition, MIRMoveAttributes,
    MIRTemplateInput, MIRType, MIRTypeId, MIRTypeKind,
};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

use crate::log_typecheck_error;
use crate::type_completion::complete_type;
use crate::type_completion::prototypes::_complete_fn_prototype;
use crate::type_completion::templates::instantiate_type_template;
use crate::{environment::TypeEnvironment, log::TypeError};

pub(crate) fn base_data_from_module<'a>(
    env: &mut TypeEnvironment,
    base_data: &'a MIRBaseMappings,
    external_module: Option<&String>,
) -> (Option<Arc<MIRBaseMappings>>, &'a MIRBaseMappings) {
    match external_module {
        Some(module) => {
            let arc = env
                .module_data
                .base_mappings
                .get(&env.resolve_compilation_unit(module));

            (Some(arc.clone()), unsafe {
                std::mem::transmute(arc.as_ref())
            })
        }
        None => (None, base_data),
    }
}

pub(crate) fn _complete_template_input(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    expr: &CXExpr,
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
        specifiers: ty.specifiers,
        visibility: VisibilityMode::Private,
        kind,
    }
}

fn make_named_type(
    ty: &CXType,
    kind: MIRAggregateKind,
    type_id: MIRTypeId,
    name: CXIdent,
    template_info: Option<Box<cx_mir::mir::data::TemplateInstantiationInformation>>,
    attributes: MIRMoveAttributes,
) -> MIRType {
    let base = match kind {
        MIRAggregateKind::Struct => MIRType::named_struct(name, type_id, template_info, attributes),
        MIRAggregateKind::Union => MIRType::named_union(name, type_id),
        MIRAggregateKind::TaggedUnion => {
            MIRType::named_tagged_union(name, type_id, template_info, attributes)
        }
    };

    base.with_specifier(ty.specifiers)
}

fn named_predeclaration_type(
    env: &mut TypeEnvironment,
    ty: &CXType,
    name: &CXIdent,
    predeclaration: PredeclarationType,
) -> MIRType {
    let id = env.get_or_create_named_type_id(name.as_str());

    let mir_type = match predeclaration {
        PredeclarationType::Struct => make_named_type(
            ty,
            MIRAggregateKind::Struct,
            id,
            name.clone(),
            None,
            MIRMoveAttributes::default(),
        ),
        PredeclarationType::Union => make_named_type(
            ty,
            MIRAggregateKind::Union,
            id,
            name.clone(),
            None,
            MIRMoveAttributes::default(),
        ),
        PredeclarationType::Enum | PredeclarationType::None => {
            construct_type(ty, MIRTypeKind::Undefined { name: name.clone() })
        }
    };

    env.add_type(name.to_string(), mir_type.clone());
    mir_type
}

fn ensure_complete_value_type(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    field_name: &str,
    field_type: &MIRType,
) -> CXResult<()> {
    match &field_type.kind {
        MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. }
        | MIRTypeKind::Integer { .. }
        | MIRTypeKind::Float { .. }
        | MIRTypeKind::Unit
        | MIRTypeKind::Opaque { .. }
        | MIRTypeKind::Function { .. }
        | MIRTypeKind::Str => Ok(()),

        MIRTypeKind::Undefined { name } => log_typecheck_error!(
            env,
            expr.token_range(),
            "Field '{}' uses incomplete type {} by value",
            field_name,
            name
        ),

        MIRTypeKind::Array { inner_type, .. } => {
            ensure_complete_value_type(env, expr, field_name, inner_type)
        }

        MIRTypeKind::Structured { .. }
        | MIRTypeKind::Union { .. }
        | MIRTypeKind::TaggedUnion { .. } => {
            if field_type.is_named_aggregate_complete(&env.generated_types) {
                Ok(())
            } else {
                log_typecheck_error!(
                    env,
                    expr.token_range(),
                    "Field '{}' uses incomplete type {} by value",
                    field_name,
                    field_type
                )
            }
        }
    }
}

fn complete_named_aggregate(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    ty: &CXType,
    kind: MIRAggregateKind,
    name: CXIdent,
    template_info: Option<Box<cx_mir::mir::data::TemplateInstantiationInformation>>,
    attributes: MIRMoveAttributes,
    raw_fields: &[(String, CXType)],
) -> CXResult<MIRType> {
    let type_id = env.get_or_create_named_type_id(name.as_str());
    let nominal_type = make_named_type(
        ty,
        kind,
        type_id,
        name.clone(),
        template_info.clone(),
        attributes,
    );

    env.add_type(name.to_string(), nominal_type.clone());

    if env.has_complete_named_type_definition(type_id) || env.is_type_defining(type_id) {
        return Ok(nominal_type);
    }

    env.mark_type_defining(type_id);

    let result = (|| {
        let fields = raw_fields
            .iter()
            .map(|(field_name, field_type)| {
                let field_type = _complete_type(env, base_data, expr, field_type)?;
                ensure_complete_value_type(env, expr, field_name, &field_type)?;
                Ok((field_name.clone(), field_type))
            })
            .collect::<CXResult<Vec<_>>>()?;

        if matches!(
            kind,
            MIRAggregateKind::Struct | MIRAggregateKind::TaggedUnion
        ) {
            let resolved_attributes = CXStructAttributes {
                nocopy: attributes.nocopy,
                nodrop: attributes.nodrop,
                copy_traits: None,
            };

            validate_linear_hierarchy(
                env,
                if kind == MIRAggregateKind::Struct {
                    "struct"
                } else {
                    "enum union"
                },
                &fields,
                &resolved_attributes,
            )?;
        }

        Ok(fields)
    })();

    match result {
        Ok(fields) => {
            env.finish_type_definition(
                type_id,
                MIRNamedTypeDefinition {
                    kind,
                    name,
                    template_info,
                    attributes,
                    fields,
                },
            );
            Ok(nominal_type)
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
    expr: &CXExpr,
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
                MIRAggregateKind::Struct,
                struct_name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                fields.as_slice(),
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
            MIRAggregateKind::Union,
            union_name.clone(),
            None,
            MIRMoveAttributes::default(),
            fields.as_slice(),
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
                MIRAggregateKind::TaggedUnion,
                union_name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                variants.as_slice(),
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

pub(crate) fn _complete_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    ty: &CXType,
) -> CXResult<MIRType> {
    match &ty.kind {
        CXTypeKind::Identifier {
            name,
            predeclaration,
        } => {
            if let Some(existing) = env.get_realized_type(&name.as_string()) {
                if existing.named_type_id().is_some()
                    && !existing.is_named_aggregate_complete(&env.generated_types)
                {
                    if let Some(completed) =
                        ensure_named_identifier_completed(env, base_data, expr, ty, name)?
                    {
                        return Ok(completed.with_specifier(ty.specifiers));
                    }
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
                    &expr.range,
                    "Template deduction is not yet implemented!"
                );
            }

            if *predeclaration != PredeclarationType::None {
                return Ok(named_predeclaration_type(env, ty, name, *predeclaration));
            }

            return log_typecheck_error!(env, &expr.range, "Type not found: {name}");
        }

        CXTypeKind::TemplatedIdentifier { name, input, .. } => {
            instantiate_type_template(env, base_data, input, name.as_str())
                .map(|ty| ty.with_specifier(ty.specifiers))
        }

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = _complete_type(env, base_data, expr, inner)?;
            ensure_complete_value_type(env, expr, "<array element>", &inner_type)?;

            Ok(construct_type(
                ty,
                MIRTypeKind::Array {
                    inner_type: Box::new(inner_type),
                    size: *size,
                },
            ))
        }

        CXTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = _complete_type(env, base_data, expr, inner)?;

            Ok(construct_type(
                ty,
                MIRTypeKind::PointerTo {
                    inner_type: Box::new(inner_type),
                    weak: false,
                    nullable: true,
                },
            ))
        }

        CXTypeKind::MemoryReference { inner_type } => {
            let inner_type = _complete_type(env, base_data, expr, inner_type.as_ref())?;

            Ok(construct_type(
                ty,
                MIRTypeKind::MemoryReference {
                    inner_type: Box::new(inner_type),
                },
            ))
        }

        CXTypeKind::PointerTo { inner_type, weak } => {
            let inner_type = _complete_type(env, base_data, expr, inner_type.as_ref())?;

            Ok(construct_type(
                ty,
                MIRTypeKind::PointerTo {
                    inner_type: Box::new(inner_type),
                    weak: *weak,
                    nullable: true,
                },
            ))
        }

        CXTypeKind::FunctionPointer { prototype } => {
            let prototype = _complete_fn_prototype(env, base_data, prototype)?;

            Ok(construct_type(
                ty,
                MIRTypeKind::Function {
                    prototype: Box::new(prototype),
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
                MIRAggregateKind::Struct,
                name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                fields.as_slice(),
            )
        }

        CXTypeKind::Structured {
            name: None,
            attributes,
            fields,
        } => {
            let mut completed_fields = Vec::with_capacity(fields.len());
            for (field_name, field_type) in fields {
                let field_type = _complete_type(env, base_data, expr, field_type)?;
                ensure_complete_value_type(env, expr, field_name, &field_type)?;
                completed_fields.push((field_name.clone(), field_type));
            }

            let (nocopy, nodrop) = resolve_copy_traits(env, attributes);
            let resolved_attributes = CXStructAttributes {
                nocopy,
                nodrop,
                copy_traits: None,
            };

            validate_linear_hierarchy(env, "struct", &completed_fields, &resolved_attributes)?;

            Ok(construct_type(
                ty,
                MIRTypeKind::Structured {
                    name: None,
                    template_info: None,
                    attributes: MIRMoveAttributes { nocopy, nodrop },
                    fields: MIRAggregateContents::Anonymous(completed_fields),
                },
            ))
        }

        CXTypeKind::Union {
            name: Some(name),
            fields,
        } => complete_named_aggregate(
            env,
            base_data,
            expr,
            ty,
            MIRAggregateKind::Union,
            name.clone(),
            None,
            MIRMoveAttributes::default(),
            fields.as_slice(),
        ),

        CXTypeKind::Union { name: None, fields } => {
            let mut completed_fields = Vec::with_capacity(fields.len());
            for (field_name, field_type) in fields {
                let field_type = _complete_type(env, base_data, expr, field_type)?;
                ensure_complete_value_type(env, expr, field_name, &field_type)?;
                completed_fields.push((field_name.clone(), field_type));
            }

            Ok(construct_type(
                ty,
                MIRTypeKind::Union {
                    name: None,
                    variants: MIRAggregateContents::Anonymous(completed_fields),
                },
            ))
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
                MIRAggregateKind::TaggedUnion,
                name.clone(),
                None,
                MIRMoveAttributes { nocopy, nodrop },
                variants.as_slice(),
            )
        }
    }
}

fn resolve_copy_traits(env: &TypeEnvironment, attributes: &CXStructAttributes) -> (bool, bool) {
    let mut nocopy = attributes.nocopy || attributes.nodrop;
    let mut nodrop = attributes.nodrop;

    if let Some(param_name) = &attributes.copy_traits {
        if let Some(resolved_type) = env.get_realized_type(param_name) {
            if let Some(src_attrs) = resolved_type.struct_attributes() {
                nocopy = nocopy || src_attrs.nocopy;
                nodrop = nodrop || src_attrs.nodrop;
            }
        }
    }

    (nocopy, nodrop)
}

fn validate_linear_hierarchy(
    env: &mut TypeEnvironment,
    aggregate_kind: &str,
    members: &[(String, MIRType)],
    attributes: &CXStructAttributes,
) -> CXResult<()> {
    let aggregate_is_nocopy = attributes.nocopy || attributes.nodrop;

    for (member_name, member_type) in members {
        let Some(member_attributes) = member_type.struct_attributes() else {
            continue;
        };

        if member_attributes.nodrop && !attributes.nodrop {
            return Err(Box::new(TypeError {
                compilation_unit: env.compilation_unit.as_path().to_owned(),
                token_start: 0,
                token_end: 0,
                message: format!(
                    "{} must be declared @nodrop because member '{}' has type {}",
                    aggregate_kind, member_name, member_type
                ),
                notes: Vec::new(),
            }));
        }

        if member_attributes.nocopy && !aggregate_is_nocopy {
            return Err(Box::new(TypeError {
                compilation_unit: env.compilation_unit.as_path().to_owned(),
                token_start: 0,
                token_end: 0,
                message: format!(
                    "{} must be declared @nocopy because member '{}' has type {}",
                    aggregate_kind, member_name, member_type
                ),
                notes: Vec::new(),
            }));
        }
    }

    Ok(())
}
