use std::sync::Arc;

use cx_parsing_data::ast::VisibilityMode;
use cx_parsing_data::data::{CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker_data::mir::program::MIRBaseMappings;
use cx_typechecker_data::mir::types::{CXTemplateInput, MIRType, MIRTypeKind};
use cx_util::{CXResult, log_error};

use crate::environment::TypeEnvironment;
use crate::type_completion::complete_type;
use crate::type_completion::prototypes::_complete_fn_prototype;
use crate::type_completion::templates::instantiate_type_template;

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
                .get(&CompilationUnit::from_str(module));

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
    input: &CXNaiveTemplateInput,
) -> CXResult<CXTemplateInput> {
    let _ty = input
        .params
        .iter()
        .map(|param| complete_type(env, base_data, external_module, param))
        .collect::<CXResult<Vec<_>>>()?;

    Ok(CXTemplateInput { args: _ty })
}

pub(crate) fn _complete_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ty: &CXNaiveType,
) -> CXResult<MIRType> {
    let mut recurse_ty = |ty: &CXNaiveType| _complete_type(env, base_data, ty);
    let construct_type = |kind: MIRTypeKind| -> CXResult<MIRType> {
        Ok(MIRType {
            specifiers: ty.specifiers,
            visibility: VisibilityMode::Private,
            kind,
        })
    };

    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(existing) = env.get_realized_type(&name.as_string()) {
                return Ok(existing.clone().with_specifier(ty.specifiers));
            };

            if let Some(inner) = base_data.type_data.get_standard(&name.as_string()) {
                let ty = complete_type(
                    env,
                    base_data,
                    inner.external_module.as_ref(),
                    &inner.resource,
                )?.with_specifier(ty.specifiers);                

                return Ok(ty);
            };

            log_error!("Type not found: {name}");
        }

        CXNaiveTypeKind::TemplatedIdentifier { name, input, .. } => {
            Ok(instantiate_type_template(env, base_data, input, name.as_str())?
                .with_specifier(ty.specifiers))
        }

        CXNaiveTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = recurse_ty(inner)?;

            construct_type(MIRTypeKind::Array {
                inner_type: Box::new(inner_type),
                size: *size,
            })
        }

        CXNaiveTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = recurse_ty(inner)?;

            construct_type(MIRTypeKind::PointerTo {
                inner_type: Box::new(inner_type),
                sizeless_array: true,
                weak: false,
                nullable: true,
            })
        }

        CXNaiveTypeKind::MemoryReference { inner_type } => {
            let inner_type = recurse_ty(inner_type.as_ref())?;

            construct_type(MIRTypeKind::MemoryReference(Box::new(inner_type)))
        }

        CXNaiveTypeKind::PointerTo { inner_type, weak } => {
            let inner_type = recurse_ty(inner_type.as_ref())?;

            construct_type(MIRTypeKind::PointerTo {
                inner_type: Box::new(inner_type),
                weak: *weak,
                sizeless_array: false,
                nullable: true,
            })
        }

        CXNaiveTypeKind::FunctionPointer { prototype } => {
            let prototype = _complete_fn_prototype(env, base_data, prototype)?;

            construct_type(MIRTypeKind::Function {
                prototype: Box::new(prototype),
            })
        }

        CXNaiveTypeKind::Structured { name, fields, .. } => {
            let fields = fields
                .iter()
                .map(|(name, field_type)| {
                    let field_type = recurse_ty(field_type)?;
                    Ok((name.clone(), field_type))
                })
                .collect::<CXResult<Vec<_>>>()?;

            let ty = construct_type(MIRTypeKind::Structured {
                name: name.clone(),
                template_info: None,
                fields,
            })?;

            if let Some(name) = name {
                env.add_type(base_data, name.to_string(), ty.clone());
            }

            Ok(ty)
        }

        CXNaiveTypeKind::Union { name, fields, .. } => {
            let fields = fields
                .iter()
                .map(|(name, field_type)| {
                    let field_type = recurse_ty(field_type)?;
                    Ok((name.clone(), field_type))
                })
                .collect::<CXResult<Vec<_>>>()?;

            construct_type(MIRTypeKind::Union {
                name: name.clone(),
                variants: fields,
            })
        }

        CXNaiveTypeKind::TaggedUnion { name, variants } => {
            let variants = variants
                .iter()
                .map(|(name, variant_type)| {
                    let variant_type = recurse_ty(variant_type)?;
                    Ok((name.clone(), variant_type))
                })
                .collect::<CXResult<Vec<_>>>()?;

            Ok(MIRType::from(MIRTypeKind::TaggedUnion {
                name: name.clone(),
                variants,
            }))
        }
    }
}
