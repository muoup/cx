use std::sync::Arc;

use cx_parsing_data::data::{CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker_data::ast::TCBaseMappings;
use cx_typechecker_data::cx_types::{CXTemplateInput, CXType, CXTypeKind};
use cx_util::log_error;

use crate::environment::TCEnvironment;
use crate::type_completion::complete_type;
use crate::type_completion::prototypes::_complete_fn_prototype;
use crate::type_completion::templates::instantiate_type_template;

pub(crate) fn base_data_from_module<'a>(
    env: &mut TCEnvironment,
    base_data: &'a TCBaseMappings,
    external_module: Option<&String>,
) -> (Option<Arc<TCBaseMappings>>, &'a TCBaseMappings) {
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
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    external_module: Option<&String>,
    input: &CXNaiveTemplateInput,
) -> Option<CXTemplateInput> {
    let _ty = input
        .params
        .iter()
        .map(|param| {
            complete_type(env, base_data, external_module, param).unwrap_or_else(|| {
                panic!("Failed to precontextualize template input type: {param}")
            })
        })
        .collect::<Vec<_>>();

    Some(CXTemplateInput { args: _ty })
}

pub(crate) fn _complete_type(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    ty: &CXNaiveType,
) -> Option<CXType> {
    let mut recurse_ty = |ty: &CXNaiveType| {
        Some(
            _complete_type(env, base_data, ty)
                .unwrap_or_else(|| panic!("Failed to precontextualize type: {ty}")),
        )
    };

    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(existing) = env.realized_types.get(&name.as_string()) {
                return Some(existing.clone());
            };

            if let Some(inner) = base_data.type_data.get_standard(&name.as_string()) {
                let ty = complete_type(
                    env,
                    base_data,
                    inner.external_module.as_ref(),
                    &inner.resource,
                );
                
                return ty;
            };

            log_error!("Type not found: {name}");
        }

        CXNaiveTypeKind::TemplatedIdentifier { name, input, .. } => {
            instantiate_type_template(env, base_data, input, name.as_str())
        }

        CXNaiveTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = recurse_ty(inner)?;

            Some(CXType::from(CXTypeKind::Array {
                inner_type: Box::new(inner_type),
                size: *size,
            }))
        }

        CXNaiveTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = recurse_ty(inner)?;

            Some(CXType::from(CXTypeKind::PointerTo {
                inner_type: Box::new(inner_type),
                sizeless_array: true,
                weak: false,
                nullable: true,
            }))
        }

        CXNaiveTypeKind::StrongPointer { inner, is_array } => {
            let inner_type = recurse_ty(inner)?;

            Some(CXType::from(CXTypeKind::StrongPointer {
                inner_type: Box::new(inner_type),
                is_array: *is_array,
            }))
        }

        CXNaiveTypeKind::PointerTo { inner_type, weak } => {
            let inner_type = recurse_ty(inner_type.as_ref())?;

            Some(CXType::from(CXTypeKind::PointerTo {
                inner_type: Box::new(inner_type),
                weak: *weak,
                sizeless_array: false,
                nullable: true,
            }))
        }

        CXNaiveTypeKind::FunctionPointer { prototype } => {
            let prototype = _complete_fn_prototype(env, base_data, prototype).unwrap();

            Some(CXType::from(CXTypeKind::Function {
                prototype: Box::new(prototype),
            }))
        }

        CXNaiveTypeKind::Structured { name, fields, .. } => {
            let fields = fields
                .iter()
                .map(|(name, field_type)| {
                    let field_type = recurse_ty(field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(CXType::from(CXTypeKind::Structured {
                name: name.clone(),
                base_identifier: name.clone(),
                fields,

                move_semantics: true,
                copyable: true,
            }))
        }

        CXNaiveTypeKind::Union { name, fields, .. } => {
            let fields = fields
                .iter()
                .map(|(name, field_type)| {
                    let field_type = recurse_ty(field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(CXType::from(CXTypeKind::Union {
                name: name.clone(),
                variants: fields,
            }))
        }

        CXNaiveTypeKind::TaggedUnion { name, variants } => {
            let variants = variants
                .iter()
                .map(|(name, variant_type)| {
                    let variant_type = recurse_ty(variant_type)?;
                    Some((name.clone(), variant_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(CXType::from(CXTypeKind::TaggedUnion {
                name: name.clone(),
                variants,
            }))
        }
    }
}
