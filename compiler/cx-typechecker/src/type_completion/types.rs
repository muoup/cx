use cx_parsing_data::preparse::naive_types::{CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind, ModuleResource};
use cx_parsing_data::preparse::{CXNaiveTypeMap, NaiveFnIdent};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_typechecker_data::CXTypeMap;
use cx_typechecker_data::cx_types::{
    CXFunctionPrototype, CXParameter, CXTemplateInput, CXType, CXTypeKind,
};
use cx_typechecker_data::function_map::{CXFunctionIdentifier, CXFunctionKind};
use cx_typechecker_data::intrinsic_types::INTRINSIC_TYPES;
use cx_util::identifier::CXIdent;
use cx_util::log_error;

use crate::type_completion::templates::{instantiate_type_temp, mangle_template_name};
use crate::type_completion::prototypes::apply_implicit_fn_attr;

// As opposed to contextualizing the type like normal, pre-contextualizing a type does not require
// a fully complete type map. This can be thought of as the canon Naive -> CXType conversion since
// there will not always be an available one-degree-away mapping for a type.

fn reduce_ident_ident<'a>(
    cx_type: &'a CXNaiveType,
    type_map: &'a CXNaiveTypeMap,
) -> Option<&'a CXNaiveType> {
    match &cx_type.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = type_map.get_standard(&name.as_string()) {
                reduce_ident_ident(&inner.resource, type_map)
            } else {
                Some(cx_type)
            }
        }

        _ => Some(cx_type),
    }
}

fn ident_root<'a>(naive_map: &'a CXNaiveTypeMap, ty: &'a CXNaiveType) -> &'a CXNaiveType {
    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = naive_map.get_standard(&name.as_string()) {
                ident_root(naive_map, &inner.resource)
            } else {
                ty
            }
        }

        CXNaiveTypeKind::TemplatedIdentifier { name, .. } => {
            if let Some(template) = naive_map.get_template(&name.as_string()) {
                ident_root(naive_map, &template.resource.shell)
            } else {
                ty
            }
        }

        _ => ty,
    }
}

pub(crate) fn _complete_template_input(
    module_data: &ModuleData,
    cx_acc_types: &mut CXTypeMap,
    naive_type_map: &CXNaiveTypeMap,
    external_module: Option<&String>,
    input: &CXNaiveTemplateInput,
) -> Option<CXTemplateInput> {
    let _ty = input
        .params
        .iter()
        .map(|param| {
            _complete_type(module_data, cx_acc_types, naive_type_map, external_module, param)
                .unwrap_or_else(|| {
                    panic!("Failed to precontextualize template input type: {param}")
                })
        })
        .collect::<Vec<_>>();

    Some(CXTemplateInput { args: _ty })
}

pub(crate) fn _complete_type(
    module_data: &ModuleData,
    cx_acc_types: &mut CXTypeMap,
    base_naive_types: &CXNaiveTypeMap,
    external_module: Option<&String>,
    ty: &CXNaiveType,
) -> Option<CXType> {
    let base_type_map = match external_module {
        Some(module) => &module_data
            .accumulated_interface
            .get(&CompilationUnit::from_str(module))
            .0,
        None => base_naive_types,
    };

    let mut recurse_ty = |ty: &CXNaiveType| {
        Some(
            _complete_type(module_data, cx_acc_types, base_naive_types, None, ty)
                .unwrap_or_else(|| panic!("Failed to precontextualize type: {ty}")),
        )
    };

    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(existing) = cx_acc_types.get(&name.as_string()) {
                return Some(existing.clone());
            };
            
            if let Some(inner) = base_type_map.get_standard(&name.as_string()) {
                return _complete_type(
                    module_data,
                    cx_acc_types,
                    base_naive_types,
                    inner.external_module.as_ref(),
                    &inner.resource,
                );
            };

            if let Some((_, intrinsic)) = INTRINSIC_TYPES
                .iter()
                .find(|(intrinsic_name, _ty)| name.as_str() == *intrinsic_name)
            {
                return Some(intrinsic.clone().into());
            };

            log_error!("Type not found: {name}");
        }

        CXNaiveTypeKind::TemplatedIdentifier { name, input, .. } => {
            let contextualized_input = _complete_template_input(
                module_data,
                cx_acc_types,
                base_type_map,
                external_module,
                input,
            )?;

            instantiate_type_temp(module_data, cx_acc_types, base_type_map, &contextualized_input, name.as_str())
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
            let prototype = _complete_prototype(
                module_data,
                cx_acc_types,
                base_type_map,
                external_module,
                prototype,
            )
            .unwrap();

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

pub(crate) fn complete_fn_ident(
    _: &ModuleData,
    _: &mut CXTypeMap,
    naive_type_map: &CXNaiveTypeMap,
    ident: &NaiveFnIdent,
) -> Option<CXFunctionIdentifier> {
    match ident {
        NaiveFnIdent::Standard(name) => {
            Some(CXFunctionKind::Standard { name: name.clone() }.into())
        }

        NaiveFnIdent::MemberFunction {
            _type,
            function_name,
        } => {
            let base = _type.as_type();
            let root = ident_root(naive_type_map, &base);

            let Some(name) = root.get_name() else {
                panic!("Member function base type has no name: {root}");
            };

            Some(
                CXFunctionKind::Member {
                    base_type: name.clone(),
                    name: function_name.clone(),
                }
                .into(),
            )
        }

        NaiveFnIdent::Destructor(ty) => {
            let base = ty.as_type();
            let root = ident_root(naive_type_map, &base);

            let Some(type_name) = root.get_name() else {
                panic!("Destructor base type has no name: {root}");
            };

            Some(
                CXFunctionKind::Destructor {
                    base_type: type_name.clone(),
                }
                .into(),
            )
        }
    }
}

pub(crate) fn _complete_prototype(
    module_data: &ModuleData,
    cx_acc_map: &mut CXTypeMap,
    naive_type_map: &CXNaiveTypeMap,
    external_module: Option<&String>,
    prototype: &CXNaivePrototype,
) -> Option<CXFunctionPrototype> {
    let prototype = apply_implicit_fn_attr(prototype.clone());

    let return_type = _complete_type(
        module_data,
        cx_acc_map,
        naive_type_map,
        external_module,
        &prototype.return_type,
    )?;

    let parameters = prototype
        .params
        .iter()
        .map(|param| {
            let ty = _complete_type(
                module_data,
                cx_acc_map,
                naive_type_map,
                external_module,
                &param._type,
            )
            .unwrap();

            Some(CXParameter {
                name: param.name.clone(),
                _type: ty,
            })
        })
        .collect::<Option<Vec<_>>>()?;

    let ident = complete_fn_ident(module_data, cx_acc_map, naive_type_map, &prototype.name)?;

    Some(CXFunctionPrototype {
        name: ident,
        return_type,
        params: parameters,
        var_args: prototype.var_args,
    })
}


