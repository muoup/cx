use std::sync::Arc;
use cx_util::{log_error, CXResult};
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXParameter};
use cx_data_ast::parse::intrinsic_types::INTRINSIC_TYPES;
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::template::{CXTemplateTypeGen, TemplateGenerator};
use cx_data_ast::parse::type_mapping::contextualize_fn_prototype;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_ast::preparse::{CXNaiveFunctionMap, CXNaiveFunctionTemplates, CXNaiveTypeMap, CXNaiveTypeTemplates};
use cx_data_ast::preparse::pp_type::{CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, ModuleResource};
use cx_data_ast::PreparseContents;
use cx_data_pipeline::CompilationUnit;
use cx_data_pipeline::db::ModuleData;
use cx_util::mangling::{mangle_templated_type};
// As opposed to contextualizing the type like normal, pre-contextualizing a type does not require
// a fully complete type map. This can be thought of as the canon Naive -> CXType conversion since
// there will not always be an available one-degree-away mapping for a type.

fn reduce_ident_ident<'a>(
    cx_type: &'a CXNaiveType,
    naive_map: &'a CXNaiveTypeMap,
) -> Option<&'a CXNaiveType> {
    match &cx_type.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = naive_map.get(name.as_str()) {
                Some(&inner.resource)
            } else {
                Some(cx_type)
            }
        },
        
        _ => Some(cx_type)
    }
}

pub fn precontextualize_type(
    module_data: &ModuleData,
    cx_map: &mut CXTypeMap,
    naive_type_map: &CXNaiveTypeMap, naive_type_templates: &CXNaiveTypeTemplates,
    external_module: Option<&String>,
    ty: &CXNaiveType,
) -> Option<CXType> {
    let mut naive_type_map = naive_type_map;
    let mut naive_type_templates = naive_type_templates;
    let mut pp_lock: Option<Arc<PreparseContents>> = None;

    let mut recurse_ty = |ty: &CXNaiveType| {
        precontextualize_type(module_data, cx_map, naive_type_map, naive_type_templates, None, ty)
    };

    if let Some(module) = external_module {
        pp_lock = Some(module_data.preparse_full.get(&CompilationUnit::from_str(module)));
        let pp = pp_lock.as_ref().unwrap().as_ref();

        naive_type_map = &pp.type_definitions;
        naive_type_templates = &pp.type_templates;
    }

    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = naive_type_map.get(name.as_str()) {
                return precontextualize_type(
                    module_data, cx_map, naive_type_map, naive_type_templates,
                    inner.external_module.as_ref(), &inner.resource
                );
            };

            if let Some((_, intrinsic)) = INTRINSIC_TYPES.iter()
                .find(|(intrinsic_name, ty)| name.as_str() == *intrinsic_name) {
                return Some(intrinsic.clone().into());
            };

            log_error!("Type not found: {name}");
        },

        CXNaiveTypeKind::TemplatedIdentifier { name, input, .. } => {
            let template_name = mangle_templated_type(name.as_str(), &input.params);

            if let Some(template) = cx_map.get(template_name.as_str()) {
                return Some(template.clone());
            }

            let Some(template) = naive_type_templates.get(name.as_str()) else {
                log_error!("Template not found: {name}<{}>", input.params.iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<_>>()
                    .join(", "));
            };

            let shell = template.resource.shell.clone();
            let mut map_clone = naive_type_map.clone();

            for (name, naive_type) in template.resource.inputs
                .iter()
                .zip(input.params.iter()) {
                
                let reduced_type = reduce_ident_ident(naive_type, &map_clone)?;

                map_clone.insert(name.clone(), ModuleResource::standard(reduced_type.clone()));
            }

            let cx_type = precontextualize_type(module_data, cx_map, &map_clone, naive_type_templates, None, &shell)?;
            cx_map.insert(template_name.clone(), cx_type.clone());

            Some(cx_type)
        },

        CXNaiveTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = recurse_ty(inner)?;

            Some(
                CXType::from(
                    CXTypeKind::Array {
                        inner_type: Box::new(inner_type),
                        size: *size,
                    }
                )
            )
        },

        CXNaiveTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = recurse_ty(inner)?;

            Some(
                CXType::from(
                    CXTypeKind::PointerTo {
                        inner_type: Box::new(inner_type),
                        sizeless_array: true,
                        weak: false,
                        nullable: true,
                    }
                )
            )
        },

        CXNaiveTypeKind::StrongPointer { inner, is_array } => {
            let inner_type = recurse_ty(inner)?;

            Some(
                CXType::from(
                    CXTypeKind::StrongPointer {
                        inner_type: Box::new(inner_type),
                        is_array: *is_array,
                    }
                )
            )
        },

        CXNaiveTypeKind::PointerTo { inner_type, weak } => {
            let inner_type = recurse_ty(inner_type.as_ref())?;

            Some(
                CXType::from(
                    CXTypeKind::PointerTo {
                        inner_type: Box::new(inner_type),
                        weak: *weak,
                        sizeless_array: false,
                        nullable: true,
                    }
                )
            )
        },

        CXNaiveTypeKind::FunctionPointer { prototype } => {
            let return_type = recurse_ty(&prototype.return_type)?;
            let params = prototype.params.iter()
                .map(|param|
                         Some(
                             CXParameter {
                                 name: param.name.clone(),
                                 _type: recurse_ty(&param._type)?
                             }
                         ))
                .collect::<Option<Vec<_>>>()?;

            Some(
                CXType::from(
                    CXTypeKind::Function {
                        prototype: Box::new(CXFunctionPrototype {
                            name: prototype.name.clone(),
                            return_type,
                            params,
                            var_args: prototype.var_args,
                        })
                    }
                )
            )
        },

        CXNaiveTypeKind::Structured { name, fields, .. } => {
            let fields = fields.iter()
                .map(|(name, field_type)| {
                    let field_type = recurse_ty(field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(
                CXType::from(
                    CXTypeKind::Structured {
                        name: name.clone(),
                        fields,
                    }
                )
            )
        },

        CXNaiveTypeKind::Union { name, fields, .. } => {
            let fields = fields.iter()
                .map(|(name, field_type)| {
                    let field_type = recurse_ty(field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(
                CXType::from(
                    CXTypeKind::Union {
                        name: name.clone(),
                        fields,
                    }
                )
            )
        }
    }
}

pub fn precontextualize_prototype(
    module_data: &ModuleData, type_map: &mut CXTypeMap,
    pp_data: &PreparseContents, prototype: &ModuleResource<CXNaivePrototype>,
) -> Option<CXFunctionPrototype> {
    let add_if_not_exists = |cx_map: &mut CXTypeMap, naive: &CXNaiveType, cx_type: &CXType| {
        if let CXNaiveTypeKind::Identifier { name, .. } = &naive.kind {
            if !cx_map.contains_key(name.as_str()) {
                cx_map.insert(name.as_string(), cx_type.clone());
            }
        } else if let CXNaiveTypeKind::TemplatedIdentifier { name, input } = &naive.kind {
            let mangled_name = mangle_templated_type(name.as_str(), &input.params);

            if !cx_map.contains_key(mangled_name.as_str()) {
                cx_map.insert(mangled_name, cx_type.clone());
            }
        }
    };

    let return_type = precontextualize_type(
        module_data, type_map,
        &pp_data.type_definitions, &pp_data.type_templates,
        prototype.external_module.as_ref(),
        &prototype.resource.return_type
    )?;
    add_if_not_exists(type_map, &prototype.resource.return_type, &return_type);

    let parameters = prototype.resource.params.iter()
        .map(|param| {
            let ty = precontextualize_type(
                module_data, type_map,
                &pp_data.type_definitions, &pp_data.type_templates,
                prototype.external_module.as_ref(),
                &param._type
            )?;

            Some(
                CXParameter {
                    name: param.name.clone(),
                    _type: ty
                }
            )
        })
        .collect::<Option<Vec<_>>>()?;

    for (naive_param, param) in prototype.resource.params.iter().zip(&parameters) {
        add_if_not_exists(type_map, &naive_param._type, &param._type);
    }

    Some(
        CXFunctionPrototype {
            name: prototype.resource.name.clone(),
            return_type,
            params: parameters,
            var_args: prototype.resource.var_args,
        }
    )
}

pub fn contextualize_type_map(
    module_data: &ModuleData,
    type_map: &CXNaiveTypeMap, type_templates: &CXNaiveTypeTemplates
) -> CXResult<CXTypeMap> {
    let mut cx_type_map = CXTypeMap::default();

    for (name, template) in type_templates.iter() {
        cx_type_map.insert_template(
            name.clone(),
            CXTemplateTypeGen::from(template.resource.clone())
        );
    }

    for (intrinsic_name, intrinsic) in INTRINSIC_TYPES.iter() {
        cx_type_map.insert(intrinsic_name.to_string(), intrinsic.clone().into());
    }

    for (name, naive_type) in type_map.iter() {
        let Some(cx_type) = precontextualize_type(
            module_data, &mut cx_type_map, type_map, type_templates, 
            naive_type.external_module.as_ref(), &naive_type.resource
        ) else {
            log_error!("Failed to contextualize type: {name}");
        };

        cx_type_map.insert(name.clone(), cx_type);
    }

    Some(cx_type_map)
}

pub fn contextualize_fn_map(module_data: &ModuleData, type_map: &mut CXTypeMap, pp_data: &PreparseContents)
    -> CXResult<CXFunctionMap> {
    let mut cx_fn_map = CXFunctionMap::new();

    for (name, naive_prototype) in pp_data.function_definitions.iter() {
        let Some(cx_prototype)
            = precontextualize_prototype(module_data, type_map, &pp_data, naive_prototype) else {
            log_error!("Failed to contextualize function prototype: {name}");
        };

        cx_fn_map.insert(name.clone(), cx_prototype);
    }

    for template in pp_data.function_templates.iter() {
        let template_gen = CXTemplateTypeGen::from(template.resource.clone());

        cx_fn_map.insert_template(template.resource.name.to_string(), template_gen);
    }

    Some(cx_fn_map)
}