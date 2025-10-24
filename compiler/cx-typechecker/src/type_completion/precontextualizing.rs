use cx_parsing_data::parse::ast::{CXAST, CXGlobalStmt};
use cx_parsing_data::parse::parser::VisibilityMode;
use cx_parsing_data::preparse::naive_types::{
    CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind, ModuleResource
};
use cx_parsing_data::preparse::{NaiveFnIdent, CXNaiveFnMap, CXNaiveTypeMap};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_typechecker_data::ast::TCGlobalVariable;
use cx_typechecker_data::cx_types::{
    CXFunctionPrototype, CXParameter, CXTemplateInput, CXType, CXTypeKind
};
use cx_typechecker_data::function_map::{CXFnData, CXFunctionIdentifier, CXFunctionKind};
use cx_typechecker_data::intrinsic_types::INTRINSIC_TYPES;
use cx_typechecker_data::CXTypeData;
use cx_util::identifier::CXIdent;
use cx_util::{CXResult, log_error};
use std::collections::HashMap;
use std::sync::Arc;

use crate::type_completion::templates::mangle_template_name;
use crate::type_completion::type_mapping::apply_implicit_fn_attr;

// As opposed to contextualizing the type like normal, pre-contextualizing a type does not require
// a fully complete type map. This can be thought of as the canon Naive -> CXType conversion since
// there will not always be an available one-degree-away mapping for a type.

fn reduce_ident_ident<'a>(
    cx_type: &'a CXNaiveType,
    type_map: &'a CXNaiveTypeMap,
) -> Option<&'a CXNaiveType> {
    match &cx_type.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = type_map.get(&name.as_string()) {
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
            if let Some(inner) = naive_map.get(&name.as_string()) {
                ident_root(naive_map, &inner.resource)
            } else {
                ty
            }
        },
        
        CXNaiveTypeKind::TemplatedIdentifier { name, .. } => {
            if let Some(template) = naive_map.get_template(&name.as_string()) {
                ident_root(naive_map, &template.resource.shell)
            } else {
                ty
            }
        },

        _ => ty,
    }
}

fn precontextualize_template_input(
    module_data: &ModuleData,
    cx_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    external_module: Option<&String>,
    input: &CXNaiveTemplateInput,
) -> Option<CXTemplateInput> {
    let _ty = input.params
        .iter()
        .map(|param| {
            precontextualize_type(
                module_data,
                cx_map,
                naive_type_map,
                external_module,
                param,
            )
            .unwrap_or_else(|| panic!("Failed to precontextualize template input type: {param}"))
        })
        .collect::<Vec<_>>();
    
    Some(CXTemplateInput { args: _ty })
}

pub fn precontextualize_type(
    module_data: &ModuleData,
    cx_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    external_module: Option<&String>,
    ty: &CXNaiveType,
) -> Option<CXType> {
    let mut naive_type_map = naive_type_map;

    // This variable is here for a top-level-scope lock in case we need to load an external module.
    #[allow(unused)]
    let mut ast_lock: Option<Arc<_>> = None;

    let mut recurse_ty = |ty: &CXNaiveType| {
        Some(
            precontextualize_type(module_data, cx_map, naive_type_map, None, ty)
                .unwrap_or_else(|| panic!("Failed to precontextualize type: {ty}")),
        )
    };

    if let Some(module) = external_module {
        ast_lock = Some(
            module_data
                .naive_ast
                .get(&CompilationUnit::from_str(module)),
        );
        let ast = ast_lock.as_ref().unwrap().as_ref();

        naive_type_map = &ast.type_map;
    }

    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = naive_type_map.get(&name.as_string()) {
                return precontextualize_type(
                    module_data,
                    cx_map,
                    naive_type_map,
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
            let contextualized_input = precontextualize_template_input(
                module_data,
                cx_map,
                naive_type_map,
                external_module,
                input,
            )?;
            
            let template_name = mangle_template_name(name.as_str(), &contextualized_input);
                                    
            if let Some(template) = cx_map.get(template_name.as_str()) {
                return Some(template.clone());
            }
            
            let Some(template) = naive_type_map.get_template(&name.as_string()) else {
                log_error!(
                    "Template not found: {name}<{}>",
                    input
                        .params
                        .iter()
                        .map(|param| format!("{param}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            };

            let shell = &template.resource.shell;
            let mut map_clone = naive_type_map.clone();

            for (name, naive_type) in template
                .resource
                .prototype
                .types
                .iter()
                .zip(input.params.iter())
            {
                let reduced_type = reduce_ident_ident(naive_type, naive_type_map)?;
                
                map_clone
                    .insert_standard(name.clone(), ModuleResource::standard(reduced_type.clone()));
            }

            let mut cx_type = precontextualize_type(module_data, cx_map, &map_clone, None, shell)?;
            cx_type.set_name(CXIdent::from(template_name.as_str()));
            cx_map.insert_standard(template_name, cx_type.clone());
            Some(cx_type)
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
            let prototype = precontextualize_prototype(
                module_data,
                cx_map,
                naive_type_map,
                &ModuleResource {
                    visibility: VisibilityMode::Private,
                    external_module: external_module.cloned(),
                    resource: *prototype.clone(),
                },
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

pub(crate) fn precontextualize_fn_ident(
    _: &ModuleData,
    _: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    ident: &NaiveFnIdent,
) -> Option<CXFunctionIdentifier> {
    match ident {
        NaiveFnIdent::Standard(name) => Some(CXFunctionKind::Standard { name: name.clone() }.into()),

        NaiveFnIdent::MemberFunction {
            _type,
            function_name,
        } => {
            let base = _type.as_type();
            let root = ident_root(naive_type_map, &base);
            
            let Some(name) = root.get_name() else {
                panic!("Member function base type has no name: {root}");
            };
            
            Some(CXFunctionKind::Member {
                base_type: name.clone(),
                name: function_name.clone(),
            }.into())
        }

        NaiveFnIdent::Destructor(ty) => {
            let base = ty.as_type();
            let root = ident_root(naive_type_map, &base);
            
            let Some(type_name) = root.get_name() else {
                panic!("Destructor base type has no name: {root}");
            };
            
            Some(CXFunctionKind::Destructor { base_type: type_name.clone() }.into())
        },
    }
}

// fn add_if_not_exists(cx_map: &mut CXTypeData, naive: &CXNaiveType, cx_type: &CXType) {
//     if let CXNaiveTypeKind::Identifier { name, .. } = &naive.kind {
//         if !cx_map.standard.contains_key(name.as_str()) {
//             cx_map.insert_standard(name.as_string(), cx_type.clone());
//         }
//     } else if let CXNaiveTypeKind::TemplatedIdentifier { name, input } = &naive.kind {
//         let mangled_name = mangle_template_name(name.as_str(), &input);

//         if !cx_map.templates.contains_key(mangled_name.as_str()) {
//             cx_map.insert_standard(mangled_name, cx_type.clone());
//         }
//     }
// }

pub fn precontextualize_prototype(
    module_data: &ModuleData,
    type_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    prototype_resource: &ModuleResource<CXNaivePrototype>,
) -> Option<CXFunctionPrototype> {
    let prototype = apply_implicit_fn_attr(prototype_resource.resource.clone());
    
    let return_type = precontextualize_type(
        module_data,
        type_map,
        naive_type_map,
        prototype_resource.external_module.as_ref(),
        &prototype.return_type,
    )?;

    let parameters = prototype.params
        .iter()
        .map(|param| {
            let ty = precontextualize_type(
                module_data,
                type_map,
                naive_type_map,
                prototype_resource.external_module.as_ref(),
                &param._type,
            )
            .unwrap();

            Some(CXParameter {
                name: param.name.clone(),
                _type: ty,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    
    let ident = precontextualize_fn_ident(
        module_data,
        type_map,
        naive_type_map,
        &prototype.name,
    )?;

    Some(
        CXFunctionPrototype {
            name: ident,
            return_type,
            params: parameters,
            var_args: prototype.var_args,
        }
    )
}

pub fn contextualize_type_map(
    module_data: &ModuleData,
    type_map: &CXNaiveTypeMap,
) -> CXResult<CXTypeData> {
    let mut cx_type_map = CXTypeData::new();

    for (name, template) in type_map.template_iter() {
        cx_type_map.insert_template(name.clone(), template.clone());
    }

    for (intrinsic_name, intrinsic) in INTRINSIC_TYPES.iter() {
        cx_type_map.insert_standard(intrinsic_name.to_string(), intrinsic.clone().into());
    }

    for (name, naive_type) in type_map.standard_iter() {
        let Some(cx_type) = precontextualize_type(
            module_data,
            &mut cx_type_map,
            type_map,
            naive_type.external_module.as_ref(),
            &naive_type.resource,
        ) else {
            log_error!("Failed to contextualize type: {name}");
        };

        cx_type_map.insert_standard(name.clone(), cx_type);
    }

    Some(cx_type_map)
}

pub fn contextualize_fn_map(
    module_data: &ModuleData,
    fn_map: &CXNaiveFnMap,
    type_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
) -> CXResult<CXFnData> {
    let mut cx_fn_map = CXFnData::new();

    for (_, naive_prototype) in fn_map.standard_iter() {
        let Some(cx_prototype) =
            precontextualize_prototype(module_data, type_map, naive_type_map, naive_prototype)
        else {
            log_error!(
                "Failed to contextualize function prototype: {:#?}",
                naive_prototype
            );
        };

        cx_fn_map.insert_standard(cx_prototype);
    }

    for (_, template) in fn_map.template_iter() {
        let ident = precontextualize_fn_ident(
            module_data,
            type_map,
            naive_type_map,
            &template.resource.shell.name,
        )?;
        
        cx_fn_map.insert_template(ident, template.clone());
    }

    Some(cx_fn_map)
}

pub fn contextualize_globals(
    module_data: &ModuleData,
    type_map: &mut CXTypeData,
    naive_map: &CXNaiveTypeMap,
    ast: &CXAST,
) -> Option<HashMap<String, TCGlobalVariable>> {
    let mut tc_globals = HashMap::new();

    for (name, constant) in ast.enum_constants.iter() {
        tc_globals.insert(
            name.clone(),
            TCGlobalVariable::UnaddressableConstant {
                name: CXIdent::from(name.clone()),
                val: *constant,
            },
        );
    }

    for global in ast.global_stmts.iter() {
        if let CXGlobalStmt::GlobalVariable {
            name,
            type_,
            initializer,
        } = global
        {
            if initializer.is_some() {
                todo!("Global variable with initializer")
            }

            let _type = precontextualize_type(module_data, type_map, naive_map, None, type_)?;

            tc_globals.insert(
                name.to_string(),
                TCGlobalVariable::Variable {
                    name: name.clone(),
                    _type,
                    initializer: None,
                },
            );
        }
    }

    Some(tc_globals)
}
