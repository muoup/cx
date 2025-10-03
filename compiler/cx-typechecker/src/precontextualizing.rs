use std::collections::HashMap;
use std::sync::Arc;
use cx_ast_data::parse::ast::{CXGlobalStmt, CXAST};
use cx_util::identifier::CXIdent;
use cx_util::{log_error, CXResult};
use cx_ast_data::parse::parser::VisibilityMode;
use cx_ast_data::preparse::{CXNaiveFnIdent, CXNaiveFnMap, CXNaiveTypeMap};
use cx_ast_data::preparse::naive_types::{CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, ModuleResource};
use cx_ast_data::PreparseContents;
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_util::mangling::mangle_template;
use cx_typechecker_data::cx_types::{CXFunctionIdentifier, CXFunctionPrototype, CXParameter, CXType, CXTypeKind};
use cx_typechecker_data::{CXFnData, CXTypeData};
use cx_typechecker_data::ast::TCGlobalVariable;
use cx_typechecker_data::intrinsic_types::INTRINSIC_TYPES;
use crate::type_mapping::assemble_method;

// As opposed to contextualizing the type like normal, pre-contextualizing a type does not require
// a fully complete type map. This can be thought of as the canon Naive -> CXType conversion since
// there will not always be an available one-degree-away mapping for a type.

fn reduce_ident_ident<'a>(
    cx_type: &'a CXNaiveType,
    type_map: &'a CXNaiveTypeMap,
) -> Option<&'a CXNaiveType> {
    match &cx_type.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = type_map.standard.get(name.as_str()) {
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
    cx_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    external_module: Option<&String>,
    ty: &CXNaiveType,
) -> Option<CXType> {
    let mut naive_type_map = naive_type_map;
    
    // This variable is here for a top-level-scope lock in case we need to load an external module.
    #[allow(unused)]
    let mut pp_lock: Option<Arc<PreparseContents>> = None;

    let mut recurse_ty = |ty: &CXNaiveType| {
        Some(precontextualize_type(module_data, cx_map, naive_type_map, None, ty)
            .unwrap_or_else(|| panic!("Failed to precontextualize type: {ty}")))
    };

    if let Some(module) = external_module {
        pp_lock = Some(module_data.preparse_full.get(&CompilationUnit::from_str(module)));
        let pp = pp_lock.as_ref().unwrap().as_ref();

        naive_type_map = &pp.type_definitions;
    }

    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            if let Some(inner) = naive_type_map.standard.get(name.as_str()) {
                return precontextualize_type(
                    module_data, cx_map, naive_type_map,
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
            let template_name = mangle_template(name.as_str(), &input.params);

            if let Some(template) = cx_map.get(template_name.as_str()) {
                return Some(template.clone());
            }

            let Some(template) = naive_type_map.templates.get(name.as_str()) else {
                log_error!("Template not found: {name}<{}>", input.params.iter()
                    .map(|param| format!("{param}"))
                    .collect::<Vec<_>>()
                    .join(", "));
            };

            let shell = template.resource.shell.clone();
            let mut map_clone = naive_type_map.clone();

            for (name, naive_type) in template.resource.prototype
                .types
                .iter()
                .zip(input.params.iter()) {
                
                let reduced_type = reduce_ident_ident(naive_type, &map_clone)?;

                map_clone.insert_standard(name.clone(), ModuleResource::standard(reduced_type.clone()));
            }

            let mut cx_type = precontextualize_type(module_data, cx_map, &map_clone, None, &shell)?;
            cx_type.map_name(|name| mangle_template(name, &input.params));

            cx_map.insert_standard(template_name.clone(), cx_type.clone());
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
            let prototype = precontextualize_prototype(
                module_data, cx_map, naive_type_map,
                &ModuleResource {
                    visibility: VisibilityMode::Private,
                    external_module: external_module.cloned(),
                    resource: *prototype.clone()
                }
            ).unwrap();

            Some(
                CXType::from(
                    CXTypeKind::Function {
                        prototype: Box::new(prototype)
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
                        base_identifier: name.clone(),
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
                        variants: fields,
                    }
                )
            )
        }

        CXNaiveTypeKind::TaggedUnion { name, variants } => {
            let variants = variants.iter()
                .map(|(name, variant_type)| {
                    let variant_type = recurse_ty(variant_type)?;
                    Some((name.clone(), variant_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(
                CXType::from(
                    CXTypeKind::TaggedUnion {
                        name: name.clone(),
                        variants,
                    }
                )
            )
        },
    }
}

pub(crate) fn precontextualize_fn_ident(
    module_data: &ModuleData, type_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    ident: &CXNaiveFnIdent
) -> Option<CXFunctionIdentifier> {
    match ident {
        CXNaiveFnIdent::Standard(name) => {
            Some(CXFunctionIdentifier::Standard(name.clone()))
        },

        CXNaiveFnIdent::MemberFunction { _type, function_name } => {
            let cx_type = precontextualize_type(
                module_data, type_map, naive_type_map,
                None, _type
            )?;

            Some(
                CXFunctionIdentifier::MemberFunction {
                    _type: cx_type.clone(),
                    function_name: function_name.clone()
                }
            )
        },

        CXNaiveFnIdent::Destructor(ty) => {
            Some(CXFunctionIdentifier::Destructor(ty.clone()))
        },
    }
}

fn add_if_not_exists(cx_map: &mut CXTypeData, naive: &CXNaiveType, cx_type: &CXType) {
    if let CXNaiveTypeKind::Identifier { name, .. } = &naive.kind {
        if !cx_map.standard.contains_key(name.as_str()) {
            cx_map.insert_standard(name.as_string(), cx_type.clone());
        }
    } else if let CXNaiveTypeKind::TemplatedIdentifier { name, input } = &naive.kind {
        let mangled_name = mangle_template(name.as_str(), &input.params);

        if !cx_map.templates.contains_key(mangled_name.as_str()) {
            cx_map.insert_standard(mangled_name, cx_type.clone());
        }
    }
}

pub fn precontextualize_prototype(
    module_data: &ModuleData, type_map: &mut CXTypeData,
    naive_type_map: &CXNaiveTypeMap,
    prototype: &ModuleResource<CXNaivePrototype>,
) -> Option<CXFunctionPrototype> {
    let return_type = precontextualize_type(
        module_data, type_map, naive_type_map,
        prototype.external_module.as_ref(),
        &prototype.resource.return_type
    )?;

    let parameters = prototype.resource.params.iter()
        .map(|param| {
            let ty = precontextualize_type(
                module_data, type_map,
                naive_type_map,
                prototype.external_module.as_ref(),
                &param._type
            ).unwrap();

            Some(
                CXParameter {
                    name: param.name.clone(),
                    _type: ty
                }
            )
        })
        .collect::<Option<Vec<_>>>()?;

    let ident = precontextualize_fn_ident(
        module_data, type_map, naive_type_map,
        &prototype.resource.name
    )?;

    let cx_proto = assemble_method(
        &ident, return_type, parameters,
        prototype.resource.var_args
    );

    add_if_not_exists(type_map, &prototype.resource.return_type, &cx_proto.return_type);
    for (naive_param, param) in prototype.resource.params.iter().zip(&cx_proto.params) {
        add_if_not_exists(type_map, &naive_param._type, &param._type);
    }

    Some(cx_proto)
}

pub fn contextualize_type_map(module_data: &ModuleData, type_map: &CXNaiveTypeMap) -> CXResult<CXTypeData> {
    let mut cx_type_map = CXTypeData::new();

    for (name, template) in type_map.templates.iter() {
        cx_type_map.insert_template(name.clone(), template.clone());
    }

    for (intrinsic_name, intrinsic) in INTRINSIC_TYPES.iter() {
        cx_type_map.insert_standard(intrinsic_name.to_string(), intrinsic.clone().into());
    }

    for (name, naive_type) in type_map.standard.iter() {
        let Some(cx_type) = precontextualize_type(
            module_data, &mut cx_type_map, type_map,
            naive_type.external_module.as_ref(), &naive_type.resource
        ) else {
            log_error!("Failed to contextualize type: {name}");
        };

        cx_type_map.insert_standard(name.clone(), cx_type);
    }

    Some(cx_type_map)
}

pub fn contextualize_fn_map(
    module_data: &ModuleData, fn_map: &CXNaiveFnMap,
    type_map: &mut CXTypeData, naive_type_map: &CXNaiveTypeMap
) -> CXResult<CXFnData> {
    let mut cx_fn_map = CXFnData::new();

    for (name, naive_prototype) in fn_map.standard.iter() {
        let Some(cx_prototype)
            = precontextualize_prototype(module_data, type_map, naive_type_map, naive_prototype) else {
            log_error!("Failed to contextualize function prototype: {:#?}", naive_prototype);
        };

        cx_fn_map.insert_standard(name.clone(), cx_prototype);
    }

    for (name, template) in fn_map.templates.iter() {
        cx_fn_map.insert_template(name.clone(), template.clone());
    }


    Some(cx_fn_map)
}

pub fn contextualize_globals(module_data: &ModuleData,
                             type_map: &mut CXTypeData, naive_map: &CXNaiveTypeMap,
                             ast: &CXAST)
    -> Option<HashMap<String, TCGlobalVariable>> {

    let mut tc_globals = HashMap::new();

    for (name, constant) in ast.enum_constants.iter() {
        tc_globals.insert(
            name.clone(),
            TCGlobalVariable::UnaddressableConstant {
                name: CXIdent::from(name.clone()),
                val: *constant,
            }
        );
    }

    for global in ast.global_stmts.iter() {
        if let CXGlobalStmt::GlobalVariable { name, type_, initializer } = global {
            if initializer.is_some() {
                todo!("Global variable with initializer")
            }

            let _type = precontextualize_type(
                module_data, type_map, naive_map,
                None, type_
            )?;

            tc_globals.insert(
                name.to_string(),
                TCGlobalVariable::Variable {
                    name: name.clone(),
                    _type,
                    initializer: None
                }
            );
        }
    }

    Some(tc_globals)
}