use cx_util::{log_error, CXResult};
use crate::parse::ast::{CXFunctionPrototype, CXParameter};
use crate::parse::intrinsic_types::INTRINSIC_TYPES;
use crate::parse::maps::{CXFunctionMap, CXTypeMap};
use crate::parse::precontextualizing::precontextualize_type;
use crate::parse::template::{CXTemplateInput, CXTemplateTypeGen};
use crate::parse::value_type::{CXType, CXTypeKind};
use crate::preparse::{CXNaiveFunctionMap, CXNaiveFunctionTemplates, CXNaiveTypeMap, CXNaiveTypeTemplates};
use crate::preparse::pp_type::{CXNaiveType, CXNaiveTypeKind, CXNaivePrototype, CXNaiveParameter, CXNaiveTemplateInput};

pub fn contextualize_type_map(type_map: &CXNaiveTypeMap, type_templates: &CXNaiveTypeTemplates) -> CXResult<CXTypeMap> {
    let mut cx_type_map = CXTypeMap::default();

    for template in type_templates.iter() {
        cx_type_map.insert_template(
            template.name.as_string(),
            CXTemplateTypeGen::from(template.clone())
        );
    }

    for (name, _type) in INTRINSIC_TYPES.iter() {
        cx_type_map.insert(name.to_string(), _type.clone().into());
    }

    for (name, naive_type) in type_map.iter() {
        let Some(cx_type) = precontextualize_type(&cx_type_map, type_map, naive_type) else {
            log_error!("Failed to contextualize type: {name}");
        };

        cx_type_map.insert(name.clone(), cx_type);
    }

    Some(cx_type_map)
}

pub fn contextualize_fn_map(type_map: &CXTypeMap, naive_fn_map: &CXNaiveFunctionMap, function_templates: &CXNaiveFunctionTemplates) -> CXResult<CXFunctionMap> {
    let mut cx_fn_map = CXFunctionMap::new();

    for (name, _, naive_prototype) in naive_fn_map.iter() {
        let Some(cx_prototype) = contextualize_fn_prototype(type_map, naive_prototype) else {
            log_error!("Failed to contextualize function prototype: {name}");
        };

        cx_fn_map.insert(name.clone(), cx_prototype);
    }
    
    for template in function_templates.iter() {
        let template_gen = CXTemplateTypeGen::from(template.clone());
        
        cx_fn_map.insert_template(template.name.to_string(), template_gen);
    }

    Some(cx_fn_map)
}

pub fn contextualize_template_args(type_map: &CXTypeMap, template_args: &CXNaiveTemplateInput) -> Option<CXTemplateInput> {
    let args = template_args.params.iter()
        .map(|arg| contextualize_type(type_map, arg))
        .collect::<Option<Vec<_>>>()?;

    Some(
        CXTemplateInput {
            params: args
        }
    )
}

pub fn contextualize_type(type_map: &CXTypeMap, naive_type: &CXNaiveType) -> Option<CXType> {
    match &naive_type.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            let Some(_type) = type_map.get(name.as_str()).cloned() else {
                log_error!("Unknown type: {name}");
            };

            Some(_type)
        },

        CXNaiveTypeKind::TemplatedIdentifier { name, input } => {
            let input = contextualize_template_args(type_map, input)?;

            type_map.get_template(type_map, name.as_str(), input)
        },

        CXNaiveTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = contextualize_type(type_map, inner)?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::Array {
                        inner_type: Box::new(inner_type),
                        size: *size,
                    }
                )
            )
        },
        CXNaiveTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = contextualize_type(type_map, inner)?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::PointerTo {
                        inner_type: Box::new(inner_type),
                        weak: false,
                        sizeless_array: true,
                        nullable: true
                    },
                )
            )
        },

        CXNaiveTypeKind::PointerTo { inner_type: inner, weak } => {
            let inner_type = contextualize_type(type_map, inner)?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::PointerTo {
                        inner_type: Box::new(inner_type),
                        weak: *weak,
                        sizeless_array: false,
                        nullable: true,
                    }
                )
            )
        },

        CXNaiveTypeKind::StrongPointer { inner, is_array } => {
            let inner_type = contextualize_type(type_map, inner)?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::StrongPointer {
                        inner_type: Box::new(inner_type),
                        is_array: *is_array,
                    }
                )
            )
        },

        CXNaiveTypeKind::Structured { name, fields } => {
            let fields = fields.iter()
                .map(|(name, field_type)| {
                    let field_type = contextualize_type(type_map, field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::Structured {
                        name: name.clone(),
                        fields,
                    }
                )
            )
        },

        CXNaiveTypeKind::Union { name, fields } => {
            let fields = fields.iter()
                .map(|(name, field_type)| {
                    let field_type = contextualize_type(type_map, field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::Union {
                        name: name.clone(),
                        fields,
                    }
                )
            )
        },

        CXNaiveTypeKind::FunctionPointer { prototype, .. } => {
            let prototype = contextualize_fn_prototype(type_map, prototype)?;

            Some(
                CXType::new(
                    0,
                    CXTypeKind::Function {
                        prototype: Box::new(prototype),
                    }
                )
            )
        },
    }
}

pub fn contextualize_fn_prototype(type_map: &CXTypeMap, prototype: &CXNaivePrototype)
    -> Option<CXFunctionPrototype> {
    let return_type = contextualize_type(type_map, &prototype.return_type)?;
    let parameters = prototype.params.iter()
        .map(| CXNaiveParameter { name, _type }| {
            let param_type = contextualize_type(type_map, _type)?;

            Some(CXParameter { name: name.clone(), _type: param_type })
        })
        .collect::<Option<Vec<_>>>()?;

    Some(CXFunctionPrototype {
        name: prototype.name.clone(),
        params: parameters,
        return_type,
        var_args: prototype.var_args,
    })
}