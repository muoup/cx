use cx_util::{log_error};
use cx_util::mangling::mangle_templated_type;
use crate::parse::ast::{CXFunctionPrototype, CXParameter};
use crate::parse::CXFunctionIdentifier;
use crate::parse::identifier::CXIdent;
use crate::parse::maps::{CXTypeMap};
use crate::parse::template::{CXTemplateInput};
use crate::parse::value_type::{CXType, CXTypeKind};
use crate::preparse::CXNaiveFnIdent;
use crate::preparse::pp_type::{CXNaiveType, CXNaiveTypeKind, CXNaivePrototype, CXNaiveParameter, CXNaiveTemplateInput, PredeclarationType};

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
            
            let mangled_name = mangle_templated_type(name.as_str(), &input.params);
            
            if let Some(existing) = type_map.get(&mangled_name) {
                return Some(existing.clone());
            }

            if let Some(template) = type_map.get_template(type_map, name.as_str(), input) {
                return Some(template);
            }
            
            log_error!("Unknown templated type: {name}");
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