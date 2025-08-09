use std::collections::HashMap;
use cx_util::log_error;
use crate::parse::ast::{CXFunctionPrototype, CXParameter};
use crate::parse::maps::CXTypeMap;
use crate::parse::template::{CXFunctionGenerator, CXTemplateTypeGen, CXTypeGenerator, TemplateGenerator};
use crate::parse::value_type::{CXType, CXTypeKind};
use crate::preparse::CXNaiveTypeMap;
use crate::preparse::pp_type::{CXNaiveType, CXNaiveTypeKind};

// As opposed to contextualizing the type like normal, pre-contextualizing a type does not require
// a fully complete type map. This can be thought of as the canon Naive -> CXType conversion since
// there will not always be an available one-degree-away mapping for a type.

pub(crate) fn precontextualize_type(
    incomplete_type_map: &CXTypeMap,
    naive_type_map: &CXNaiveTypeMap,
    ty: &CXNaiveType,
) -> Option<CXType> {
    match &ty.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            let Some(inner) = naive_type_map.get(name.as_str()) else {
                if let Some(_type) = incomplete_type_map.get(name.as_str()) {
                    return Some(_type.clone());
                }
                
                log_error!("Type not found: {name}");
            };
            
            precontextualize_type(incomplete_type_map, naive_type_map, inner)
        },
        
        CXNaiveTypeKind::TemplatedIdentifier { name, input, .. } => {
            let Some(template) = incomplete_type_map.get_generator(name.as_str()) else {
                log_error!("Template not found: {name}");
            };
        
            let shell = template.generator.get_shell().clone();
            let mut map_clone = naive_type_map.clone();
            
            for (name, naive_type) in template.generator.args()
                .iter()
                .zip(input.params.iter()) {
                
                map_clone.insert(name.clone(), naive_type.clone());
            }
            
            precontextualize_type(incomplete_type_map, &map_clone, &shell)
        },
        
        CXNaiveTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = precontextualize_type(incomplete_type_map, naive_type_map, inner)?;
            
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
            let inner_type = precontextualize_type(incomplete_type_map, naive_type_map, inner)?;
            
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
            let inner_type = precontextualize_type(incomplete_type_map, naive_type_map, inner)?;
            
            Some(
                CXType::from(
                    CXTypeKind::PointerTo {
                        inner_type: Box::new(inner_type),
                        sizeless_array: *is_array,
                        weak: false,
                        nullable: true,
                    }
                )
            )
        },
        
        CXNaiveTypeKind::PointerTo { inner_type, weak } => {
            let inner_type = precontextualize_type(incomplete_type_map, naive_type_map, inner_type)?;
            
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
            let return_type = precontextualize_type(incomplete_type_map, naive_type_map, &prototype.return_type)?;
            let params = prototype.params.iter()
                .map(|param|
                         Some(
                             CXParameter {
                                 name: param.name.clone(),
                                 _type: precontextualize_type(incomplete_type_map, naive_type_map, &param._type)?
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
                    let field_type = precontextualize_type(incomplete_type_map, naive_type_map, field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;
            
            Some(
                CXType::from(
                    CXTypeKind::Structured {
                        name: name.clone(),
                        fields,
                        has_destructor: false
                    }
                )
            )
        },
        
        CXNaiveTypeKind::Union { name, fields, .. } => {
            let fields = fields.iter()
                .map(|(name, field_type)| {
                    let field_type = precontextualize_type(incomplete_type_map, naive_type_map, field_type)?;
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