use crate::environment::TCEnvironment;
use crate::type_completion::templates::mangle_template_name;
use cx_parsing_data::preparse::NaiveFnIdent;
use cx_parsing_data::preparse::naive_types::{
    CXNaiveParameter, CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind,
};
use cx_typechecker_data::cx_types::{
    CXFunctionPrototype, CXParameter, CXTemplateInput, CXType, CXTypeKind,
};
use cx_typechecker_data::function_map::{CXFunctionIdentifier, CXFunctionKind};
use cx_util::identifier::CXIdent;
use cx_util::log_error;

pub(crate) fn assemble_method(
    name: CXFunctionIdentifier,
    return_type: CXType,
    mut params: Vec<CXParameter>,
    var_args: bool,
) -> CXFunctionPrototype {
    let needs_buffer = return_type.is_structured();

    if let Some(implicit_member) = name.implicit_member() {
        params.insert(
            0,
            CXParameter {
                name: Some(CXIdent::from("this")),
                _type: implicit_member.clone(),
            },
        );
    }

    if needs_buffer {
        params.insert(
            0,
            CXParameter {
                name: Some(CXIdent::from("__internal_buffer")),
                _type: return_type.clone().pointer_to(),
            },
        );
    }

    CXFunctionPrototype {
        name,
        return_type,
        params,
        needs_buffer,
        var_args,
    }
}

pub fn contextualize_template_args(
    env: &mut TCEnvironment,
    template_args: &CXNaiveTemplateInput,
) -> Option<CXTemplateInput> {
    let args = template_args
        .params
        .iter()
        .map(|arg| contextualize_type(env, arg))
        .collect::<Option<Vec<_>>>()?;

    Some(CXTemplateInput { args })
}

pub(crate) fn contextualize_fn_ident(
    env: &mut TCEnvironment,
    ident: &NaiveFnIdent,
) -> Option<CXFunctionIdentifier> {
    match ident {
        NaiveFnIdent::MemberFunction {
            function_name,
            _type,
        } => {
            let _type = contextualize_type(env, _type)?;

            Some(
                CXFunctionKind::Member {
                    base_type: _type,
                    name: function_name.clone(),
                }
                .into(),
            )
        }

        NaiveFnIdent::Destructor(name) => {
            let cx_type = contextualize_type(env, name)?;

            Some(CXFunctionKind::Destructor { base_type: cx_type }.into())
        }

        NaiveFnIdent::Standard(name) => {
            Some(CXFunctionKind::Standard { name: name.clone() }.into())
        }
    }
}

pub fn contextualize_fn_prototype(
    env: &mut TCEnvironment,
    prototype: &CXNaivePrototype,
) -> Option<CXFunctionPrototype> {
    let ident = contextualize_fn_ident(env, &prototype.name)?;
    let return_type = contextualize_type(env, &prototype.return_type)?;
    let parameters = prototype
        .params
        .iter()
        .map(|CXNaiveParameter { name, _type }| {
            let param_type = contextualize_type(env, _type)?;

            Some(CXParameter {
                name: name.clone(),
                _type: param_type,
            })
        })
        .collect::<Option<Vec<_>>>()?;

    Some(assemble_method(
        ident,
        return_type,
        parameters,
        prototype.var_args,
    ))
}

pub fn contextualize_type(env: &mut TCEnvironment, naive_type: &CXNaiveType) -> Option<CXType> {
    match &naive_type.kind {
        CXNaiveTypeKind::Identifier { name, .. } => {
            let Some(_type) = env.get_type(name.as_str()) else {
                log_error!("Unknown type: {name}");
            };

            Some(_type.add_specifier(naive_type.specifiers))
        }

        CXNaiveTypeKind::TemplatedIdentifier { name, input } => {
            let input = contextualize_template_args(env, input)?;
            let mangled_name = mangle_template_name(name.as_str(), &input);

            if let Some(existing) = env.get_type(&mangled_name) {
                return Some(existing.clone());
            }

            if let Some(template) = env.get_templated_type(name.as_str(), &input) {
                return Some(template);
            }

            log_error!("Unknown templated type: {name}");
        }

        CXNaiveTypeKind::ExplicitSizedArray(inner, size) => {
            let inner_type = contextualize_type(env, inner)?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::Array {
                    inner_type: Box::new(inner_type),
                    size: *size,
                },
            ))
        }
        CXNaiveTypeKind::ImplicitSizedArray(inner) => {
            let inner_type = contextualize_type(env, inner)?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::PointerTo {
                    inner_type: Box::new(inner_type),
                    weak: false,
                    sizeless_array: true,
                    nullable: true,
                },
            ))
        }

        CXNaiveTypeKind::PointerTo {
            inner_type: inner,
            weak,
        } => {
            let inner_type = contextualize_type(env, inner)?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::PointerTo {
                    inner_type: Box::new(inner_type),
                    weak: *weak,
                    sizeless_array: false,
                    nullable: true,
                },
            ))
        }

        CXNaiveTypeKind::StrongPointer { inner, is_array } => {
            let inner_type = contextualize_type(env, inner)?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::StrongPointer {
                    inner_type: Box::new(inner_type),
                    is_array: *is_array,
                },
            ))
        }

        CXNaiveTypeKind::Structured { name, fields } => {
            let fields = fields
                .iter()
                .map(|(name, field_type)| {
                    let field_type = contextualize_type(env, field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::Structured {
                    name: name.clone(),
                    base_identifier: name.clone(),
                    fields,
                    move_semantics: false,
                    copyable: true,
                },
            ))
        }

        CXNaiveTypeKind::Union { name, fields } => {
            let fields = fields
                .iter()
                .map(|(name, field_type)| {
                    let field_type = contextualize_type(env, field_type)?;
                    Some((name.clone(), field_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::Union {
                    name: name.clone(),
                    variants: fields,
                },
            ))
        }

        CXNaiveTypeKind::TaggedUnion { name, variants } => {
            let variants = variants
                .iter()
                .map(|(name, variant_type)| {
                    let variant_type = contextualize_type(env, variant_type)?;
                    Some((name.clone(), variant_type))
                })
                .collect::<Option<Vec<_>>>()?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::TaggedUnion {
                    name: name.clone(),
                    variants,
                },
            ))
        }

        CXNaiveTypeKind::FunctionPointer { prototype, .. } => {
            let prototype = contextualize_fn_prototype(env, prototype)?;

            Some(CXType::new(
                naive_type.specifiers,
                CXTypeKind::Function {
                    prototype: Box::new(prototype),
                },
            ))
        }
    }
}
