use crate::environment::TCEnvironment;
use crate::type_completion::complete_type;
use crate::type_completion::types::_complete_type;
use cx_parsing_data::data::{CXNaiveParameter, CXNaivePrototype, CXNaiveTemplateInput, NaiveFnKind};
use cx_typechecker_data::ast::TCBaseMappings;
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXParameter, CXTemplateInput};
use cx_typechecker_data::function_map::{CXFunctionIdentifier, CXFunctionKind};
use cx_util::identifier::CXIdent;
use cx_util::log_error;

pub(crate) fn apply_implicit_fn_attr(
    mut proto: CXNaivePrototype,
) -> CXNaivePrototype {
    if let Some(implicit_member) = proto.name.implicit_member() {
        proto.params.insert(
            0,
            CXNaiveParameter {
                name: Some(CXIdent::from("this")),
                _type: implicit_member.as_type().pointer_to(false, 0),
            },
        );
    }

    proto
}

pub fn complete_template_args(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    template_args: &CXNaiveTemplateInput,
) -> Option<CXTemplateInput> {
    let args = template_args
        .params
        .iter()
        .map(|arg| complete_type(env, base_data, None, arg))
        .collect::<Option<Vec<_>>>()?;

    Some(CXTemplateInput { args })
}

pub(crate) fn complete_fn_ident(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    ident: &NaiveFnKind,
) -> Option<CXFunctionIdentifier> {
    match ident {
        NaiveFnKind::MemberFunction {
            function_name,
            _type,
        } => {
            let base = _type.as_type();
            let Some(_ty) = env.complete_type(&base) else {
                log_error!("Unknown type for member function: {function_name} of type {_type}");
            };

            let Some(cx_type) = _ty.get_identifier() else {
                log_error!(
                    "Member function base type should be identifiable: {function_name} of type {_type}"
                );
            };

            Some(
                CXFunctionKind::Member {
                    base_type: cx_type.clone(),
                    name: function_name.clone(),
                }
                .into(),
            )
        }

        NaiveFnKind::Destructor(name) => {
            let base = name.as_type();
            let Some(_ty) = _complete_type(env, base_data, None, &base) else {
                log_error!("Unknown type for destructor: {name}");
            };

            let Some(cx_type) = _ty.get_identifier() else {
                log_error!("Destructor base type should be identifiable: {name}");
            };

            Some(
                CXFunctionKind::Destructor {
                    base_type: cx_type.clone(),
                }
                .into(),
            )
        }

        NaiveFnKind::Standard(name) => {
            Some(CXFunctionKind::Standard { name: name.clone() }.into())
        }
    }
}

pub fn complete_fn_prototype(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    prototype: &CXNaivePrototype,
) -> Option<CXFunctionPrototype> {
    let ident = complete_fn_ident(env, base_data, &prototype.name)?;

    if let Some(existing) = env.get_realized_func(&ident) {
        return Some(existing.clone());
    }

    let prototype = apply_implicit_fn_attr(prototype.clone());
    let return_type = env.complete_type(&prototype.return_type)?;

    let parameters = prototype
        .params
        .iter()
        .map(|CXNaiveParameter { name, _type }| {
            let param_type = env.complete_type(_type)?;

            Some(CXParameter {
                name: name.clone(),
                _type: param_type,
            })
        })
        .collect::<Option<Vec<_>>>()?;

    Some(CXFunctionPrototype {
        name: ident,
        return_type,
        params: parameters,
        var_args: prototype.var_args,
    })
}
