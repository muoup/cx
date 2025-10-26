use cx_parsing_data::data::{CXNaivePrototype, CXNaiveType};
use cx_typechecker_data::{
    ast::TCBaseMappings,
    cx_types::{CXFunctionPrototype, CXParameter, CXType},
};

use crate::{
    environment::TCEnvironment,
    type_completion::{prototypes::{apply_implicit_fn_attr, complete_fn_ident}, types::_complete_type},
};

pub mod prototypes;
pub mod templates;
pub mod types;

pub fn complete_prototype(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    external_module: Option<&String>,
    prototype: &CXNaivePrototype,
) -> Option<CXFunctionPrototype> {
    let prototype = apply_implicit_fn_attr(prototype.clone());

    let return_type = complete_type(env, base_data, external_module, &prototype.return_type)?;

    let parameters = prototype
        .params
        .iter()
        .map(|param| {
            let ty = complete_type(env, base_data, external_module, &param._type).unwrap();

            Some(CXParameter {
                name: param.name.clone(),
                _type: ty,
            })
        })
        .collect::<Option<Vec<_>>>()?;

    let ident = complete_fn_ident(env, env.base_data, &prototype.name)?;
    let prototype = CXFunctionPrototype {
        name: ident,
        return_type,
        params: parameters,
        var_args: prototype.var_args,
    };

    env.realized_fns
        .insert(prototype.name.clone(), prototype.clone());
    Some(prototype)
}

pub fn complete_type(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    external_module: Option<&String>,
    _type: &CXNaiveType,
) -> Option<CXType> {
    _complete_type(env, base_data, external_module, _type)
}
