use cx_ast::{
    ast::CXExpr,
    data::{CXFunctionPrototype, CXType},
};
use cx_mir::mir::{
    data::{MIRFunctionPrototype, MIRType},
    program::MIRBaseMappings,
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    type_completion::{
        prototypes::int_complete_fn_prototype,
        types::{base_data_from_module, int_complete_type},
    },
};

pub mod prototypes;
pub mod templates;
pub mod types;

pub fn complete_prototype_no_insert(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let (_, base_data) = base_data_from_module(env, base_data, external_module);

    int_complete_fn_prototype(env, base_data, prototype)
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    expr: &CXExpr,
    _type: &CXType,
) -> CXResult<MIRType> {
    let (_, base_data) = base_data_from_module(env, base_data, external_module);

    int_complete_type(env, base_data, expr, _type)
}
