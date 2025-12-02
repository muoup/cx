use cx_parsing_data::data::{CXNaivePrototype, CXNaiveType};
use cx_typechecker_data::mir::{program::MIRBaseMappings, types::{CXFunctionPrototype, CXType}};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment, type_completion::{prototypes::_complete_fn_prototype, types::{_complete_type, base_data_from_module}}
};

pub mod prototypes;
pub mod templates;
pub mod types;

pub fn complete_fn_prototype(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    prototype: &CXNaivePrototype,
) -> CXResult<CXFunctionPrototype> {
    let (_, base_data) = base_data_from_module(env, base_data, external_module);
    
    _complete_fn_prototype(env, base_data, prototype)
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    _type: &CXNaiveType,
) -> CXResult<CXType> {
    let (_, base_data) = base_data_from_module(env, base_data, external_module);
    
    _complete_type(env, base_data, _type)
}
