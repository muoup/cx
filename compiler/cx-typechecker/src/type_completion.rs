use cx_ast::data::{CXPrototype, CXType};
use cx_mir::mir::{program::MIRBaseMappings, types::{MIRFunctionPrototype, MIRType}};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment, type_completion::{prototypes::_complete_fn_prototype, types::{_complete_type, base_data_from_module}}
};

pub mod prototypes;
pub mod templates;
pub mod types;

pub fn complete_prototype_no_insert(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    prototype: &CXPrototype,
) -> CXResult<MIRFunctionPrototype> {
    let (_, base_data) = base_data_from_module(env, base_data, external_module);
    
    _complete_fn_prototype(env, base_data, prototype)
}

pub fn complete_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    _type: &CXType,
) -> CXResult<MIRType> {
    let (_, base_data) = base_data_from_module(env, base_data, external_module);
    
    _complete_type(env, base_data, _type)
}
