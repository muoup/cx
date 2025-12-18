use cx_parsing_data::data::CXFunctionKind;
use cx_typechecker_data::mir::{name_mangling::{base_mangle_destructor, base_mangle_member, base_mangle_standard}, program::MIRBaseMappings, types::MIRType};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

fn prototype_mangle(return_type: &MIRType, parameter_types: &[MIRType]) -> String {
    let mut mangled = String::new();

    mangled.push_str(&return_type.mangle());

    for param_type in parameter_types {
        mangled.push_str(&param_type.mangle());
    }

    mangled
}

pub fn base_mangle_fn_name(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    kind: &CXFunctionKind,
) -> CXResult<String> {
    Ok(match &kind {
        CXFunctionKind::Standard(name) => base_mangle_standard(name.as_str()),

        CXFunctionKind::MemberFunction { name, member_type } => {
            let member_type = env.complete_type(base_data, &member_type.as_type())?;
            base_mangle_member(name.as_str(), &member_type)
        }

        CXFunctionKind::Destructor(type_name) => {
            let type_name = env.complete_type(base_data, &type_name.as_type())?;
            base_mangle_destructor(&type_name)
        }
    })
}

pub fn mangle_templated_fn_name(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    kind: &CXFunctionKind,
    return_type: &MIRType,
    parameter_types: &[MIRType],
) -> CXResult<String> {
    let prototype_mangling = prototype_mangle(return_type, parameter_types);

    // Destructors need not be mangled with template args, as they're only defining
    // trait is their destructing type.
    if matches!(kind, CXFunctionKind::Destructor { .. }) {
        return Ok(prototype_mangling);
    }

    Ok(format!(
        "{}_{}",
        base_mangle_fn_name(env, base_data, kind)?,
        prototype_mangling
    ))
}
