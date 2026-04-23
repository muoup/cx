use cx_ast::{ast::CXExpr, data::CXFunctionKind};
use cx_mir::mir::{
    data::MIRType,
    name_mangling::{base_mangle_member, base_mangle_standard, base_mangle_static_member},
    program::MIRBaseMappings,
};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

pub fn base_mangle_fn_name(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    kind: &CXFunctionKind,
) -> CXResult<String> {
    Ok(match &kind {
        CXFunctionKind::Standard(name) => base_mangle_standard(name.as_str()),

        CXFunctionKind::MemberFunction {
            name, member_type, ..
        } => {
            let member_type =
                env.complete_type(base_data, &CXExpr::default(), &member_type.as_type())?;
            base_mangle_member(&env.types.context, name.as_str(), &member_type)
        }

        CXFunctionKind::StaticMemberFunction { name, member_type } => {
            let member_type =
                env.complete_type(base_data, &CXExpr::default(), &member_type.as_type())?;
            base_mangle_static_member(&env.types.context, name.as_str(), &member_type)
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
    let base_mangle = base_mangle_fn_name(env, base_data, kind)?;
    let mut prototype_mangling = String::new();
    prototype_mangling.push_str(&env.types.context.mangle(return_type));
    for param_type in parameter_types {
        prototype_mangling.push_str(&env.types.context.mangle(param_type));
    }

    Ok(format!("{}_{}", base_mangle, prototype_mangling))
}
