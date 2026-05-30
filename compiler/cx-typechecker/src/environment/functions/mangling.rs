use cx_ast::ast::{expression::CXExpression, function::CXFunctionKind};
use cx_mir::mir::{
    data::MIRType,
    name_mangling::{base_mangle_member, base_mangle_standard, base_mangle_static_member},
    program::EnvironmentNamespace,
};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

pub fn base_mangle_fn_name(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
) -> CXResult<String> {
    Ok(match &kind {
        CXFunctionKind::Standard(name) => base_mangle_standard(name.as_str()),

        CXFunctionKind::MemberFunction {
            name, member_type, ..
        } => {
            let member_type =
                env.complete_type(namespace, &CXExpression::default(), &member_type.as_type())?;
            base_mangle_member(&env.symbols.context, name.as_str(), &member_type)
        }

        CXFunctionKind::StaticMemberFunction { name, member_type } => {
            let member_type =
                env.complete_type(namespace, &CXExpression::default(), &member_type.as_type())?;
            base_mangle_static_member(&env.symbols.context, name.as_str(), &member_type)
        }
    })
}

pub fn mangle_templated_fn_name(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    kind: &CXFunctionKind,
    return_type: &MIRType,
    parameter_types: &[MIRType],
) -> CXResult<String> {
    let base_mangle = base_mangle_fn_name(env, namespace, kind)?;
    let mut prototype_mangling = String::new();
    prototype_mangling.push_str(&env.symbols.context.mangle(return_type));
    for param_type in parameter_types {
        prototype_mangling.push_str(&env.symbols.context.mangle(param_type));
    }

    Ok(format!("{}_{}", base_mangle, prototype_mangling))
}
