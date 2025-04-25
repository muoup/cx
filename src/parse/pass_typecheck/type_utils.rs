use crate::parse::value_type::CXValType;
use crate::parse::pass_typecheck::TypeEnvironment;

pub fn type_matches(
    env: &TypeEnvironment,
    lhs: &CXValType,
    rhs: &CXValType
) -> Option<bool> {
    Some(get_intrinsic_val(env, lhs)? == get_intrinsic_val(env, rhs)?)
}

pub fn get_intrinsic_val<'a>(
    env: &'a TypeEnvironment,
    val: &'a CXValType
) -> Option<&'a CXValType> {
    match val {
        CXValType::Identifier(ident) => {
            let type_ = env.type_map.get(ident)?;

            get_intrinsic_val(env, type_)
        },

        _ => Some(val)
    }
}