
#[macro_export]
macro_rules! type_matches {
    ($env:expr, $lhs:expr, $rhs:pat) => {
        {
            use cx_compiler_typechecker::type_utils::get_intrinsic_val;

            matches!(get_intrinsic_val($env, $lhs)?, rhs)
        }
    };
}