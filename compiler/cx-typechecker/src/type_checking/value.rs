pub(crate) mod identifiers;
pub(crate) mod literals;
pub(crate) mod locals;
pub(crate) mod moves;
pub(crate) mod sizeof;
pub(crate) mod unsafe_ops;

use crate::{environment::TypeEnvironment, log_typecheck_error};
use cx_mir::mir::data::{MIRType, MIRTypeKind};
use cx_tokens::TokenRange;
use cx_util::CXResult;

pub(crate) fn ensure_valid_allocation_type(
    env: &mut TypeEnvironment,
    range: Option<TokenRange>,
    context: &str,
    ty: &MIRType,
) -> CXResult<()> {
    match &ty.kind {
        MIRTypeKind::Function { .. } => log_typecheck_error!(
            env,
            range,
            "Cannot create {} of function type '{}'; use a pointer to the function type instead",
            context,
            ty.display_with(&env.symbols.context)
        ),
        MIRTypeKind::Str => log_typecheck_error!(
            env,
            range,
            "Cannot create {} of unsized type 'str'; use '&str' instead",
            context
        ),
        MIRTypeKind::Array { inner_type, .. } => {
            let inner_type = env
                .symbols
                .context
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0))
                .clone();
            ensure_valid_allocation_type(env, range, "an array element", &inner_type)
        }
        _ => Ok(()),
    }
}
