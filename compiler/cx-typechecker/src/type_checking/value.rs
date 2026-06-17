pub(crate) mod identifiers;
pub(crate) mod literals;
pub(crate) mod locals;
pub(crate) mod moves;
pub(crate) mod unsafe_ops;

use crate::{environment::TypeEnvironment, log_typecheck_error};
use cx_log::CXResult;
use cx_mir::{
    mir::data::{MIRType, MIRTypeKind},
    type_context::MIRTypeContext,
};
use cx_tokens::TokenRange;

pub(crate) fn ensure_valid_allocation_type(
    env: &mut TypeEnvironment,
    range: TokenRange,
    context: &str,
    ty: &MIRType,
) -> CXResult<()> {
    match &ty.kind {
        MIRTypeKind::Function { .. } => log_typecheck_error!(
            env,
            range,
            "Cannot create {} of function type '{}'; use a pointer to the function type instead",
            context,
            ty.display_with(&env.symbols)
        ),
        MIRTypeKind::Str => log_typecheck_error!(
            env,
            range,
            "Cannot create {} of unsized type 'str'; use '&str' instead",
            context
        ),
        MIRTypeKind::Array { inner_type, .. } => {
            let inner_type = env.symbols.resolve_type_id(*inner_type).clone();
            ensure_valid_allocation_type(env, range.clone(), "an array element", &inner_type)
        }
        _ => Ok(()),
    }
}
