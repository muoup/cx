use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        coerce::{compatible, result::Coercion},
        storage,
    },
    value::{HMIRValue, HMIRValueKind},
};

pub fn bind(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let env = context.env();
    if !compatible::compatible(env, value.ty(), inner) {
        return Ok(Coercion::Unapplied(value));
    }

    let storage = match value.kind() {
        HMIRValueKind::Indirect(_) => {
            if env.semantics(value.ty()).constant && !env.semantics(inner).constant {
                return Ok(Coercion::Unapplied(value));
            }
            value
        }
        _ if compatible::is_designator(env, value.ty()) => value,
        _ => storage::temporary(context, value, span)?,
    };

    Ok(Coercion::Applied(emit::address(
        context, &storage, target, span,
    )?))
}
