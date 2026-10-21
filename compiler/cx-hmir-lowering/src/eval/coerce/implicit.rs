mod conversion;
mod reference;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::coerce::{compatible, result::Coercion},
    value::HMIRValue,
};

pub fn convert(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let env = context.env();
    let source = value.ty();
    if matches!(env.resolve_kind(source), HMIRTypeKind::Unreachable)
        || (compatible::compatible(env, source, target)
            && compatible::qualified(env, source, target))
    {
        return Ok(Coercion::Applied(value.with_ty(target)));
    }

    match conversion::apply(context, value, target, span)? {
        Coercion::Applied(value) => convert(context, value, target, span),
        unapplied => Ok(unapplied),
    }
}
