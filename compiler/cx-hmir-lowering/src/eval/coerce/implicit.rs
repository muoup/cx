mod conversion;
mod reference;
mod string;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{compatible, result::Coercion},
        types,
    },
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
    if let (
        HMIRTypeKind::Array {
            element: from,
            length: Some(length),
        },
        HMIRTypeKind::Array {
            element: to,
            length: None,
        },
    ) = (env.resolve_kind(source), env.resolve_kind(target))
        && env.same_type(*from, *to)
        && env.semantics(source).move_semantics == env.semantics(target).move_semantics
    {
        let length = *length;
        let ty = types::array(context.env_mut(), target, length, span);
        return Ok(Coercion::Applied(value.with_ty(ty)));
    }
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
