use cx_hmir::ty::{
    HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext, intrinsic::HMIRIntWidth,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{
            promotion::{address, designated},
            result::Coercion,
        },
        op::specify,
        types,
    },
    value::HMIRValue,
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let subject = designated(context, &value);
    if !matches!(context.env().resolve_kind(subject), HMIRTypeKind::Str) {
        return Ok(Coercion::Unapplied(value));
    }

    let semantics = context.env().semantics(subject);
    let char = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
    let char = specify(
        context,
        char,
        HMIRTypeSemantics {
            constant: semantics.constant,
            volatile: semantics.volatile,
            ..Default::default()
        },
        span,
    );
    address(context, value, char, span)
}
