pub(crate) mod aggregate;
pub(crate) mod arithmetic;
pub(crate) mod binding;
pub(crate) mod call;
pub(crate) mod coerce;
pub(crate) mod comptime;
pub(crate) mod control;
pub(crate) mod intrinsic;
pub(crate) mod name;
pub(crate) mod place;
pub(crate) mod staged;
pub(crate) mod types;

use cx_hmir::{expr::literal::HMIRLiteral, ty::HMIRTypeKind};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::{LocalLoweringContext, Permission},
    log::hmir_error,
    value::{
        self, HMIRValue,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmediate,
    },
};

pub(crate) fn eval_literal(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    literal: &HMIRLiteral,
) -> HMIRValue {
    match literal {
        HMIRLiteral::Unit => value::unit(context.env()),
        HMIRLiteral::Str(value) => {
            let ty = types::realize(context.env_mut(), HMIRTypeKind::Str, span);
            let literal = HMIRAddress::new(Addressable::Literal(value.clone()), 0);
            HMIRValue::new(ty, HMIRImmediate::Address(literal))
        }
        HMIRLiteral::Int { value, ty } => {
            let ty = types::int(context.env_mut(), ty.width(), ty.signed(), span);
            value::int(context.env(), *value, ty)
        }
        HMIRLiteral::Float { value, ty } => {
            let ty = types::realize(context.env_mut(), HMIRTypeKind::Float(*ty), span);
            value::float(context.env(), f64::from(value), ty)
        }
    }
}

pub(crate) fn require_nonsafe(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if !context.is_unevaluated() && context.permission() < Permission::Nonsafe {
        return Err(hmir_error(
            span,
            &typecheck::UNSAFE_OPERATION,
            operation.into(),
        ));
    }
    Ok(())
}

pub(crate) fn require_impure(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if !context.is_unevaluated() && !context.expectations().allows_impure() {
        return Err(hmir_error(
            span,
            &typecheck::IMPURE_OPERATION,
            operation.into(),
        ));
    }
    Ok(())
}
