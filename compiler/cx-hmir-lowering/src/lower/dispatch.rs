use cx_hmir::{
    HMIRBody,
    expr::{HMIRExpr, HMIRExprKind},
};
use cx_log::CXResult;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{HMIRValue, constant::lower_constant},
};

pub fn lower_expression(
    env: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: &HMIRExpr,
) -> CXResult<HMIRValue> {
    match expr.kind() {
        HMIRExprKind::Constant(constant) => lower_constant(env, body, expr.span(), constant),

        _ => todo!(),
    }
}
