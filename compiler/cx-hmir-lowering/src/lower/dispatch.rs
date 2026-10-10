use cx_hmir::expr::{HMIRExpr, HMIRExprKind};
use cx_log::CXResult;
use cx_mir::MIRValue;

use crate::{env::lowering::FnLoweringContext, lower::{HMIRValue, constant::lower_constant}};

pub fn lower_expression(env: &mut FnLoweringContext, expr: &HMIRExpr) -> CXResult<HMIRValue> {
    match expr.kind() {
        HMIRExprKind::Constant(constant) => lower_constant(env, expr.span(), constant),

        _ => todo!()
    }
}
