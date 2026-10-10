use cx_hmir::expr::constant::HMIRConstant;
use cx_log::CXResult;
use cx_mir::{MIRTypeKind, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{HMIRValue, HMIRValueKind},
};

pub(crate) fn lower_constant(
    env: &mut FnLoweringContext,
    span: &TokenRange,
    constant: &HMIRConstant,
) -> CXResult<HMIRValue> {
    match constant {
        // Note: because lowering refers to runtime expressions, where comptime expressions are "evaluated", there
        // is not a need to handle HMIRConstant::Comptime here. A type reaching this path would indicate that we are
        // trying to use a type directly.
        //
        // In the future, we may want to introduce a runtime-friendly type info API, but for now, we will just error out.
        HMIRConstant::Type(_) => todo!("Error: unexpected type constant in runtime expression"),

        HMIRConstant::Aggregate { ty, .. }
        | HMIRConstant::Int { ty, .. }
        | HMIRConstant::Float { ty, .. } => Ok(HMIRValue::new(
            *ty,
            HMIRValueKind::Constant(constant.clone()),
        )),

        _ => todo!(),
    }
}
