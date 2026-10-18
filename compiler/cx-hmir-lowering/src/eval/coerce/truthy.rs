use cx_hmir::{
    HMIRBinaryOp,
    expr::constant::HMIRConstant,
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{coerce::promotion, types},
    log::hmir_error,
};

// C11 6.8.4.1: a scalar is true when it compares unequal to 0
pub(crate) fn truthy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::rvalue(context, value, span)?;
    let ty = value.ty();

    let zero = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => return Ok(value),
        HMIRTypeKind::Int(_) => HMIRConstant::Int { value: 0, ty },
        HMIRTypeKind::Float(_) => HMIRConstant::Float {
            value: FloatWrapper::from(0.0),
            ty,
        },
        HMIRTypeKind::PointerTo(_) => HMIRConstant::Null(ty),
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::NO_TRUTH_VALUE,
                types::name(context.env(), ty),
            ));
        }
    };

    let zero = HMIRValue::constant(ty, zero);
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    emit::binary(context, HMIRBinaryOp::Neq, &value, &zero, bool_ty, span)
}
