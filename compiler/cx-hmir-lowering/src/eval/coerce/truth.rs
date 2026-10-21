use cx_hmir::{
    HMIRBinaryOp,
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{coerce::result::Coercion, types},
    value::{HMIRValue, constant::HMIRConstant},
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let ty = value.ty();
    let zero = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => {
            return Ok(Coercion::Applied(value));
        }
        HMIRTypeKind::Int(_) => HMIRConstant::Int { value: 0, ty },
        HMIRTypeKind::Float(_) => HMIRConstant::Float {
            value: FloatWrapper::from(0.0),
            ty,
        },
        HMIRTypeKind::PointerTo(_) => HMIRConstant::Null(ty),
        _ => return Ok(Coercion::Unapplied(value)),
    };

    let zero = HMIRValue::constant(ty, zero);
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let truth = emit::binary(context, HMIRBinaryOp::Neq, &value, &zero, bool_ty, span)?;

    Ok(Coercion::Applied(truth))
}
