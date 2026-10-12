use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind,
    context::HMIRTypeContext,
    intrinsic::{HMIRFloatType, HMIRIntWidth},
};
use cx_tokens::TokenRange;

use crate::{env::function::FnLoweringContext, eval::types};

pub(crate) fn integer(
    context: &mut FnLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    match context.env().resolve_type_id(ty).kind() {
        HMIRTypeKind::Int(int) if int.width() < HMIRIntWidth::I32 => {
            types::int(context.env_mut(), HMIRIntWidth::I32, true, span)
        }
        _ => ty,
    }
}

pub(crate) fn common(
    context: &mut FnLoweringContext,
    lhs: HMIRTypeID,
    rhs: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let lhs = integer(context, lhs, span);
    let rhs = integer(context, rhs, span);

    let kinds = (
        context.env().resolve_type_id(lhs).kind().clone(),
        context.env().resolve_type_id(rhs).kind().clone(),
    );

    match kinds {
        (HMIRTypeKind::Float(left), HMIRTypeKind::Float(right)) => {
            let width = left.width().max(right.width());
            let kind = HMIRTypeKind::Float(HMIRFloatType::new(width));
            Some(context.env_mut().resolver_mut().realize_type(kind, span))
        }
        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_)) => Some(lhs),
        (HMIRTypeKind::Int(_), HMIRTypeKind::Float(_)) => Some(rhs),
        (HMIRTypeKind::Int(left), HMIRTypeKind::Int(right)) => {
            let (width, signed) = if left.width() == right.width() {
                (left.width(), left.signed() && right.signed())
            } else if left.width() > right.width() {
                (left.width(), left.signed())
            } else {
                (right.width(), right.signed())
            };

            Some(types::int(context.env_mut(), width, signed, span))
        }
        _ => None,
    }
}
