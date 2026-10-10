use cx_hmir::{
    HMIRBody, HMIRCoerceMode,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic,
    MIRTarget,
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    env::lowering::FnLoweringContext,
    log::hmir_error,
    lower::{
        HMIRValue, constant,
        dispatch::lower_expression,
        emit,
        types::{self, mir_float, mir_int},
    },
};

pub(crate) fn lower_coerce(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
) -> CXResult<HMIRValue> {
    let value = lower_expression(context, body, body.expr(value))?;
    if emit::terminated(context.builder()) {
        return Ok(value);
    }

    if mode == HMIRCoerceMode::Truthy {
        return truthy(context, value, span);
    }

    let target = context.env_mut().eval_type(body, target)?;
    convert(context, value, target, span)
}

pub(crate) fn truthy(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();
    let kind = context.env().resolve_type_id(ty).kind().clone();

    let zero = match kind {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => return Ok(value),
        HMIRTypeKind::Int(_) => HMIRConstant::Int { value: 0, ty },
        HMIRTypeKind::Float(_) => HMIRConstant::Float {
            value: FloatWrapper::from(0.0),
            ty,
        },
        HMIRTypeKind::PointerTo(_) => HMIRConstant::Null(ty),
        HMIRTypeKind::ReferenceTo(_) => todo!("truthiness of a reference"),
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_) => {
            todo!("truthiness of an array, string or function")
        }
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::NO_TRUTH_VALUE,
                todo!("type display"),
            ));
        }
    };

    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let lhs = context.materialize(&value, span)?;
    let rhs = context.materialize(&HMIRValue::constant(ty, zero), span)?;
    let register = emit::register(context, bool_ty, span)?;
    let out = MIRTarget::Register(register);

    let intrinsic: MIRIntrinsic = match kind {
        HMIRTypeKind::Float(_) => MIRFloatIntrinsic::Neq { out, lhs, rhs }.into(),
        HMIRTypeKind::PointerTo(_) => MIRPtrIntrinsic::Neq { out, lhs, rhs }.into(),
        _ => MIRIntIntrinsic::Neq { out, lhs, rhs }.into(),
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    Ok(HMIRValue::register(bool_ty, register))
}

pub(crate) fn convert(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    if context.env().same_type(source, target) {
        return Ok(value);
    }

    let kinds = (
        context.env().resolve_type_id(source).kind().clone(),
        context.env().resolve_type_id(target).kind().clone(),
    );

    match &kinds {
        (_, HMIRTypeKind::Void) => return Ok(constant::unit(context, span)),

        (
            HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_),
            HMIRTypeKind::Int(to),
        ) if to.width() == HMIRIntWidth::I1 => {
            return Ok(truthy(context, value, span)?.with_ty(target));
        }

        (HMIRTypeKind::ReferenceTo(_), _) | (_, HMIRTypeKind::ReferenceTo(_)) => {
            todo!("conversion involving a reference: needs an indirect place value")
        }
        (HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_), _)
        | (_, HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_)) => {
            todo!("conversion involving an array, string or function")
        }

        _ => {}
    }

    let source_mir = context.env_mut().lower_type(source, span)?;
    let target_mir = context.env_mut().lower_type(target, span)?;
    if source_mir == target_mir {
        return Ok(emit::read(context, value, span)?.with_ty(target));
    }

    let operand = context.materialize(&value, span)?;
    let register = emit::register(context, target, span)?;
    let out = MIRTarget::Register(register);

    let intrinsic: MIRIntrinsic = match kinds {
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) => MIRIntIntrinsic::IntCast {
            out,
            value: operand,
            target: mir_int(to.width()),
            sign_extend: from.signed() && from.width() != HMIRIntWidth::I1,
        }
        .into(),
        (HMIRTypeKind::Int(from), HMIRTypeKind::Float(to)) => MIRIntIntrinsic::ToFloat {
            out,
            value: operand,
            target: mir_float(to.width()),
            signed: from.signed(),
        }
        .into(),
        (HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)) => MIRIntIntrinsic::ToPtr {
            out,
            value: operand,
            sign_extend: from.signed(),
        }
        .into(),

        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(to)) => MIRFloatIntrinsic::ToInt {
            out,
            value: operand,
            target_ty: target_mir,
            signed: to.signed(),
        }
        .into(),
        (HMIRTypeKind::Float(_), HMIRTypeKind::Float(to)) => MIRFloatIntrinsic::FloatCast {
            out,
            value: operand,
            float_ty: mir_float(to.width()),
        }
        .into(),

        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => MIRPtrIntrinsic::ToInt {
            out,
            ptr: operand,
            target_ty: target_mir,
        }
        .into(),
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => MIRInternalIntrinsic::Bitcast {
            out,
            value: operand,
            target_ty: target_mir,
        }
        .into(),

        _ => {
            return Err(hmir_error(
                span,
                &typecheck::INVALID_CONVERSION,
                (todo!("type display"), todo!("type display")),
            ));
        }
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    Ok(HMIRValue::register(target, register))
}
