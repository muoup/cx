use cx_hmir::{
    HMIRBody, HMIRCoerceMode,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::HMIRTypeID,
};
use cx_log::{
    CXResult,
    catalogue::typecheck,
};
use cx_mir::{
    MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic,
    MIRTarget,
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{
        HMIRValue,
        dispatch::lower_expression,
        emit::{diverges, emit_intrinsic, new_register, rvalue, void_value},
        shape::{TypeShape, bool_type, describe, is_bool, mir_float, mir_int, shape},
    },
    log::hmir_error,
};

pub(crate) fn lower_coerce(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
) -> CXResult<HMIRValue> {
    let value = lower_expression(cx, body, body.expr(value))?;
    if diverges(cx, &value) {
        return Ok(value);
    }

    if mode == HMIRCoerceMode::Truthy {
        return truthy(cx, value, span);
    }

    let target = cx.env().eval_type(body, target)?;
    convert(cx, value, target, span)
}

pub(crate) fn retype(
    cx: &mut FnLoweringContext,
    value: HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    Ok(rvalue(cx, value, span)?.with_ty(ty))
}

pub(crate) fn truthy(
    cx: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();
    let bool_ty = bool_type(cx, span);

    let class = shape(cx, ty);
    let zero = match class {
        TypeShape::Int(int) if is_bool(int) => return Ok(value),
        TypeShape::Int(_) => HMIRConstant::Int { value: 0, ty },
        TypeShape::Float(_) => HMIRConstant::Float {
            value: FloatWrapper::from(0.0),
            ty,
        },
        TypeShape::Pointer(_) => HMIRConstant::Null(ty),
        TypeShape::Decays => todo!("truthiness of an array, string or function"),
        TypeShape::Reference => todo!("truthiness of a reference"),
        _ => {
            let found = describe(cx, ty);
            return Err(hmir_error(span, &typecheck::NO_TRUTH_VALUE, found));
        }
    };

    let lhs = cx.materialize(&value, span)?;
    let rhs = cx.materialize(&HMIRValue::constant(ty, zero), span)?;
    let register = new_register(cx, bool_ty, span)?;

    let out = MIRTarget::Register(register);
    let intrinsic: MIRIntrinsic = match class {
        TypeShape::Float(_) => MIRFloatIntrinsic::Neq { out, lhs, rhs }.into(),
        TypeShape::Pointer(_) => MIRPtrIntrinsic::Neq { out, lhs, rhs }.into(),
        _ => MIRIntIntrinsic::Neq { out, lhs, rhs }.into(),
    };
    emit_intrinsic(cx, intrinsic, span);

    Ok(HMIRValue::register(bool_ty, register))
}

pub(crate) fn convert(
    cx: &mut FnLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    if cx.env().same_type(source, target) {
        return Ok(value);
    }

    let source_shape = shape(cx, source);
    let target_shape = shape(cx, target);

    match (source_shape, target_shape) {
        (TypeShape::Unreachable, _) => Ok(value),
        (_, TypeShape::Void) => Ok(void_value(cx, span)),

        (TypeShape::Int(from), TypeShape::Int(to)) => {
            if from.width() == to.width() {
                return retype(cx, value, target, span);
            }

            let operand = cx.materialize(&value, span)?;
            let out = new_register(cx, target, span)?;
            let out_target = MIRTarget::Register(out);

            if is_bool(to) {
                let zero = HMIRValue::constant(source, HMIRConstant::Int { value: 0, ty: source });
                let rhs = cx.materialize(&zero, span)?;
                let intrinsic = MIRIntIntrinsic::Neq {
                    out: out_target,
                    lhs: operand,
                    rhs,
                };
                emit_intrinsic(cx, intrinsic, span);
            } else {
                let intrinsic = MIRIntIntrinsic::IntCast {
                    out: out_target,
                    value: operand,
                    target: mir_int(to.width()),
                    sign_extend: from.signed() && !is_bool(from),
                };
                emit_intrinsic(cx, intrinsic, span);
            }

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Int(from), TypeShape::Float(to)) => {
            let operand = cx.materialize(&value, span)?;
            let out = new_register(cx, target, span)?;
            let intrinsic = MIRIntIntrinsic::ToFloat {
                out: MIRTarget::Register(out),
                value: operand,
                target: mir_float(to.width()),
                signed: from.signed(),
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Float(_), TypeShape::Int(to)) => {
            if is_bool(to) {
                return truthy(cx, value, span);
            }

            let operand = cx.materialize(&value, span)?;
            let target_ty = cx.env().lower_type(target, span)?;
            let out = new_register(cx, target, span)?;
            let intrinsic = MIRFloatIntrinsic::ToInt {
                out: MIRTarget::Register(out),
                value: operand,
                target_ty,
                signed: to.signed(),
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Float(from), TypeShape::Float(to)) => {
            if from.width() == to.width() {
                return retype(cx, value, target, span);
            }

            let operand = cx.materialize(&value, span)?;
            let out = new_register(cx, target, span)?;
            let intrinsic = MIRFloatIntrinsic::FloatCast {
                out: MIRTarget::Register(out),
                value: operand,
                float_ty: mir_float(to.width()),
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Pointer(_), TypeShape::Int(to)) => {
            if is_bool(to) {
                return truthy(cx, value, span);
            }

            let ptr = cx.materialize(&value, span)?;
            let target_ty = cx.env().lower_type(target, span)?;
            let out = new_register(cx, target, span)?;
            let intrinsic = MIRPtrIntrinsic::ToInt {
                out: MIRTarget::Register(out),
                ptr,
                target_ty,
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Int(from), TypeShape::Pointer(_)) => {
            let operand = cx.materialize(&value, span)?;
            let out = new_register(cx, target, span)?;
            let intrinsic = MIRIntIntrinsic::ToPtr {
                out: MIRTarget::Register(out),
                value: operand,
                sign_extend: from.signed(),
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Pointer(_), TypeShape::Pointer(_)) => {
            let source_mir = cx.env().lower_type(source, span)?;
            let target_mir = cx.env().lower_type(target, span)?;
            if source_mir == target_mir {
                return retype(cx, value, target, span);
            }

            let operand = cx.materialize(&value, span)?;
            let out = new_register(cx, target, span)?;
            let intrinsic = MIRInternalIntrinsic::Bitcast {
                out: MIRTarget::Register(out),
                value: operand,
                target_ty: target_mir,
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(target, out))
        }

        (TypeShape::Reference, _) | (_, TypeShape::Reference) => {
            todo!("conversion involving a reference: needs an indirect place value")
        }
        (TypeShape::Decays, _) | (_, TypeShape::Decays) => {
            todo!("conversion involving an array, string or function")
        }

        _ => {
            let source_mir = cx.env().lower_type(source, span)?;
            let target_mir = cx.env().lower_type(target, span)?;
            if source_mir == target_mir {
                return retype(cx, value, target, span);
            }

            let from = format!("'{}'", describe(cx, source));
            let to = describe(cx, target);
            Err(hmir_error(span, &typecheck::INVALID_CONVERSION, (from, to)))
        }
    }
}
