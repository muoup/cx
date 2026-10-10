use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRUnaryOp,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeID, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRBlockTarget, MIRFloatIntrinsic, MIRInstructionKind, MIRIntIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{
        HMIRValue, HMIRValueKind,
        coerce::{convert, truthy},
        dispatch::lower_expression,
        emit::{
            diverges, emit_branch, emit_intrinsic, emit_jump, new_block, new_block_param,
            new_register, rvalue,
        },
        shape::{
            TypeShape, arithmetic_type, bool_type, describe, int_type, promote_type, shape,
            size_of,
        },
    },
    log::hmir_error,
};

fn is_comparison(op: HMIRBinaryOp) -> bool {
    matches!(
        op,
        HMIRBinaryOp::Eq
            | HMIRBinaryOp::Neq
            | HMIRBinaryOp::Lt
            | HMIRBinaryOp::Le
            | HMIRBinaryOp::Gt
            | HMIRBinaryOp::Ge
    )
}

fn promote(
    cx: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match shape(cx, value.ty()) {
        TypeShape::Reference => todo!("reference operand: needs an indirect place value"),
        TypeShape::Decays => todo!("array, string or function operand decay"),
        _ => {}
    }

    let promoted = promote_type(cx, value.ty(), span);
    convert(cx, value, promoted, span)
}

fn lower_operand(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    expr: HMIRExprID,
) -> CXResult<Result<HMIRValue, HMIRValue>> {
    let value = lower_expression(cx, body, body.expr(expr))?;
    if diverges(cx, &value) {
        return Ok(Err(value));
    }

    Ok(Ok(promote(cx, value, span)?))
}

pub(crate) fn lower_binary(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> CXResult<HMIRValue> {
    if matches!(op, HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr) {
        return lower_short_circuit(cx, body, span, op, lhs, rhs);
    }

    let lhs = match lower_operand(cx, body, span, lhs)? {
        Ok(value) => value,
        Err(diverged) => return Ok(diverged),
    };
    let rhs = match lower_operand(cx, body, span, rhs)? {
        Ok(value) => value,
        Err(diverged) => return Ok(diverged),
    };

    match (shape(cx, lhs.ty()), shape(cx, rhs.ty())) {
        (TypeShape::Pointer(element), TypeShape::Int(_))
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            let subtract = op == HMIRBinaryOp::Sub;
            lower_pointer_offset(cx, span, lhs, rhs, element, subtract)
        }
        (TypeShape::Int(_), TypeShape::Pointer(element)) if op == HMIRBinaryOp::Add => {
            lower_pointer_offset(cx, span, rhs, lhs, element, false)
        }
        (TypeShape::Pointer(element), TypeShape::Pointer(_)) if op == HMIRBinaryOp::Sub => {
            let element_ty = cx.env().lower_type(element, span)?;
            let ty = int_type(cx, HMIRIntWidth::I64, true, span);
            let lhs = cx.materialize(&lhs, span)?;
            let rhs = cx.materialize(&rhs, span)?;
            let out = new_register(cx, ty, span)?;
            let intrinsic = MIRPtrIntrinsic::Diff {
                out: MIRTarget::Register(out),
                lhs,
                rhs,
                element_ty,
            };
            emit_intrinsic(cx, intrinsic, span);

            Ok(HMIRValue::register(ty, out))
        }
        (TypeShape::Pointer(_), _) | (_, TypeShape::Pointer(_)) if is_comparison(op) => {
            let (lhs, rhs) = if matches!(shape(cx, lhs.ty()), TypeShape::Pointer(_)) {
                let ty = lhs.ty();
                let rhs = convert(cx, rhs, ty, span)?;
                (lhs, rhs)
            } else {
                let ty = rhs.ty();
                (convert(cx, lhs, ty, span)?, rhs)
            };

            lower_pointer_comparison(cx, span, op, lhs, rhs)
        }
        _ => lower_arithmetic(cx, span, op, lhs, rhs),
    }
}

fn lower_arithmetic(
    cx: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    let common = match op {
        HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => {
            arithmetic_type(cx, lhs.ty(), lhs.ty(), span)
        }
        _ => arithmetic_type(cx, lhs.ty(), rhs.ty(), span),
    };
    let Some(common) = common else {
        let left = describe(cx, lhs.ty());
        let right = describe(cx, rhs.ty());
        return Err(hmir_error(
            span,
            &typecheck::INVALID_BINARY_OPERANDS,
            (op.path().into(), left, right),
        ));
    };

    let lhs = convert(cx, lhs, common, span)?;
    let rhs = convert(cx, rhs, common, span)?;
    let lhs = cx.materialize(&lhs, span)?;
    let rhs = cx.materialize(&rhs, span)?;

    let result = if is_comparison(op) {
        bool_type(cx, span)
    } else {
        common
    };
    let out = new_register(cx, result, span)?;
    let out_target = MIRTarget::Register(out);

    let intrinsic: MIRIntrinsic = match shape(cx, common) {
        TypeShape::Float(_) => {
            let (out, lhs, rhs) = (out_target, lhs, rhs);
            match op {
                HMIRBinaryOp::Add => MIRFloatIntrinsic::Add { out, lhs, rhs },
                HMIRBinaryOp::Sub => MIRFloatIntrinsic::Sub { out, lhs, rhs },
                HMIRBinaryOp::Mul => MIRFloatIntrinsic::Mul { out, lhs, rhs },
                HMIRBinaryOp::Div => MIRFloatIntrinsic::Div { out, lhs, rhs },
                HMIRBinaryOp::Eq => MIRFloatIntrinsic::Eq { out, lhs, rhs },
                HMIRBinaryOp::Neq => MIRFloatIntrinsic::Neq { out, lhs, rhs },
                HMIRBinaryOp::Lt => MIRFloatIntrinsic::Lt { out, lhs, rhs },
                HMIRBinaryOp::Le => MIRFloatIntrinsic::Le { out, lhs, rhs },
                HMIRBinaryOp::Gt => MIRFloatIntrinsic::Gt { out, lhs, rhs },
                HMIRBinaryOp::Ge => MIRFloatIntrinsic::Geq { out, lhs, rhs },
                _ => {
                    return Err(hmir_error(
                        span,
                        &typecheck::FLOATING_OPERAND,
                        op.path().into(),
                    ));
                }
            }
            .into()
        }
        TypeShape::Int(int) => {
            let (out, lhs, rhs) = (out_target, lhs, rhs);
            let signed = int.signed();
            match op {
                HMIRBinaryOp::Add => MIRIntIntrinsic::Add { out, lhs, rhs },
                HMIRBinaryOp::Sub => MIRIntIntrinsic::Sub { out, lhs, rhs },
                HMIRBinaryOp::Mul if signed => MIRIntIntrinsic::SMul { out, lhs, rhs },
                HMIRBinaryOp::Mul => MIRIntIntrinsic::UMul { out, lhs, rhs },
                HMIRBinaryOp::Div if signed => MIRIntIntrinsic::SDiv { out, lhs, rhs },
                HMIRBinaryOp::Div => MIRIntIntrinsic::UDiv { out, lhs, rhs },
                HMIRBinaryOp::Mod if signed => MIRIntIntrinsic::SMod { out, lhs, rhs },
                HMIRBinaryOp::Mod => MIRIntIntrinsic::UMod { out, lhs, rhs },
                HMIRBinaryOp::Eq => MIRIntIntrinsic::Eq { out, lhs, rhs },
                HMIRBinaryOp::Neq => MIRIntIntrinsic::Neq { out, lhs, rhs },
                HMIRBinaryOp::Lt if signed => MIRIntIntrinsic::SLt { out, lhs, rhs },
                HMIRBinaryOp::Lt => MIRIntIntrinsic::ULt { out, lhs, rhs },
                HMIRBinaryOp::Le if signed => MIRIntIntrinsic::SLe { out, lhs, rhs },
                HMIRBinaryOp::Le => MIRIntIntrinsic::ULe { out, lhs, rhs },
                HMIRBinaryOp::Gt if signed => MIRIntIntrinsic::SGt { out, lhs, rhs },
                HMIRBinaryOp::Gt => MIRIntIntrinsic::UGt { out, lhs, rhs },
                HMIRBinaryOp::Ge if signed => MIRIntIntrinsic::SGe { out, lhs, rhs },
                HMIRBinaryOp::Ge => MIRIntIntrinsic::UGe { out, lhs, rhs },
                HMIRBinaryOp::BAnd => MIRIntIntrinsic::BAnd { out, lhs, rhs },
                HMIRBinaryOp::BOr => MIRIntIntrinsic::BOr { out, lhs, rhs },
                HMIRBinaryOp::BXor => MIRIntIntrinsic::BXor { out, lhs, rhs },
                HMIRBinaryOp::LShift => MIRIntIntrinsic::LShift { out, lhs, rhs },
                HMIRBinaryOp::RShift if signed => MIRIntIntrinsic::ARShift { out, lhs, rhs },
                HMIRBinaryOp::RShift => MIRIntIntrinsic::LRShift { out, lhs, rhs },
                HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                    unreachable!("logical operators are lowered as short circuits")
                }
            }
            .into()
        }
        _ => unreachable!("arithmetic types are integral or floating"),
    };

    emit_intrinsic(cx, intrinsic, span);
    Ok(HMIRValue::register(result, out))
}

fn lower_pointer_comparison(
    cx: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    let lhs = cx.materialize(&lhs, span)?;
    let rhs = cx.materialize(&rhs, span)?;
    let bool_ty = bool_type(cx, span);
    let register = new_register(cx, bool_ty, span)?;

    let (out, lhs, rhs) = (MIRTarget::Register(register), lhs, rhs);
    let intrinsic = match op {
        HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq { out, lhs, rhs },
        HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq { out, lhs, rhs },
        HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt { out, lhs, rhs },
        HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq { out, lhs, rhs },
        HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt { out, lhs, rhs },
        HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq { out, lhs, rhs },
        _ => unreachable!("pointer comparisons are comparisons"),
    };

    emit_intrinsic(cx, intrinsic, span);
    Ok(HMIRValue::register(bool_ty, register))
}

fn lower_pointer_offset(
    cx: &mut FnLoweringContext,
    span: &TokenRange,
    pointer: HMIRValue,
    index: HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
) -> CXResult<HMIRValue> {
    let result = pointer.ty();
    let size = size_of(cx, element) as i128;
    let signed = matches!(shape(cx, index.ty()), TypeShape::Int(int) if int.signed());
    let offset_ty = int_type(cx, HMIRIntWidth::I64, signed, span);

    let index = convert(cx, index, offset_ty, span)?;
    let index = cx.materialize(&index, span)?;
    let pointer = cx.materialize(&pointer, span)?;
    let size = HMIRValue::constant(
        offset_ty,
        HMIRConstant::Int {
            value: size,
            ty: offset_ty,
        },
    );
    let size = cx.materialize(&size, span)?;

    let scaled = new_register(cx, offset_ty, span)?;
    let scaled_target = MIRTarget::Register(scaled);
    if signed {
        let intrinsic = MIRIntIntrinsic::SMul {
            out: scaled_target,
            lhs: index,
            rhs: size,
        };
        emit_intrinsic(cx, intrinsic, span);
    } else {
        let intrinsic = MIRIntIntrinsic::UMul {
            out: scaled_target,
            lhs: index,
            rhs: size,
        };
        emit_intrinsic(cx, intrinsic, span);
    }

    let out = new_register(cx, result, span)?;
    let (out_target, offset) = (MIRTarget::Register(out), MIRValue::Register(scaled));
    if subtract {
        let intrinsic = MIRPtrIntrinsic::Sub {
            out: out_target,
            ptr: pointer,
            offset,
        };
        emit_intrinsic(cx, intrinsic, span);
    } else {
        let intrinsic = MIRPtrIntrinsic::Add {
            out: out_target,
            ptr: pointer,
            offset,
        };
        emit_intrinsic(cx, intrinsic, span);
    }

    Ok(HMIRValue::register(result, out))
}

fn lower_short_circuit(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> CXResult<HMIRValue> {
    let left = lower_expression(cx, body, body.expr(lhs))?;
    if diverges(cx, &left) {
        return Ok(left);
    }

    let left = truthy(cx, left, span)?;
    let left = rvalue(cx, left, span)?;
    let left = cx.materialize(&left, span)?;

    let bool_ty = bool_type(cx, span);
    let rhs_block = new_block(cx, "logical.rhs");
    let merge = new_block(cx, "logical.merge");
    let result = new_block_param(cx, merge, bool_ty, span)?;

    let rhs_target = MIRBlockTarget::new(rhs_block);
    let merge_target = MIRBlockTarget::with_args(merge, vec![left.clone()]);
    let (true_target, false_target) = match op {
        HMIRBinaryOp::LAnd => (rhs_target, merge_target),
        _ => (merge_target, rhs_target),
    };
    emit_branch(cx, left, true_target, false_target, span);

    cx.builder().switch_to(rhs_block);
    let right = lower_expression(cx, body, body.expr(rhs))?;
    if !diverges(cx, &right) {
        let right = truthy(cx, right, span)?;
        let right = cx.materialize(&right, span)?;
        emit_jump(cx, merge, vec![right], span);
    }

    cx.builder().switch_to(merge);
    Ok(result)
}

pub(crate) fn lower_unary(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
) -> CXResult<HMIRValue> {
    let value = lower_expression(cx, body, body.expr(operand))?;
    if diverges(cx, &value) {
        return Ok(value);
    }

    match op {
        HMIRUnaryOp::PreIncrement => return lower_increment(cx, span, value, 1, true),
        HMIRUnaryOp::PreDecrement => return lower_increment(cx, span, value, -1, true),
        HMIRUnaryOp::PostIncrement => return lower_increment(cx, span, value, 1, false),
        HMIRUnaryOp::PostDecrement => return lower_increment(cx, span, value, -1, false),
        _ => {}
    }

    if op == HMIRUnaryOp::LNot {
        let value = truthy(cx, value, span)?;
        let ty = value.ty();
        let operand = cx.materialize(&value, span)?;
        let out = new_register(cx, ty, span)?;
        let intrinsic = MIRIntIntrinsic::LNot {
            out: MIRTarget::Register(out),
            value: operand,
        };
        emit_intrinsic(cx, intrinsic, span);

        return Ok(HMIRValue::register(ty, out));
    }

    let value = promote(cx, value, span)?;
    let ty = value.ty();
    let operand = cx.materialize(&value, span)?;
    let out = new_register(cx, ty, span)?;
    let out_target = MIRTarget::Register(out);

    let intrinsic: MIRIntrinsic = match (op, shape(cx, ty)) {
        (HMIRUnaryOp::Neg, TypeShape::Float(_)) => MIRFloatIntrinsic::Neg {
            out: out_target,
            value: operand,
        }
        .into(),
        (HMIRUnaryOp::Neg, TypeShape::Int(_)) => MIRIntIntrinsic::Neg {
            out: out_target,
            value: operand,
        }
        .into(),
        (HMIRUnaryOp::BNot, TypeShape::Int(_)) => MIRIntIntrinsic::BNot {
            out: out_target,
            value: operand,
        }
        .into(),
        (_, TypeShape::Float(_)) => {
            return Err(hmir_error(
                span,
                &typecheck::FLOATING_OPERAND,
                op.path().into(),
            ));
        }
        _ => {
            let found = describe(cx, ty);
            return Err(hmir_error(
                span,
                &typecheck::INVALID_OPERAND,
                (format!("apply '{}' to", op.path()), found),
            ));
        }
    };

    emit_intrinsic(cx, intrinsic, span);
    Ok(HMIRValue::register(ty, out))
}

fn lower_increment(
    cx: &mut FnLoweringContext,
    span: &TokenRange,
    target: HMIRValue,
    amount: i128,
    prefix: bool,
) -> CXResult<HMIRValue> {
    let HMIRValueKind::Place(place) = *target.kind() else {
        return Err(hmir_error(
            span,
            &typecheck::NOT_ADDRESSABLE,
            "increment".into(),
        ));
    };

    let ty = target.ty();
    let current = rvalue(cx, target.clone(), span)?;
    let lhs = cx.materialize(&current, span)?;
    let out = new_register(cx, ty, span)?;
    let out_target = MIRTarget::Register(out);
    let subtract = amount < 0;

    match shape(cx, ty) {
        TypeShape::Int(_) => {
            let step = HMIRValue::constant(ty, HMIRConstant::Int { value: 1, ty });
            let rhs = cx.materialize(&step, span)?;
            if subtract {
                let intrinsic = MIRIntIntrinsic::Sub {
                    out: out_target,
                    lhs,
                    rhs,
                };
                emit_intrinsic(cx, intrinsic, span);
            } else {
                let intrinsic = MIRIntIntrinsic::Add {
                    out: out_target,
                    lhs,
                    rhs,
                };
                emit_intrinsic(cx, intrinsic, span);
            }
        }
        TypeShape::Float(_) => {
            let one = HMIRConstant::Float {
                value: FloatWrapper::from(1.0),
                ty,
            };
            let rhs = cx.materialize(&HMIRValue::constant(ty, one), span)?;
            if subtract {
                let intrinsic = MIRFloatIntrinsic::Sub {
                    out: out_target,
                    lhs,
                    rhs,
                };
                emit_intrinsic(cx, intrinsic, span);
            } else {
                let intrinsic = MIRFloatIntrinsic::Add {
                    out: out_target,
                    lhs,
                    rhs,
                };
                emit_intrinsic(cx, intrinsic, span);
            }
        }
        TypeShape::Pointer(element) => {
            let size = size_of(cx, element) as i128;
            let offset_ty = int_type(cx, HMIRIntWidth::I64, true, span);
            let step = HMIRConstant::Int {
                value: size,
                ty: offset_ty,
            };
            let offset = cx.materialize(&HMIRValue::constant(offset_ty, step), span)?;
            if subtract {
                let intrinsic = MIRPtrIntrinsic::Sub {
                    out: out_target,
                    ptr: lhs,
                    offset,
                };
                emit_intrinsic(cx, intrinsic, span);
            } else {
                let intrinsic = MIRPtrIntrinsic::Add {
                    out: out_target,
                    ptr: lhs,
                    offset,
                };
                emit_intrinsic(cx, intrinsic, span);
            }
        }
        TypeShape::Reference => todo!("increment of a reference: needs an indirect place value"),
        _ => {
            let found = describe(cx, ty);
            return Err(hmir_error(
                span,
                &typecheck::INVALID_OPERAND,
                ("increment".into(), found),
            ));
        }
    }

    let mir = cx.env().lower_type(ty, span)?;
    cx.builder().emit(
        MIRInstructionKind::Store {
            target: MIRTarget::Place(place),
            value: MIRValue::Register(out),
            ty: mir,
            bitfield: None,
        },
        span,
    );

    if prefix { Ok(target) } else { Ok(current) }
}
