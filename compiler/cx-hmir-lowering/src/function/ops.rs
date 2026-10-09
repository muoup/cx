use cx_hmir::{HMIRBinaryOp, HMIRExprID, HMIRIntWidth, HMIRUnaryOp};
use cx_log::catalogue::typecheck;
use cx_mir::{
    MIRBindable, MIRBlockTarget, MIRConstant, MIRFloatIntrinsic, MIRInstructionKind,
    MIRIntIntrinsic, MIRIntrinsic, MIRPtrIntrinsic, MIRStoreBitfield, MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    eval::ops::{fold_binary, fold_unary},
    function::{
        Expect, FunctionLowering, LowerResult, Operand, Stop,
        coerce::{lower_convert, lower_truthy},
        expr::{lower_expr, lower_static_operand},
        operand::{lower_copy, lower_int_constant, lower_store, lower_value},
        promote::lower_promote,
    },
    ty::{HMIRTypeID, HMIRTypeKind, TypeTable},
    value::{arithmetic_type, is_comparison, is_logical},
};

pub(super) fn lower_binary(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if is_logical(op) {
        return lower_short_circuit(cx, frame, op, lhs, rhs, span);
    }
    let lhs = lower_expr(cx, frame, lhs, Expect::Any)?;
    let lhs = lower_promote(cx, lhs, span)?;
    let rhs = lower_expr(cx, frame, rhs, Expect::Any)?;
    let rhs = lower_promote(cx, rhs, span)?;
    lower_binary_operands(cx, op, lhs, rhs, span)
}

fn lower_binary_operands(
    cx: &mut FunctionLowering<'_, '_>,
    op: HMIRBinaryOp,
    lhs: Operand,
    rhs: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if (!cx.unevaluated || matches!(cx.program.types().kind(lhs.ty()), HMIRTypeKind::Type))
        && let (Some(left), Some(right)) = (lhs.as_static(), rhs.as_static())
        && let Ok(value) = fold_binary(cx.program, op, left.clone(), right.clone(), span)
    {
        return lower_static_operand(cx, value, span);
    }
    let left = cx.program.types().kind(lhs.ty()).clone();
    let right = cx.program.types().kind(rhs.ty()).clone();
    match (&left, &right) {
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::Int { .. })
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            let ty = lhs.ty();
            lower_pointer_offset(cx, lhs, rhs, *element, op == HMIRBinaryOp::Sub, ty, span)
        }
        (HMIRTypeKind::Int { .. }, HMIRTypeKind::PointerTo(element)) if op == HMIRBinaryOp::Add => {
            let ty = rhs.ty();
            lower_pointer_offset(cx, rhs, lhs, *element, false, ty, span)
        }
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::PointerTo(_))
            if op == HMIRBinaryOp::Sub =>
        {
            let element_ty = cx.mir(*element, span)?;
            let ty = cx.program.types_mut().int(HMIRIntWidth::I64, true);
            let lhs = lower_value(cx, lhs, span)?;
            let rhs = lower_value(cx, rhs, span)?;
            let out = cx.register(ty, span)?;
            cx.intrinsic(
                MIRPtrIntrinsic::Diff {
                    out: MIRTarget::Register(out),
                    lhs,
                    rhs,
                    element_ty,
                },
                span,
            );
            Ok(Operand::register(out, ty))
        }
        (HMIRTypeKind::PointerTo(_), _) | (_, HMIRTypeKind::PointerTo(_)) if is_comparison(op) => {
            let (lhs, rhs) = if matches!(left, HMIRTypeKind::PointerTo(_)) {
                let ty = lhs.ty();
                let rhs = lower_convert(cx, rhs, ty, span)?;
                (lhs, rhs)
            } else {
                let ty = rhs.ty();
                (lower_convert(cx, lhs, ty, span)?, rhs)
            };
            lower_pointer_comparison(cx, op, lhs, rhs, span)
        }
        _ => lower_arithmetic(cx, op, lhs, rhs, span),
    }
}

fn lower_arithmetic(
    cx: &mut FunctionLowering<'_, '_>,
    op: HMIRBinaryOp,
    lhs: Operand,
    rhs: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let types = cx.program.types_mut();
    let common = match op {
        HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => arithmetic_type(types, lhs.ty(), lhs.ty()),
        _ => arithmetic_type(types, lhs.ty(), rhs.ty()),
    };
    let Some(common) = common else {
        return cx.error(
            span,
            &typecheck::INVALID_BINARY_OPERANDS,
            (
                op.path().into(),
                cx.program.types().display(lhs.ty()),
                cx.program.types().display(rhs.ty()),
            ),
        );
    };
    let lhs = lower_convert(cx, lhs, common, span)?;
    let rhs = lower_convert(cx, rhs, common, span)?;
    let lhs = lower_value(cx, lhs, span)?;
    let rhs = lower_value(cx, rhs, span)?;
    let result = if is_comparison(op) {
        cx.program.types_mut().bool()
    } else {
        common
    };
    let out = cx.register(result, span)?;
    let out_target = MIRTarget::Register(out);
    let intrinsic: MIRIntrinsic = match cx.program.types().kind(common).clone() {
        HMIRTypeKind::Float { .. } => {
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
                    return cx.error(span, &typecheck::FLOATING_OPERAND, op.path().into());
                }
            }
            .into()
        }
        HMIRTypeKind::Int { signed, .. } => {
            let (out, lhs, rhs) = (out_target, lhs, rhs);
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
                    unreachable!("logical operators short-circuit")
                }
            }
            .into()
        }
        _ => unreachable!("arithmetic types are integral or floating"),
    };
    cx.intrinsic(intrinsic, span);
    Ok(Operand::register(out, result))
}

fn lower_pointer_comparison(
    cx: &mut FunctionLowering<'_, '_>,
    op: HMIRBinaryOp,
    lhs: Operand,
    rhs: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let lhs = lower_value(cx, lhs, span)?;
    let rhs = lower_value(cx, rhs, span)?;
    let bool = cx.program.types_mut().bool();
    let out = MIRTarget::Register(cx.register(bool, span)?);
    let intrinsic = match op {
        HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq { out, lhs, rhs },
        HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq { out, lhs, rhs },
        HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt { out, lhs, rhs },
        HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq { out, lhs, rhs },
        HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt { out, lhs, rhs },
        HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq { out, lhs, rhs },
        _ => unreachable!("pointer comparisons are comparisons"),
    };
    let MIRTarget::Register(register) = out else {
        unreachable!()
    };
    cx.intrinsic(intrinsic, span);
    Ok(Operand::register(register, bool))
}

// 'pointer' displaced by 'index' elements, typed 'result'
pub(super) fn lower_pointer_offset(
    cx: &mut FunctionLowering<'_, '_>,
    pointer: Operand,
    index: Operand,
    element: HMIRTypeID,
    subtract: bool,
    result: HMIRTypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let origin = pointer.pointee_origin();
    let size = if cx.program.types().is_void(element) {
        1
    } else {
        cx.program.types_mut().size_of(element, span)?
    };
    let signed = cx.program.types().is_signed(index.ty());
    let offset_ty = cx.program.types_mut().int(HMIRIntWidth::I64, signed);
    let index = lower_convert(cx, index, offset_ty, span)?;
    let index = lower_value(cx, index, span)?;
    let pointer = lower_value(cx, pointer, span)?;
    let scaled = cx.register(offset_ty, span)?;
    let size = lower_int_constant(cx, size as i128, offset_ty);
    cx.intrinsic(
        MIRIntIntrinsic::SMul {
            out: MIRTarget::Register(scaled),
            lhs: index,
            rhs: size,
        },
        span,
    );
    let out = cx.register(result, span)?;
    let (out_target, offset) = (MIRTarget::Register(out), MIRValue::Register(scaled));
    if subtract {
        cx.intrinsic(
            MIRPtrIntrinsic::Sub {
                out: out_target,
                ptr: pointer,
                offset,
            },
            span,
        );
    } else {
        cx.intrinsic(
            MIRPtrIntrinsic::Add {
                out: out_target,
                ptr: pointer,
                offset,
            },
            span,
        );
    }
    Ok(Operand::register(out, result).with_pointee_origin(origin))
}

fn lower_short_circuit(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let bool = cx.program.types_mut().bool();
    let left = lower_expr(cx, frame, lhs, Expect::Any)?;
    let left = lower_truthy(cx, left, span)?;
    let left = lower_value(cx, left, span)?;

    let rhs_block = cx.new_block("logical.rhs");
    let merge = cx.new_block("logical.merge");
    let mir_bool = cx.mir(bool, span)?;
    let result = cx.body.add_block_param(merge, mir_bool, None);
    let rhs_target = MIRBlockTarget::new(rhs_block);
    let merge_target = MIRBlockTarget::with_args(merge, vec![left.clone()]);
    let (true_target, false_target) = match op {
        HMIRBinaryOp::LAnd => (rhs_target, merge_target),
        _ => (merge_target, rhs_target),
    };
    cx.emit(
        MIRInstructionKind::Branch {
            cond: left,
            true_target,
            false_target,
        },
        span,
    );

    cx.set_block(rhs_block);
    let right = lower_expr(cx, frame, rhs, Expect::Any)
        .and_then(|right| lower_truthy(cx, right, span))
        .and_then(|right| lower_value(cx, right, span));
    match right {
        Ok(right) => cx.jump(merge, vec![right], span),
        Err(Stop::Diverged) => {}
        Err(error) => return Err(error),
    }
    cx.set_block(merge);
    Ok(Operand::register(result, bool))
}

pub(super) fn lower_unary(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIRUnaryOp::PreIncrement => return lower_increment(cx, frame, operand, 1, true, span),
        HMIRUnaryOp::PreDecrement => return lower_increment(cx, frame, operand, -1, true, span),
        HMIRUnaryOp::PostIncrement => return lower_increment(cx, frame, operand, 1, false, span),
        HMIRUnaryOp::PostDecrement => return lower_increment(cx, frame, operand, -1, false, span),
        _ => {}
    }
    let value = lower_expr(cx, frame, operand, Expect::Any)?;
    if !cx.unevaluated
        && let Some(value) = value.as_static()
    {
        let folded = fold_unary(cx.program, op, value.clone(), span)?;
        return lower_static_operand(cx, folded, span);
    }
    if op == HMIRUnaryOp::LNot {
        let value = lower_truthy(cx, value, span)?;
        let bool = value.ty();
        let value = lower_value(cx, value, span)?;
        let out = cx.register(bool, span)?;
        cx.intrinsic(
            MIRIntIntrinsic::LNot {
                out: MIRTarget::Register(out),
                value,
            },
            span,
        );
        return Ok(Operand::register(out, bool));
    }

    let types = cx.program.types_mut();
    let Some(ty) = arithmetic_type(types, value.ty(), value.ty()) else {
        return cx.error(
            span,
            &typecheck::INVALID_OPERAND,
            (
                format!("apply '{}' to", op.path()),
                cx.program.types().display(value.ty()),
            ),
        );
    };
    let value = lower_convert(cx, value, ty, span)?;
    let value = lower_value(cx, value, span)?;
    let out = cx.register(ty, span)?;
    let target = MIRTarget::Register(out);
    let float = matches!(cx.program.types().kind(ty), HMIRTypeKind::Float { .. });
    let intrinsic: MIRIntrinsic = match op {
        HMIRUnaryOp::Neg if float => MIRFloatIntrinsic::Neg { out: target, value }.into(),
        HMIRUnaryOp::Neg => MIRIntIntrinsic::Neg { out: target, value }.into(),
        HMIRUnaryOp::BNot if !float => MIRIntIntrinsic::BNot { out: target, value }.into(),
        _ => return cx.error(span, &typecheck::FLOATING_OPERAND, op.path().into()),
    };
    cx.intrinsic(intrinsic, span);
    Ok(Operand::register(out, ty))
}

fn lower_increment(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    operand: HMIRExprID,
    amount: i128,
    prefix: bool,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let target = lower_expr(cx, frame, operand, Expect::Any)?;
    let Some(destination) = target.target() else {
        return cx.error(span, &typecheck::NOT_ADDRESSABLE, "increment".into());
    };
    cx.require_mutable(target.ty(), "modify", span)?;
    let ty = target.ty();
    let current = lower_copy(cx, &target, span)?;
    let out = cx.register(ty, span)?;
    let out_target = MIRTarget::Register(out);
    match cx.program.types().kind(ty).clone() {
        HMIRTypeKind::Int { .. } => {
            let step = lower_int_constant(cx, amount.abs(), ty);
            let (lhs, rhs) = (current.clone(), step);
            if amount >= 0 {
                cx.intrinsic(
                    MIRIntIntrinsic::Add {
                        out: out_target,
                        lhs,
                        rhs,
                    },
                    span,
                );
            } else {
                cx.intrinsic(
                    MIRIntIntrinsic::Sub {
                        out: out_target,
                        lhs,
                        rhs,
                    },
                    span,
                );
            }
        }
        HMIRTypeKind::Float { width } => {
            let step = MIRValue::Constant(MIRConstant::Float {
                value: (amount as f64).into(),
                ty: TypeTable::mir_float(width),
            });
            cx.intrinsic(
                MIRFloatIntrinsic::Add {
                    out: out_target,
                    lhs: current.clone(),
                    rhs: step,
                },
                span,
            );
        }
        HMIRTypeKind::PointerTo(element) => {
            let size = cx.program.types_mut().size_of(element, span)?;
            let size_type = cx.program.types_mut().size_type();
            let offset = lower_int_constant(cx, amount * size as i128, size_type);
            cx.intrinsic(
                MIRPtrIntrinsic::Add {
                    out: out_target,
                    ptr: current.clone(),
                    offset,
                },
                span,
            );
        }
        _ => {
            return cx.error(
                span,
                &typecheck::INVALID_OPERAND,
                ("increment".into(), cx.program.types().display(ty)),
            );
        }
    }
    let bitfield = target.bitfield().map(MIRStoreBitfield::Target);
    lower_store(cx, destination, MIRValue::Register(out), ty, bitfield, span)?;
    if prefix {
        Ok(target)
    } else {
        Ok(Operand::value(current, ty))
    }
}

pub(super) fn lower_assign(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    target: HMIRExprID,
    op: Option<HMIRBinaryOp>,
    value: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let lhs = lower_expr(cx, frame, target, Expect::Any)?;
    let Some(destination) = lhs.target() else {
        return cx.error(span, &typecheck::NOT_ADDRESSABLE, "assign to".into());
    };
    cx.require_mutable(lhs.ty(), "assign to", span)?;
    let ty = lhs.ty();
    let value = match op {
        Some(op) => {
            let current = lower_promote(cx, lhs.clone(), span)?;
            let rhs = lower_expr(cx, frame, value, Expect::Any)?;
            let rhs = lower_promote(cx, rhs, span)?;
            lower_binary_operands(cx, op, current, rhs, span)?
        }
        None => lower_expr(cx, frame, value, Expect::Type(ty))?,
    };
    let value = lower_convert(cx, value, ty, span)?;
    let value = lower_value(cx, value, span)?;
    if let MIRTarget::Place(place) = destination {
        cx.invalidate(MIRBindable::Place(place), MIRInvalidationKind::Drop, span);
        cx.initialize(place, span);
    }
    let bitfield = lhs.bitfield().map(MIRStoreBitfield::Target);
    lower_store(cx, destination, value, ty, bitfield, span)?;
    Ok(lhs)
}
