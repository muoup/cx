use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::VAIntrinsic;
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRFloatIntrinsic, MIRInstructionKind, MIRIntIntrinsic,
    MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic, MIRTarget, MIRVAIntrinsic, MIRValue,
    ty::layout::calculate_type_layout,
};
use cx_tokens::TokenRange;

use super::{output, register, slot, storage::read};
use crate::{
    env::function::LocalLoweringContext,
    eval::types,
    lower::types::{lower_type, mir_float, mir_int},
    value::{HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_kind(ty).clone()
}

pub fn binary(
    context: &mut LocalLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let operands = kind(context, lhs.ty());
    let ml = super::value(context, lhs, span)?;
    let mr = super::value(context, rhs, span)?;
    let (out, result) = output(context, result, span)?;

    let instruction: MIRIntrinsic = match operands {
        HMIRTypeKind::Float(_) => match op {
            HMIRBinaryOp::Add => MIRFloatIntrinsic::Add {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Sub => MIRFloatIntrinsic::Sub {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Mul => MIRFloatIntrinsic::Mul {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Div => MIRFloatIntrinsic::Div {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Eq => MIRFloatIntrinsic::Eq {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Neq => MIRFloatIntrinsic::Neq {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Lt => MIRFloatIntrinsic::Lt {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Le => MIRFloatIntrinsic::Le {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Gt => MIRFloatIntrinsic::Gt {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Ge => MIRFloatIntrinsic::Geq {
                out,
                lhs: ml,
                rhs: mr,
            },
            _ => unreachable!("'{}' has no floating form", op.path()),
        }
        .into(),
        HMIRTypeKind::Int(int) => {
            let signed = int.signed();
            match op {
                HMIRBinaryOp::Add => MIRIntIntrinsic::Add {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Sub => MIRIntIntrinsic::Sub {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Mul if signed => MIRIntIntrinsic::SMul {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Mul => MIRIntIntrinsic::UMul {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Div if signed => MIRIntIntrinsic::SDiv {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Div => MIRIntIntrinsic::UDiv {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Mod if signed => MIRIntIntrinsic::SMod {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Mod => MIRIntIntrinsic::UMod {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Eq => MIRIntIntrinsic::Eq {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Neq => MIRIntIntrinsic::Neq {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Lt if signed => MIRIntIntrinsic::SLt {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Lt => MIRIntIntrinsic::ULt {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Le if signed => MIRIntIntrinsic::SLe {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Le => MIRIntIntrinsic::ULe {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Gt if signed => MIRIntIntrinsic::SGt {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Gt => MIRIntIntrinsic::UGt {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Ge if signed => MIRIntIntrinsic::SGe {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::Ge => MIRIntIntrinsic::UGe {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::BAnd => MIRIntIntrinsic::BAnd {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::BOr => MIRIntIntrinsic::BOr {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::BXor => MIRIntIntrinsic::BXor {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::LShift => MIRIntIntrinsic::LShift {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::RShift if signed => MIRIntIntrinsic::ARShift {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::RShift => MIRIntIntrinsic::LRShift {
                    out,
                    lhs: ml,
                    rhs: mr,
                },
                HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                    unreachable!("logical operators evaluate as short circuits")
                }
            }
            .into()
        }
        HMIRTypeKind::PointerTo(_) => match op {
            HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt {
                out,
                lhs: ml,
                rhs: mr,
            },
            HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq {
                out,
                lhs: ml,
                rhs: mr,
            },
            _ => unreachable!("'{}' has no pointer form", op.path()),
        }
        .into(),
        _ => {
            let lhs = types::name(context.env(), lhs.ty());
            let rhs = types::name(context.env(), rhs.ty());
            unreachable!("'{}' does not apply to these {lhs} and {rhs}", op.path())
        }
    };
    context.runtime_mut().emit_intrinsic(instruction, span);

    Ok(result)
}

pub fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();
    let value = super::value(context, value, span)?;
    let (out, result) = output(context, ty, span)?;

    let instruction: MIRIntrinsic = match (op, kind(context, ty)) {
        (HMIRUnaryOp::LNot, _) => MIRIntIntrinsic::LNot { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_)) => MIRFloatIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::BNot { out, value }.into(),
        _ => unreachable!("'{}' does not apply to this operand", op.path()),
    };
    context.runtime_mut().emit_intrinsic(instruction, span);

    Ok(result)
}

pub fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let kinds = (kind(context, value.ty()), kind(context, target));
    let target_ty = lower_type(context.env_mut(), target, span)?;
    let value = super::value(context, value, span)?;
    let (out, result) = output(context, target, span)?;

    let instruction: MIRIntrinsic = match kinds {
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) => MIRIntIntrinsic::IntCast {
            out,
            value,
            target: mir_int(to.width()),
            sign_extend: from.signed() && from.width() != HMIRIntWidth::I1,
        }
        .into(),
        (HMIRTypeKind::Int(from), HMIRTypeKind::Float(to)) => MIRIntIntrinsic::ToFloat {
            out,
            value,
            target: mir_float(to.width()),
            signed: from.signed(),
        }
        .into(),
        (HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)) => MIRIntIntrinsic::ToPtr {
            out,
            value,
            sign_extend: from.signed(),
        }
        .into(),

        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(to)) => MIRFloatIntrinsic::ToInt {
            out,
            value,
            target_ty,
            signed: to.signed(),
        }
        .into(),
        (HMIRTypeKind::Float(_), HMIRTypeKind::Float(to)) => MIRFloatIntrinsic::FloatCast {
            out,
            value,
            float_ty: mir_float(to.width()),
        }
        .into(),

        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => MIRPtrIntrinsic::ToInt {
            out,
            ptr: value,
            target_ty,
        }
        .into(),
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => MIRInternalIntrinsic::Bitcast {
            out,
            value,
            target_ty,
        }
        .into(),

        _ => unreachable!("conversions are between scalar types"),
    };
    context.runtime_mut().emit_intrinsic(instruction, span);

    Ok(result)
}

fn element_size(
    context: &mut LocalLoweringContext,
    element: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<i128> {
    let element = lower_type(context.env_mut(), element, span)?;

    Ok(calculate_type_layout(context.env().module(), element).size() as i128)
}

pub fn offset(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = index.ty();
    let size = element_size(context, element, span)?;

    let (ptr, offset) = match index.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Int(value)) => {
            let value = value * size;
            let offset = HMIRValue::constant(ty, HMIRImmediate::Int(value));

            let ptr = super::value(context, pointer, span)?;
            (ptr, super::value(context, &offset, span)?)
        }
        _ => {
            let size = HMIRValue::constant(ty, HMIRImmediate::Int(size));
            let lhs = super::value(context, index, span)?;
            let rhs = super::value(context, &size, span)?;
            let ptr = super::value(context, pointer, span)?;

            let scaled = register(context, ty, span)?;
            let out = MIRTarget::Register(scaled);
            let scale = match kind(context, ty) {
                HMIRTypeKind::Int(int) if int.signed() => MIRIntIntrinsic::SMul { out, lhs, rhs },
                _ => MIRIntIntrinsic::UMul { out, lhs, rhs },
            };
            context.runtime_mut().emit_intrinsic(scale, span);

            (ptr, MIRValue::Register(scaled))
        }
    };

    let (out, result) = output(context, pointer.ty(), span)?;
    let instruction = match subtract {
        true => MIRPtrIntrinsic::Sub { out, ptr, offset },
        false => MIRPtrIntrinsic::Add { out, ptr, offset },
    };
    context.runtime_mut().emit_intrinsic(instruction, span);

    Ok(result)
}

pub fn difference(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let element_ty = lower_type(context.env_mut(), element, span)?;
    let lhs = super::value(context, lhs, span)?;
    let rhs = super::value(context, rhs, span)?;
    let (out, result) = output(context, result, span)?;
    context.runtime_mut().emit_intrinsic(
        MIRPtrIntrinsic::Diff {
            out,
            lhs,
            rhs,
            element_ty,
        },
        span,
    );

    Ok(result)
}

pub fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if let HMIRValueKind::Immediate(HMIRImmediate::TaggedUnion { variant, .. }) = value.kind() {
        return Ok(HMIRValue::new(ty, HMIRImmediate::Int(*variant as i128)));
    }

    let stored = context.env().referent(value.ty());
    let sum_ty = lower_type(context.env_mut(), stored, span)?;
    let value = if context.env().is_reference(value.ty()) {
        read(context, value, span)?
    } else {
        super::value(context, value, span)?
    };
    let (out, result) = output(context, ty, span)?;
    context.runtime_mut().emit_intrinsic(
        MIRAggregateIntrinsic::SumIndex { out, value, sum_ty },
        span,
    );

    Ok(result)
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let fields = fields
        .iter()
        .map(|(index, value)| Ok((*index, super::value(context, value, span)?)))
        .collect::<CXResult<_>>()?;

    let mir_ty = lower_type(context.env_mut(), ty, span)?;
    let (out, result) = output(context, ty, span)?;
    context.runtime_mut().emit_intrinsic(
        MIRAggregateIntrinsic::AggregateInit {
            out,
            ty: mir_ty,
            fields,
        },
        span,
    );

    Ok(result)
}

pub fn function_address(
    context: &mut LocalLoweringContext,
    function: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let HMIRValueKind::Immediate(function_constant) = function.kind() else {
        unreachable!("a function designator is constant")
    };

    let cx_mir::MIRConstant::Function(fn_id) =
        super::constant(context, function_constant, function.ty(), span)?
    else {
        unreachable!("a function designator lowers to a function")
    };

    let (out, result) = output(context, ty, span)?;
    context
        .runtime_mut()
        .emit_intrinsic(MIRInternalIntrinsic::GetFnPtr { out, fn_id }, span);
    Ok(result)
}

pub fn assert(
    context: &mut LocalLoweringContext,
    condition: &HMIRValue,
    message: Option<&str>,
    span: &TokenRange,
) -> CXResult<()> {
    if message.is_some() && !context.env().settings().assertions() {
        return Ok(());
    }

    let condition = super::value(context, condition, span)?;
    let kind = match message {
        Some(message) => MIRInternalIntrinsic::Assert {
            condition,
            message: Some(message.into()),
        },
        None => MIRInternalIntrinsic::Assume { condition },
    };
    context.runtime_mut().emit_intrinsic(kind, span);
    Ok(())
}

pub fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let callee = match callee.as_function() {
        Some(function) => {
            let HMIRValueKind::Immediate(constant_value) = function.kind() else {
                unreachable!("a known function is constant")
            };
            MIRValue::Constant(super::constant(
                context,
                constant_value,
                function.ty(),
                span,
            )?)
        }
        None => super::value(context, callee, span)?,
    };
    let args = args
        .iter()
        .map(|arg| super::value(context, arg, span))
        .collect::<CXResult<_>>()?;

    let returns = context.env().resolve_kind(result);
    let (out, value) = match matches!(returns, HMIRTypeKind::Void | HMIRTypeKind::Unreachable) {
        true => (None, HMIRValue::constant(result, HMIRImmediate::Unit)),
        false => {
            let register = register(context, result, span)?;
            (
                Some(register),
                slot(context, result, MIRBindable::Register(register)),
            )
        }
    };
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Call { out, callee, args }, span.clone());

    Ok(value)
}

pub fn variadic(
    context: &mut LocalLoweringContext,
    op: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    let (instruction, result) = match op {
        VAIntrinsic::Start { list, last } => {
            let list = super::value(context, list, span)?;
            let last = super::value(context, last, span)?;
            (MIRVAIntrinsic::VaStart { list, last }, None)
        }
        VAIntrinsic::End { list } => {
            let list = super::value(context, list, span)?;
            (MIRVAIntrinsic::VaEnd { list }, None)
        }
        VAIntrinsic::Arg { list, ty } => {
            let list = super::value(context, list, span)?;
            let mir_ty = lower_type(context.env_mut(), *ty, span)?;
            let (out, result) = output(context, *ty, span)?;
            (
                MIRVAIntrinsic::VaArg {
                    out,
                    list,
                    ty: mir_ty,
                },
                Some(result),
            )
        }
    };
    context.runtime_mut().emit_intrinsic(instruction, span);
    Ok(result)
}
