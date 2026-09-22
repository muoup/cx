use cx_log::CXResult;
use cx_mir::{
    MIRBlockTarget, MIRFloatIntrinsic, MIRInstructionKind, MIRIntIntrinsic, MIRInternalIntrinsic,
    MIRIntrinsic, MIRPtrIntrinsic, MIRTarget, MIRValue,
};
use cx_thir::thir::{
    data::THIRType,
    expression::{
        THIRBinOp, THIRCoercion, THIRExpression, THIRFloatBinOp, THIRIntBinOp, THIRPtrBinOp,
        THIRUnOp,
    },
};

use super::types::{lower_float_type, lower_int_type};
use crate::{
    builder::MIRBuilder,
    lowering::{lower_expression, types::lower_type},
};

pub(super) fn lower_binary_op(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRBinOp,
) -> CXResult<MIRValue> {
    if matches!(
        op,
        THIRBinOp::Integer {
            op: THIRIntBinOp::LAND | THIRIntBinOp::LOR,
            ..
        }
    ) {
        return lower_short_circuit(builder, expr, lhs, rhs, op);
    }

    let lhs = lower_expression(builder, lhs)?;
    let rhs = lower_expression(builder, rhs)?;

    let result_type = lower_type(builder, &expr._type)?;
    let out = builder.fun_mut().new_register(result_type, None);

    let target = MIRTarget::Register(out);
    let intrinsic = match op {
        THIRBinOp::Integer { op, .. } => MIRIntrinsic::Int(match op {
            THIRIntBinOp::ADD => MIRIntIntrinsic::Add { out: target, lhs, rhs },
            THIRIntBinOp::SUB => MIRIntIntrinsic::Sub { out: target, lhs, rhs },
            THIRIntBinOp::MUL => MIRIntIntrinsic::UMul { out: target, lhs, rhs },
            THIRIntBinOp::IMUL => MIRIntIntrinsic::SMul { out: target, lhs, rhs },
            THIRIntBinOp::DIV => MIRIntIntrinsic::UDiv { out: target, lhs, rhs },
            THIRIntBinOp::IDIV => MIRIntIntrinsic::SDiv { out: target, lhs, rhs },
            THIRIntBinOp::MOD => MIRIntIntrinsic::UMod { out: target, lhs, rhs },
            THIRIntBinOp::IMOD => MIRIntIntrinsic::SMod { out: target, lhs, rhs },
            THIRIntBinOp::EQ => MIRIntIntrinsic::Eq { out: target, lhs, rhs },
            THIRIntBinOp::NE => MIRIntIntrinsic::Neq { out: target, lhs, rhs },
            THIRIntBinOp::LT => MIRIntIntrinsic::ULt { out: target, lhs, rhs },
            THIRIntBinOp::LE => MIRIntIntrinsic::ULe { out: target, lhs, rhs },
            THIRIntBinOp::GT => MIRIntIntrinsic::UGt { out: target, lhs, rhs },
            THIRIntBinOp::GE => MIRIntIntrinsic::UGe { out: target, lhs, rhs },
            THIRIntBinOp::ILT => MIRIntIntrinsic::SLt { out: target, lhs, rhs },
            THIRIntBinOp::ILE => MIRIntIntrinsic::SLe { out: target, lhs, rhs },
            THIRIntBinOp::IGT => MIRIntIntrinsic::SGt { out: target, lhs, rhs },
            THIRIntBinOp::IGE => MIRIntIntrinsic::SGe { out: target, lhs, rhs },
            THIRIntBinOp::LAND => MIRIntIntrinsic::LAnd { out: target, lhs, rhs },
            THIRIntBinOp::LOR => MIRIntIntrinsic::LOr { out: target, lhs, rhs },
            THIRIntBinOp::BAND => MIRIntIntrinsic::BAnd { out: target, lhs, rhs },
            THIRIntBinOp::BOR => MIRIntIntrinsic::BOr { out: target, lhs, rhs },
            THIRIntBinOp::BXOR => MIRIntIntrinsic::BXor { out: target, lhs, rhs },
            THIRIntBinOp::SHL => MIRIntIntrinsic::LShift { out: target, lhs, rhs },
            THIRIntBinOp::ASHR => MIRIntIntrinsic::ARShift { out: target, lhs, rhs },
            THIRIntBinOp::LSHR => MIRIntIntrinsic::LRShift { out: target, lhs, rhs },
        }),
        THIRBinOp::Float { op, .. } => MIRIntrinsic::Float(match op {
            THIRFloatBinOp::FEQ => MIRFloatIntrinsic::Eq { out: target, lhs, rhs },
            THIRFloatBinOp::FNE => MIRFloatIntrinsic::Neq { out: target, lhs, rhs },
            THIRFloatBinOp::FLT => MIRFloatIntrinsic::Lt { out: target, lhs, rhs },
            THIRFloatBinOp::FLE => MIRFloatIntrinsic::Le { out: target, lhs, rhs },
            THIRFloatBinOp::FGT => MIRFloatIntrinsic::Gt { out: target, lhs, rhs },
            THIRFloatBinOp::FGE => MIRFloatIntrinsic::Geq { out: target, lhs, rhs },
            _ => todo!(),
        }),
        THIRBinOp::Pointer { op } => MIRIntrinsic::Pointer(match op {
            THIRPtrBinOp::EQ => MIRPtrIntrinsic::Eq { out: target, lhs, rhs },
            THIRPtrBinOp::NE => MIRPtrIntrinsic::Neq { out: target, lhs, rhs },
            THIRPtrBinOp::LT => MIRPtrIntrinsic::Lt { out: target, lhs, rhs },
            THIRPtrBinOp::LE => MIRPtrIntrinsic::Leq { out: target, lhs, rhs },
            THIRPtrBinOp::GT => MIRPtrIntrinsic::Gt { out: target, lhs, rhs },
            THIRPtrBinOp::GE => MIRPtrIntrinsic::Geq { out: target, lhs, rhs },
        }),
        THIRBinOp::PtrDiff { .. } => todo!(),
    };
    builder
        .fun_mut()
        .emit_intrinsic(intrinsic, expr.token_range.clone());

    Ok(MIRValue::Register(out))
}

pub(crate) fn lower_short_circuit(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRBinOp,
) -> CXResult<MIRValue> {
    let lhs_value = lower_expression(builder, lhs)?;
    let rhs_block = builder.fun_mut().new_block("logical.rhs");
    let merge_block = builder.fun_mut().new_block("logical.merge");
    let result_type_id = lower_type(builder, &expr._type)?;

    let result = builder
        .fun_mut()
        .block_param(merge_block, result_type_id, None);
    let is_and = matches!(
        op,
        THIRBinOp::Integer {
            op: THIRIntBinOp::LAND,
            ..
        }
    );

    let rhs_target = MIRBlockTarget::new(rhs_block);
    let merge_target = MIRBlockTarget::with_args(merge_block, vec![lhs_value.clone()]);
    builder.emit(MIRInstructionKind::Branch {
        cond: lhs_value,
        true_target: if is_and {
            rhs_target.clone()
        } else {
            merge_target.clone()
        },
        false_target: if is_and { merge_target } else { rhs_target },
    });

    builder.fun_mut().set_current_block(rhs_block);
    let rhs_value = lower_expression(builder, rhs)?;
    if !builder.fun().current_block_terminated() {
        builder.emit(MIRInstructionKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
        });
    }

    builder.fun_mut().set_current_block(merge_block);
    Ok(MIRValue::Register(result))
}

pub(super) fn lower_unary_op(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
    operand: &THIRExpression,
    op: &THIRUnOp,
) -> CXResult<MIRValue> {
    let operand = lower_expression(builder, operand)?;
    let return_type = lower_type(builder, &expr._type)?;

    let out = builder.fun_mut().new_register(return_type, None);
    let target = MIRTarget::Register(out);
    let intrinsic = match op {
        THIRUnOp::INEG => MIRIntrinsic::Int(MIRIntIntrinsic::Neg {
            out: target,
            value: operand,
        }),
        THIRUnOp::FNEG => MIRIntrinsic::Float(MIRFloatIntrinsic::Neg {
            out: target,
            value: operand,
        }),
        THIRUnOp::BNOT => MIRIntrinsic::Int(MIRIntIntrinsic::BNot {
            out: target,
            value: operand,
        }),
        THIRUnOp::LNOT => MIRIntrinsic::Int(MIRIntIntrinsic::LNot {
            out: target,
            value: operand,
        }),
        THIRUnOp::PreIncrement(_) | THIRUnOp::PostIncrement(_) => todo!(),
    };
    builder
        .fun_mut()
        .emit_intrinsic(intrinsic, expr.token_range.clone());

    Ok(MIRValue::Register(out))
}

pub(super) fn lower_coercion(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
    operand: MIRValue,
    coercion: &THIRCoercion,
    _from_type: &THIRType,
    to_type: &THIRType,
) -> CXResult<MIRValue> {
    let mir_to_type = lower_type(builder, to_type)?;

    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type: _,
            to_type,
        } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::IntCast {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target: lower_int_type(*to_type),
                    sign_extend: *sextend,
                },
                expr.token_range.clone(),
            );

            Ok(MIRValue::Register(out))
        }
        THIRCoercion::FloatCast { to_type } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRFloatIntrinsic::FloatCast {
                    out: MIRTarget::Register(out),
                    value: operand,
                    float_ty: lower_float_type(*to_type),
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::IntToFloat {
            to_type,
            sextend: _,
        } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::ToFloat {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target: lower_float_type(*to_type),
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::FloatToInt { to_type: _, sextend: _ } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRFloatIntrinsic::ToInt {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target_ty: mir_to_type,
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::PtrToInt { to_type: _ } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRPtrIntrinsic::ToInt {
                    out: MIRTarget::Register(out),
                    ptr: operand,
                    target_ty: mir_to_type,
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::IntToPtr { sextend: _ } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::ToPtr {
                    out: MIRTarget::Register(out),
                    value: operand,
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::GetFnPtr => {
            let MIRValue::Constant(cx_mir::MIRConstant::Function(function)) = operand else {
                unreachable!("function decay requires a function reference");
            };
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRInternalIntrinsic::GetFnPtr {
                    out: MIRTarget::Register(out),
                    fn_id: function,
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::Typechange => Ok(operand),
        THIRCoercion::ReinterpretBits => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRInternalIntrinsic::Bitcast {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target_ty: mir_to_type,
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::ReferenceBounding(_) => todo!(),
        THIRCoercion::Unreachable => unreachable!("unreachable coercions are handled before lowering"),
    }
}
