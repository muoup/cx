use cx_log::CXResult;
use cx_mir::{
    MIRBlockTarget, MIRConstant, MIRFloatIntrinsic, MIRInstruction, MIRInstructionKind,
    MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic, MIRTarget, MIRValue,
    ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_thir::thir::{
    contextual_eq::TypeContextEqual,
    data::THIRType,
    expression::{
        THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRFloatBinOp, THIRIntBinOp,
        THIRPtrBinOp, THIRPtrDiffBinOp, THIRUnOp,
    },
    r#type::THIRTypeKind,
};
use cx_thir::type_context::THIRTypeContext;

use super::types::{lower_float_type, lower_int_type, lower_type_id};
use crate::{
    builder::MIRBuilder,
    lowering::{lower_expression, types::lower_type},
};

pub(super) fn lower_binary_op<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    lhs: &'thir THIRExpression,
    rhs: &'thir THIRExpression,
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

    let rhs_type = &rhs._type;
    let lhs = lower_expression(builder, lhs)?;
    let rhs = lower_expression(builder, rhs)?;

    let result_type = lower_type(builder, &expr._type)?;
    let out = builder.fun_mut().new_register(result_type, None);

    let target = MIRTarget::Register(out);
    let intrinsic = match op {
        THIRBinOp::Integer { op, .. } => MIRIntrinsic::Int(match op {
            THIRIntBinOp::ADD => MIRIntIntrinsic::Add {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::SUB => MIRIntIntrinsic::Sub {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::MUL => MIRIntIntrinsic::UMul {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::IMUL => MIRIntIntrinsic::SMul {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::DIV => MIRIntIntrinsic::UDiv {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::IDIV => MIRIntIntrinsic::SDiv {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::MOD => MIRIntIntrinsic::UMod {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::IMOD => MIRIntIntrinsic::SMod {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::EQ => MIRIntIntrinsic::Eq {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::NE => MIRIntIntrinsic::Neq {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::LT => MIRIntIntrinsic::ULt {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::LE => MIRIntIntrinsic::ULe {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::GT => MIRIntIntrinsic::UGt {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::GE => MIRIntIntrinsic::UGe {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::ILT => MIRIntIntrinsic::SLt {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::ILE => MIRIntIntrinsic::SLe {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::IGT => MIRIntIntrinsic::SGt {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::IGE => MIRIntIntrinsic::SGe {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::LAND => MIRIntIntrinsic::LAnd {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::LOR => MIRIntIntrinsic::LOr {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::BAND => MIRIntIntrinsic::BAnd {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::BOR => MIRIntIntrinsic::BOr {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::BXOR => MIRIntIntrinsic::BXor {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::SHL => MIRIntIntrinsic::LShift {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::ASHR => MIRIntIntrinsic::ARShift {
                out: target,
                lhs,
                rhs,
            },
            THIRIntBinOp::LSHR => MIRIntIntrinsic::LRShift {
                out: target,
                lhs,
                rhs,
            },
        }),
        THIRBinOp::Float { op, .. } => MIRIntrinsic::Float(match op {
            THIRFloatBinOp::FADD => MIRFloatIntrinsic::Add {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FSUB => MIRFloatIntrinsic::Sub {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FMUL => MIRFloatIntrinsic::Mul {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FDIV => MIRFloatIntrinsic::Div {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FEQ => MIRFloatIntrinsic::Eq {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FNE => MIRFloatIntrinsic::Neq {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FLT => MIRFloatIntrinsic::Lt {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FLE => MIRFloatIntrinsic::Le {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FGT => MIRFloatIntrinsic::Gt {
                out: target,
                lhs,
                rhs,
            },
            THIRFloatBinOp::FGE => MIRFloatIntrinsic::Geq {
                out: target,
                lhs,
                rhs,
            },
        }),
        THIRBinOp::Pointer { op } => MIRIntrinsic::Pointer(match op {
            THIRPtrBinOp::EQ => MIRPtrIntrinsic::Eq {
                out: target,
                lhs,
                rhs,
            },
            THIRPtrBinOp::NE => MIRPtrIntrinsic::Neq {
                out: target,
                lhs,
                rhs,
            },
            THIRPtrBinOp::LT => MIRPtrIntrinsic::Lt {
                out: target,
                lhs,
                rhs,
            },
            THIRPtrBinOp::LE => MIRPtrIntrinsic::Leq {
                out: target,
                lhs,
                rhs,
            },
            THIRPtrBinOp::GT => MIRPtrIntrinsic::Gt {
                out: target,
                lhs,
                rhs,
            },
            THIRPtrBinOp::GE => MIRPtrIntrinsic::Geq {
                out: target,
                lhs,
                rhs,
            },
        }),
        THIRBinOp::PtrDiff { op, ptr_inner } => {
            let ptr_inner_ty = lower_type_id(builder, *ptr_inner)?;
            let size = calculate_type_layout(builder.types(), ptr_inner_ty).size();
            let offset_ty = lower_type(builder, rhs_type)?;
            let scaled = builder.fun_mut().new_register(offset_ty, None);
            let integer_ty = match &rhs_type.kind {
                cx_thir::thir::data::THIRTypeKind::Integer { _type, .. } => lower_int_type(*_type),
                _ => unreachable!("pointer offset must be an integer"),
            };
            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::SMul {
                    out: MIRTarget::Register(scaled),
                    lhs: rhs,
                    rhs: MIRValue::Constant(MIRConstant::Integer {
                        value: size as i128,
                        ty: integer_ty,
                    }),
                },
                expr.token_range.clone(),
            );
            MIRIntrinsic::Pointer(match op {
                THIRPtrDiffBinOp::ADD => MIRPtrIntrinsic::Add {
                    out: target,
                    ptr: lhs,
                    offset: MIRValue::Register(scaled),
                },
                THIRPtrDiffBinOp::SUB => MIRPtrIntrinsic::Sub {
                    out: target,
                    ptr: lhs,
                    offset: MIRValue::Register(scaled),
                },
            })
        }
    };
    builder
        .fun_mut()
        .emit_intrinsic(intrinsic, expr.token_range.clone());

    Ok(MIRValue::Register(out))
}

pub(crate) fn lower_short_circuit<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    lhs: &'thir THIRExpression,
    rhs: &'thir THIRExpression,
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
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Branch {
            cond: lhs_value,
            true_target: if is_and {
                rhs_target.clone()
            } else {
                merge_target.clone()
            },
            false_target: if is_and { merge_target } else { rhs_target },
        },
        expr.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(rhs_block);
    let rhs_value = lower_expression(builder, rhs)?;
    if !builder.fun().current_block_terminated() {
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
            },
            expr.token_range.clone(),
        ));
    }

    builder.fun_mut().set_current_block(merge_block);
    Ok(MIRValue::Register(result))
}

pub(super) fn lower_unary_op<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    operand: &'thir THIRExpression,
    op: &THIRUnOp,
) -> CXResult<MIRValue> {
    if let THIRUnOp::PreIncrement(amount) | THIRUnOp::PostIncrement(amount) = op {
        return lower_increment(
            builder,
            expr,
            operand,
            *amount,
            matches!(op, THIRUnOp::PreIncrement(_)),
        );
    }

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
        THIRUnOp::PreIncrement(_) | THIRUnOp::PostIncrement(_) => unreachable!(),
    };
    builder
        .fun_mut()
        .emit_intrinsic(intrinsic, expr.token_range.clone());

    Ok(MIRValue::Register(out))
}

fn lower_increment<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    operand: &'thir THIRExpression,
    amount: i8,
    prefix: bool,
) -> CXResult<MIRValue> {
    let MIRValue::PlaceRef(place) = lower_expression(builder, operand)? else {
        unreachable!("increment operand must lower to a place");
    };
    let cx_thir::thir::data::THIRTypeKind::MemoryReference { inner_type, .. } = &operand._type.kind
    else {
        unreachable!("increment operand must have reference type");
    };
    let (integer_type, pointee_type) = match &builder.registry().resolve_type_id(*inner_type).kind {
        cx_thir::thir::data::THIRTypeKind::Integer { _type, .. } => (Some(*_type), None),
        cx_thir::thir::data::THIRTypeKind::PointerTo { inner_type } => (None, Some(*inner_type)),
        _ => (None, None),
    };
    let ty = lower_type_id(builder, *inner_type)?;
    let previous = builder.fun_mut().new_register(ty, None);
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::LiftPlace {
            out: previous,
            place,
        },
        expr.token_range.clone(),
    ));

    let updated = builder.fun_mut().new_register(ty, None);
    let target = MIRTarget::Register(updated);
    match (integer_type, pointee_type) {
        (Some(integer_type), _) => {
            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::Add {
                    out: target,
                    lhs: MIRValue::Register(previous),
                    rhs: MIRValue::Constant(MIRConstant::Integer {
                        value: amount as i128,
                        ty: lower_int_type(integer_type),
                    }),
                },
                expr.token_range.clone(),
            );
        }
        (_, Some(pointee)) => {
            let pointee = lower_type_id(builder, pointee)?;
            let stride = calculate_type_layout(builder.types(), pointee).size() as i128;
            let offset_ty =
                cx_mir::MIRIntType::from_bytes(builder.types().architecture().pointer_size() as u8)
                    .expect("target pointer size has no integer type");
            let intrinsic = if amount >= 0 {
                MIRPtrIntrinsic::Add {
                    out: target,
                    ptr: MIRValue::Register(previous),
                    offset: MIRValue::Constant(MIRConstant::Integer {
                        value: stride * amount as i128,
                        ty: offset_ty,
                    }),
                }
            } else {
                MIRPtrIntrinsic::Sub {
                    out: target,
                    ptr: MIRValue::Register(previous),
                    offset: MIRValue::Constant(MIRConstant::Integer {
                        value: stride * -(amount as i128),
                        ty: offset_ty,
                    }),
                }
            };
            builder
                .fun_mut()
                .emit_intrinsic(intrinsic, expr.token_range.clone());
        }
        _ => unreachable!("increment requires an integer or pointer place"),
    }

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Store {
            target: place,
            value: MIRValue::Register(updated),
            ty,
        },
        expr.token_range.clone(),
    ));
    Ok(if prefix {
        MIRValue::PlaceRef(place)
    } else {
        MIRValue::Register(previous)
    })
}

pub(super) fn lower_coercion<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    operand: MIRValue,
    coercion: &THIRCoercion,
    _from_type: &THIRType,
    to_type: &'thir THIRType,
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
        THIRCoercion::IntToFloat { to_type, sextend } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::ToFloat {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target: lower_float_type(*to_type),
                    signed: *sextend,
                },
                expr.token_range.clone(),
            );
            Ok(MIRValue::Register(out))
        }
        THIRCoercion::FloatToInt {
            to_type: _,
            sextend: _,
        } => {
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
        THIRCoercion::Unreachable => {
            unreachable!("unreachable coercions are handled before lowering")
        }
    }
}

pub(super) fn lower_address_of<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    operand: &'thir THIRExpression,
) -> CXResult<MIRValue> {
    let result_type = lower_type(builder, &expr._type)?;
    let out = builder.fun_mut().new_register(result_type, None);
    let target = MIRTarget::Register(out);

    let intrinsic = if let THIRExpressionKind::StringLiteral { value } = &operand.kind {
        MIRInternalIntrinsic::StringAddress {
            out: target,
            string: value.clone(),
        }
    } else {
        let value = lower_expression(builder, operand)?;
        if is_array_decay(builder, operand, expr) {
            MIRInternalIntrinsic::ArrayAddress {
                out: target,
                array: value,
            }
        } else {
            match value {
                MIRValue::PlaceRef(place) => {
                    MIRInternalIntrinsic::PlaceAddress { out: target, place }
                }
                MIRValue::Constant(MIRConstant::String(string)) => {
                    MIRInternalIntrinsic::StringAddress {
                        out: target,
                        string,
                    }
                }
                MIRValue::Constant(MIRConstant::Function(function)) => {
                    MIRInternalIntrinsic::GetFnPtr {
                        out: target,
                        fn_id: function,
                    }
                }
                MIRValue::Constant(MIRConstant::GlobalRef(global)) => {
                    MIRInternalIntrinsic::GlobalAddress {
                        out: target,
                        global,
                    }
                }
                reference => MIRInternalIntrinsic::ReferenceAddress {
                    out: target,
                    reference,
                },
            }
        }
    };

    builder
        .fun_mut()
        .emit_intrinsic(intrinsic, expr.token_range.clone());
    Ok(MIRValue::Register(out))
}

fn is_array_decay(
    builder: &MIRBuilder<'_>,
    operand: &THIRExpression,
    expr: &THIRExpression,
) -> bool {
    let array_type = match &operand._type.kind {
        THIRTypeKind::Array { .. } => &operand._type,
        THIRTypeKind::MemoryReference { inner_type, .. } => {
            builder.registry().resolve_type_id(*inner_type)
        }
        _ => return false,
    };
    let THIRTypeKind::Array {
        inner_type: array_inner,
        ..
    } = &array_type.kind
    else {
        return false;
    };
    let THIRTypeKind::PointerTo {
        inner_type: pointer_inner,
    } = &expr._type.kind
    else {
        return false;
    };

    let array_inner = builder.registry().resolve_type_id(*array_inner);
    let pointer_inner = builder.registry().resolve_type_id(*pointer_inner);
    array_inner.clone().without_specifiers().contextual_eq(
        &pointer_inner.clone().without_specifiers(),
        builder.registry(),
    )
}
