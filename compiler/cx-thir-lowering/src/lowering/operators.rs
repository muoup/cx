use cx_log::catalogue::mir;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBlockTarget, MIRConstant, MIRFloatIntrinsic, MIRInstruction,
    MIRInstructionKind, MIRIntIntrinsic, MIRIntType, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRTarget, MIRTypeKind, MIRValue,
    ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_thir::thir::{
    contextual_eq::TypeContextEqual,
    data::THIRType,
    expression::{
        THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRFloatBinOp, THIRIntBinOp,
        THIRPtrBinOp, THIRPtrDiffBinOp, THIRUnOp,
    },
    r#type::THIRIntType,
};
use cx_thir::type_context::THIRTypeContext;

use super::types::{lower_float_type, lower_int_type, lower_type_id};
use crate::{
    builder::MIRBuilder,
    lowering::{LowerResult, LowerStop, lower_expression, memory, types::lower_type},
};

pub(super) fn lower_binary_op<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    lhs: &'thir THIRExpression,
    rhs: &'thir THIRExpression,
    op: &THIRBinOp,
) -> LowerResult<MIRValue> {
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

    let result_type = lower_type(builder, &expr._type).map_err(LowerStop::Diagnostic)?;
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
            let ptr_inner_ty = lower_type_id(builder, *ptr_inner).map_err(LowerStop::Diagnostic)?;
            let size = calculate_type_layout(builder.types(), ptr_inner_ty).size();
            let offset_ty = lower_type(builder, rhs_type).map_err(LowerStop::Diagnostic)?;
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
) -> LowerResult<MIRValue> {
    let lhs_value = lower_expression(builder, lhs)?;
    let rhs_block = builder.fun_mut().new_block("logical.rhs");
    let merge_block = builder.fun_mut().new_block("logical.merge");
    let result_type_id = lower_type(builder, &expr._type).map_err(LowerStop::Diagnostic)?;

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
    let rhs_value = lower_expression(builder, rhs);
    match rhs_value {
        Ok(rhs_value) if !builder.fun().current_block_terminated() => {
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
                },
                expr.token_range.clone(),
            ));
        }
        Err(super::LowerStop::Diagnostic(error)) => {
            return Err(super::LowerStop::Diagnostic(error));
        }
        _ => {}
    }

    builder.fun_mut().set_current_block(merge_block);
    Ok(MIRValue::Register(result))
}

pub(super) fn lower_unary_op<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    operand: &'thir THIRExpression,
    op: &THIRUnOp,
) -> LowerResult<MIRValue> {
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
    let return_type = lower_type(builder, &expr._type).map_err(LowerStop::Diagnostic)?;

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
) -> LowerResult<MIRValue> {
    let operand_value = lower_expression(builder, operand)?;
    let operand_target = memory::expect_target(&operand_value);

    let Some(inner_type) = builder.registry().mem_ref_inner(&operand._type) else {
        unreachable!(
            "increment requires a memory reference type, got {:?}",
            operand._type
        )
    };

    let lowered_inner_id = lower_type(builder, inner_type).map_err(LowerStop::Diagnostic)?;
    let lowered_inner = builder
        .types()
        .definition(lowered_inner_id)
        .unwrap()
        .clone();

    let current_value = builder.fun_mut().new_register(lowered_inner_id, None);
    let out = builder.fun_mut().new_register(lowered_inner_id, None);

    builder.emit(MIRInstruction {
        kind: MIRInstructionKind::Store {
            target: MIRTarget::Register(current_value),
            value: operand_value.clone(),
            ty: lowered_inner_id,
        },
        token_range: expr.token_range.clone(),
    });

    match lowered_inner.kind() {
        MIRTypeKind::Integer { ty, .. } => {
            if amount >= 0 {
                builder.fun_mut().emit_intrinsic(
                    MIRIntIntrinsic::Add {
                        out: MIRTarget::Register(out),
                        lhs: MIRValue::Register(current_value),
                        rhs: MIRValue::Constant(MIRConstant::Integer {
                            ty: *ty,
                            value: amount as i128,
                        }),
                    },
                    expr.token_range.clone(),
                );
            } else {
                builder.fun_mut().emit_intrinsic(
                    MIRIntIntrinsic::Sub {
                        out: MIRTarget::Register(out),
                        lhs: MIRValue::Register(current_value),
                        rhs: MIRValue::Constant(MIRConstant::Integer {
                            ty: *ty,
                            value: (-amount) as i128,
                        }),
                    },
                    expr.token_range.clone(),
                );
            }
        }

        MIRTypeKind::PointerTo { inner } => {
            let type_size = calculate_type_layout(builder.types(), *inner).size();
            let total_offset = (amount as isize) * (type_size as isize);

            let pointer_int =
                MIRIntType::from_bytes(builder.types().architecture().pointer_size() as u8)
                    .expect("pointer size must be a valid integer type");

            builder.fun_mut().emit_intrinsic(
                MIRPtrIntrinsic::Add {
                    out: MIRTarget::Register(out),
                    ptr: MIRValue::Register(current_value),
                    offset: MIRValue::Constant(MIRConstant::Integer {
                        ty: pointer_int,
                        value: total_offset as i128,
                    }),
                },
                expr.token_range.clone(),
            );
        }

        _ => {
            unreachable!(
                "increment requires an integer or pointer type, got {:?}",
                inner_type
            )
        }
    };

    builder.emit(MIRInstruction {
        kind: MIRInstructionKind::Store {
            target: operand_target,
            value: MIRValue::Register(out),
            ty: lowered_inner_id,
        },
        token_range: expr.token_range.clone(),
    });

    if prefix {
        Ok(operand_value)
    } else {
        Ok(MIRValue::Register(current_value))
    }
}

pub(super) fn lower_coercion<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
    operand: MIRValue,
    coercion: &THIRCoercion,
    to_type: &'thir THIRType,
) -> LowerResult<MIRValue> {
    let mir_to_type = lower_type(builder, to_type).map_err(LowerStop::Diagnostic)?;

    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type,
            to_type,
        } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            let intrinsic = if *to_type == THIRIntType::I1 {
                MIRIntIntrinsic::Neq {
                    out: MIRTarget::Register(out),
                    lhs: operand,
                    rhs: MIRValue::Constant(MIRConstant::Integer {
                        value: 0,
                        ty: lower_int_type(*from_type),
                    }),
                }
            } else {
                MIRIntIntrinsic::IntCast {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target: lower_int_type(*to_type),
                    sign_extend: *sextend,
                }
            };
            builder
                .fun_mut()
                .emit_intrinsic(intrinsic, expr.token_range.clone());

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
        THIRCoercion::Bitcast => {
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
        THIRCoercion::StringToArray => {
            let MIRValue::Constant(MIRConstant::String(string)) = operand else {
                return crate::log::log_mir_error(
                    &expr.token_range,
                    (&mir::EXPECTED_CONSTANT, "string array initializer".into()),
                )
                .map_err(LowerStop::Diagnostic);
            };
            let Some(MIRTypeKind::Array { length, .. }) =
                builder.types().definition(mir_to_type).map(|ty| ty.kind())
            else {
                unreachable!("string-to-array conversion requires an array target")
            };
            let length = *length;
            if string.len() > length {
                return crate::log::log_mir_error(
                    &expr.token_range,
                    (&mir::ARRAY_TOO_LONG, (string.len(), length)),
                )
                .map_err(LowerStop::Diagnostic);
            }
            let mut fields = string
                .bytes()
                .enumerate()
                .map(|(index, byte)| {
                    (
                        index,
                        MIRValue::Constant(MIRConstant::Integer {
                            value: byte as i128,
                            ty: MIRIntType::I8,
                        }),
                    )
                })
                .collect::<Vec<_>>();
            if string.len() < length {
                fields.push((
                    string.len(),
                    MIRValue::Constant(MIRConstant::Integer {
                        value: 0,
                        ty: MIRIntType::I8,
                    }),
                ));
            }
            let out = builder.fun_mut().new_register(mir_to_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::AggregateInit {
                    out: MIRTarget::Register(out),
                    ty: mir_to_type,
                    fields,
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
) -> LowerResult<MIRValue> {
    let result_type = lower_type(builder, &expr._type).map_err(LowerStop::Diagnostic)?;
    let out = builder.fun_mut().new_register(result_type, None);
    let target = MIRTarget::Register(out);

    // FIXME: The logic here can be simplified, a string literal THIR expression should lower to an MIRConstant::String.
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
                reference => match memory::expect_target(&reference) {
                    MIRTarget::Place(place) => {
                        MIRInternalIntrinsic::PlaceAddress { out: target, place }
                    }
                    MIRTarget::Global(global) => MIRInternalIntrinsic::GlobalAddress {
                        out: target,
                        global,
                    },
                    MIRTarget::Indirect(_) => MIRInternalIntrinsic::ReferenceAddress {
                        out: target,
                        reference,
                    },
                    MIRTarget::Register(_) => unreachable!(),
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
    // TODO: The typechecker should be more reliant on type ids, this function is good proof of that

    let array_type = builder
        .registry()
        .mem_ref_inner(&operand._type)
        .unwrap_or(&operand._type);
    let Some(array_inner) = builder.registry().array_inner(array_type) else {
        return false;
    };
    let Some(pointer_inner) = builder.registry().ptr_inner(&expr._type) else {
        return false;
    };

    array_inner.clone().without_specifiers().contextual_eq(
        &pointer_inner.clone().without_specifiers(),
        builder.registry(),
    )
}
