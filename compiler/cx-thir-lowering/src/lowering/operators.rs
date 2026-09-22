use cx_log::CXResult;
use cx_mir::{MIRBlockTarget, MIRInstrKind, MIRInstruction, MIRIntIntrinsic, MIRIntrinsic, MIRTarget, MIRValue};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRBinOp, THIRCoercion, THIRExpression, THIRIntBinOp, THIRUnOp},
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

    todo!()
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
    builder.emit(MIRInstrKind::Branch {
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
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
        });
    }

    builder.fun_mut().set_current_block(merge_block);
    Ok(MIRValue::Register(result))
}

pub(super) fn lower_unary_op(op: &THIRUnOp, operand_type: &THIRType) -> MIRUnaryOp {
    match op {
        THIRUnOp::NEG | THIRUnOp::INEG => {
            let (ty, signed) = integer_type(operand_type);
            MIRUnaryOp::IntegerNeg { ty, signed }
        }
        THIRUnOp::FNEG => MIRUnaryOp::FloatNeg(match operand_type.kind {
            THIRTypeKind::Float { _type } => lower_float_type(_type),
            _ => cx_mir::MIRFloatType::F64,
        }),
        THIRUnOp::BNOT => MIRUnaryOp::BitNot(integer_type(operand_type).0),
        THIRUnOp::LNOT => MIRUnaryOp::LogicalNot,
        THIRUnOp::PreIncrement(amount) => MIRUnaryOp::Increment {
            amount: *amount,
            post: false,
        },
        THIRUnOp::PostIncrement(amount) => MIRUnaryOp::Increment {
            amount: *amount,
            post: true,
        },
    }
}

pub(super) fn lower_coercion(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
    operand: MIRValue,
    coercion: &THIRCoercion,
    from_type: &THIRType,
    to_type: &THIRType,
) -> CXResult<MIRValue> {
    let mir_to_type = lower_type(builder, to_type)?;
    let emit_intrinsic = |intrinsic: MIRCoercion| {
        builder.fun_mut().emit(MIRInstruction::new(
            MIRInstrKind::IntrinsicOp(intrinsic),
            expr.token_range.clone(),
        ))
    };

    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type,
            to_type,
        } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            let to_type = lower_int_type(*to_type);

            builder
                .fun_mut()
                .emit(MIRInstruction::new(MIRInstrKind::IntrinsicOp(
                    MIRIntrinsic::Int(MIRIntIntrinsic::IntCast {
                        out: MIRTarget::Register(out),
                        value: operand,
                        target: to_type,
                        sign_extend: *sextend,
                    }),
                )))?;

            Ok(MIRValue::Register(out))
        }
        THIRCoercion::FloatCast { to_type } => emit_coercion(
            operand,
            MIRCoercion::FloatCast {
                from: match from_type.kind {
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
                    _ => cx_mir::MIRFloatType::F64,
                },
                to: lower_float_type(*to_type),
            },
        ),
        THIRCoercion::IntToFloat { to_type, sextend } => emit_coercion(
            operand,
            MIRCoercion::IntToFloat {
                from: integer_type(from_type).0,
                to: lower_float_type(*to_type),
                signed: *sextend,
            },
        ),
        THIRCoercion::FloatToInt { to_type, sextend } => emit_coercion(
            operand,
            MIRCoercion::FloatToInt {
                from: match from_type.kind {
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
                    _ => cx_mir::MIRFloatType::F64,
                },
                to: lower_int_type(*to_type),
                signed: *sextend,
            },
        ),
        THIRCoercion::PtrToInt { to_type } => emit_coercion(
            operand,
            MIRCoercion::PointerToInt {
                to: lower_int_type(*to_type),
            },
        ),
        THIRCoercion::IntToPtr { sextend } => emit_coercion(
            operand,
            MIRCoercion::IntToPointer {
                from: integer_type(from_type).0,
                sign_extend: *sextend,
            },
        ),
        THIRCoercion::GetFnPtr => emit_coercion(operand, MIRCoercion::FunctionToPointer),
        THIRCoercion::Typechange => emit_coercion(operand, MIRCoercion::TypeChange),
        THIRCoercion::ReinterpretBits => emit_coercion(operand, MIRCoercion::ReinterpretBits),

        THIRCoercion::ReferenceBounding(bounded) => {
            let place = builder.fun_mut().new_place(mir_to_type, None, false);

            builder.emit(MIRInstrKind::Store {
                target: MIRTarget::Place(place),
                value: operand,
                ty: mir_to_type,
            });

            for bound in bounded {
                let Some(value) = builder.fun().local(*bound) else {
                    unreachable!("bound local not found")
                };

                let MIRValue::Reference(target) = value else {
                    unreachable!("bound local is not a target")
                };

                builder.emit(MIRInstrKind::Bind { place, to: target });
            }

            Ok(MIRValue::Reference(cx_mir::MIRTarget::Place(place)))
        }

        THIRCoercion::Unreachable => {
            unreachable!("unreachable coercions do not reach MIR coercion lowering")
        }
    }
}
