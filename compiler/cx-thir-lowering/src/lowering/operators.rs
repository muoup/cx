use cx_log::CXResult;
use cx_mir::{
    MIRBlockTarget, MIRInstructionKind, MIRIntIntrinsic, MIRTarget,
    MIRValue,
};
use cx_thir::thir::{
    data::THIRType,
    expression::{THIRBinOp, THIRCoercion, THIRExpression, THIRIntBinOp, THIRUnOp},
};

use super::types::lower_int_type;
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

    match op {
        THIRBinOp::Integer { itype, op } => match op {
            THIRIntBinOp::ADD => builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::Add {
                    out: MIRTarget::Register(out),
                    lhs,
                    rhs,
                },
                expr.token_range.clone(),
            ),

            _ => todo!(),
        },

        _ => todo!(),
    }

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

    match op {
        THIRUnOp::INEG => {
            let out = builder
                .fun_mut()
                .new_register(return_type, None);

            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::Neg {
                    out: MIRTarget::Register(out),
                    value: operand,
                },
                expr.token_range.clone(),
            )?;

            Ok(MIRValue::Register(out))
        },

        _ => todo!()
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

    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type,
            to_type,
        } => {
            let out = builder.fun_mut().new_register(mir_to_type, None);
            let to_type = lower_int_type(*to_type);

            builder.fun_mut().emit_intrinsic(
                MIRIntIntrinsic::IntCast {
                    out: MIRTarget::Register(out),
                    value: operand,
                    target: to_type,
                    sign_extend: *sextend,
                },
                expr.token_range.clone(),
            )?;

            Ok(MIRValue::Register(out))
        }

        _ => todo!(),
    }
}
