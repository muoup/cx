//! Binary and unary operation lowering

use cx_lmir::{
    types::{LMIRIntegerType, LMIRType},
    LMIRFloatBinOp, LMIRInstructionKind, LMIRIntBinOp, LMIRPtrBinOp, LMIRValue,
};
use cx_mir::mir::{
    expression::{
        MIRBinOp, MIRExpression, MIRFloatBinOp, MIRIntegerBinOp, MIRPtrBinOp, MIRPtrDiffBinOp,
        MIRUnOp,
    },
    types::{MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use super::expressions::lower_expression;
use crate::builder::LMIRBuilder;

/// Lower a binary operation
pub fn lower_binary_op(
    builder: &mut LMIRBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRBinOp,
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    // Handle logical AND and OR with short-circuit evaluation
    if let MIRBinOp::Integer { op, .. } = op {
        if matches!(op, MIRIntegerBinOp::LAND | MIRIntegerBinOp::LOR) {
            return lower_logical_op(builder, lhs, rhs, op, result_type);
        }
    }

    let bc_lhs = lower_expression(builder, lhs)?;
    let bc_rhs = lower_expression(builder, rhs)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRBinOp::Integer { op, .. } => {
            let bc_op = convert_int_binop(op);
            LMIRInstructionKind::IntegerBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Float { op, .. } => {
            let bc_op = convert_float_binop(op);
            LMIRInstructionKind::FloatBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::PtrDiff { op, ptr_inner } => {
            let bc_inner_type = builder.convert_cx_type(ptr_inner);
            let ptr_op = match op {
                MIRPtrDiffBinOp::ADD => LMIRPtrBinOp::ADD,
                MIRPtrDiffBinOp::SUB => LMIRPtrBinOp::SUB,
            };
            LMIRInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: bc_inner_type.clone(),
                type_padded_size: ptr_inner.padded_size() as u64,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Pointer { op } => {
            let ptr_op = match op {
                MIRPtrBinOp::EQ => LMIRPtrBinOp::EQ,
                MIRPtrBinOp::NE => LMIRPtrBinOp::NE,
                MIRPtrBinOp::LT => LMIRPtrBinOp::LT,
                MIRPtrBinOp::GT => LMIRPtrBinOp::GT,
                MIRPtrBinOp::LE => LMIRPtrBinOp::LE,
                MIRPtrBinOp::GE => LMIRPtrBinOp::GE,
            };
            LMIRInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: LMIRType::default_pointer(),
                type_padded_size: 1,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

fn lower_logical_op(
    builder: &mut LMIRBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRIntegerBinOp,
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_result_type = builder.convert_cx_type(result_type);

    // Save the entry block (where we evaluate the LHS)
    let entry_block = builder.current_block();

    // Create the continue and merge blocks
    let (continue_name, merge_name) = match op {
        MIRIntegerBinOp::LOR => ("lor_continue", "lor_merge"),
        MIRIntegerBinOp::LAND => ("land_continue", "land_merge"),
        _ => unreachable!("lower_logical_op called with non-logical operation"),
    };

    let continue_block = builder.create_block(Some(continue_name));
    let merge_block = builder.create_block(Some(merge_name));

    // Evaluate LHS
    let lhs_result = lower_expression(builder, lhs)?;

    // Branch based on the operation type
    match op {
        MIRIntegerBinOp::LOR => {
            // If LHS is true, go to merge (short-circuit)
            // If LHS is false, go to continue (evaluate RHS)
            builder.add_new_instruction(
                LMIRInstructionKind::Branch {
                    condition: lhs_result,
                    true_block: merge_block.clone(),
                    false_block: continue_block.clone(),
                },
                LMIRType::unit(),
                false,
            )?;
        }
        MIRIntegerBinOp::LAND => {
            // If LHS is false, go to merge (short-circuit)
            // If LHS is true, go to continue (evaluate RHS)
            builder.add_new_instruction(
                LMIRInstructionKind::Branch {
                    condition: lhs_result,
                    true_block: continue_block.clone(),
                    false_block: merge_block.clone(),
                },
                LMIRType::unit(),
                false,
            )?;
        }
        _ => unreachable!("lower_logical_op called with non-logical operation"),
    }

    // Continue block: evaluate RHS and jump to merge
    builder.set_current_block(continue_block.clone());
    let rhs_result = lower_expression(builder, rhs)?;
    builder.add_new_instruction(
        LMIRInstructionKind::Jump {
            target: merge_block.clone(),
        },
        LMIRType::unit(),
        false,
    )?;

    // Move merge block to the end and set it as current
    builder.move_block_to_end(&merge_block);
    builder.set_current_block(merge_block.clone());

    // Create phi node at merge block
    // For LOR: from entry->true (1), from continue->rhs_result
    // For LAND: from entry->false (0), from continue->rhs_result
    let short_circuit_value = match op {
        MIRIntegerBinOp::LOR => LMIRValue::IntImmediate {
            val: 1,
            _type: LMIRIntegerType::I1,
        },
        MIRIntegerBinOp::LAND => LMIRValue::IntImmediate {
            val: 0,
            _type: LMIRIntegerType::I1,
        },
        _ => unreachable!(),
    };

    let phi_result = builder.add_new_instruction(
        LMIRInstructionKind::Phi {
            predecessors: vec![
                (short_circuit_value, entry_block),
                (rhs_result, continue_block),
            ],
        },
        bc_result_type,
        true,
    )?;

    Ok(phi_result)
}

/// Lower a unary operation
pub fn lower_unary_op(
    builder: &mut LMIRBuilder,
    operand: &MIRExpression,
    op: &MIRUnOp,
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRUnOp::LNOT => LMIRInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_lmir::LMIRIntUnOp::LNOT,
        },
        MIRUnOp::BNOT => LMIRInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_lmir::LMIRIntUnOp::BNOT,
        },
        MIRUnOp::NEG => {
            let zero = LMIRValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_lmir::types::LMIRTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            LMIRInstructionKind::IntegerBinOp {
                op: LMIRIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }
        MIRUnOp::FNEG => LMIRInstructionKind::FloatUnOp {
            op: cx_lmir::LMIRFloatUnOp::NEG,
            value: bc_operand,
        },
        MIRUnOp::INEG => {
            let zero = LMIRValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_lmir::types::LMIRTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            LMIRInstructionKind::IntegerBinOp {
                op: LMIRIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }

        MIRUnOp::PostIncrement(amt) | MIRUnOp::PreIncrement(amt) => {
            let pre_loaded_val = builder.add_new_instruction(
                LMIRInstructionKind::Load {
                    memory: bc_operand.clone(),
                    _type: bc_result_type.clone(),
                },
                bc_result_type.clone(),
                true,
            )?;

            let increment_instruction = match &result_type.kind {
                MIRTypeKind::Integer { _type: itype, .. } => {
                    let bc_itype = builder.convert_integer_type(itype);

                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::ADD,
                        left: pre_loaded_val.clone(),
                        right: LMIRValue::IntImmediate {
                            val: *amt as i64,
                            _type: bc_itype,
                        },
                    }
                }

                MIRTypeKind::PointerTo { inner_type, .. } => {
                    let bc_inner_type = builder.convert_cx_type(inner_type);

                    LMIRInstructionKind::PointerBinOp {
                        op: LMIRPtrBinOp::ADD,
                        ptr_type: bc_inner_type,
                        type_padded_size: result_type.padded_size() as u64,
                        left: pre_loaded_val.clone(),
                        right: LMIRValue::IntImmediate {
                            val: *amt as i64,
                            _type: LMIRIntegerType::I64,
                        },
                    }
                }

                _ => unreachable!("Increment operation requires integer or pointer type"),
            };

            let result =
                builder.add_new_instruction(increment_instruction, bc_result_type.clone(), true)?;

            builder.add_new_instruction(
                LMIRInstructionKind::Store {
                    memory: bc_operand,
                    value: result.clone(),
                    _type: bc_result_type.clone(),
                },
                LMIRType::unit(),
                false,
            )?;

            return match op {
                MIRUnOp::PreIncrement(_) => Ok(result),
                MIRUnOp::PostIncrement(_) => Ok(pre_loaded_val),
                _ => unreachable!(),
            };
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

fn convert_int_binop(op: &MIRIntegerBinOp) -> LMIRIntBinOp {
    match op {
        MIRIntegerBinOp::ADD => LMIRIntBinOp::ADD,
        MIRIntegerBinOp::SUB => LMIRIntBinOp::SUB,
        MIRIntegerBinOp::MUL => LMIRIntBinOp::MUL,
        MIRIntegerBinOp::IMUL => LMIRIntBinOp::IMUL,
        MIRIntegerBinOp::DIV => LMIRIntBinOp::UDIV,
        MIRIntegerBinOp::IDIV => LMIRIntBinOp::IDIV,
        MIRIntegerBinOp::MOD => LMIRIntBinOp::UREM,
        MIRIntegerBinOp::IMOD => LMIRIntBinOp::IREM,
        MIRIntegerBinOp::EQ => LMIRIntBinOp::EQ,
        MIRIntegerBinOp::NE => LMIRIntBinOp::NE,
        MIRIntegerBinOp::LT => LMIRIntBinOp::ULT,
        MIRIntegerBinOp::LE => LMIRIntBinOp::ULE,
        MIRIntegerBinOp::GT => LMIRIntBinOp::UGT,
        MIRIntegerBinOp::GE => LMIRIntBinOp::UGE,
        MIRIntegerBinOp::ILT => LMIRIntBinOp::ILT,
        MIRIntegerBinOp::ILE => LMIRIntBinOp::ILE,
        MIRIntegerBinOp::IGT => LMIRIntBinOp::IGT,
        MIRIntegerBinOp::IGE => LMIRIntBinOp::IGE,
        MIRIntegerBinOp::BAND => LMIRIntBinOp::BAND,
        MIRIntegerBinOp::BOR => LMIRIntBinOp::BOR,
        MIRIntegerBinOp::BXOR => LMIRIntBinOp::BXOR,

        _ => unreachable!("Logical operators (LAND, LOR) should be handled by lower_logical_op"),
    }
}

fn convert_float_binop(op: &MIRFloatBinOp) -> LMIRFloatBinOp {
    match op {
        MIRFloatBinOp::FADD => LMIRFloatBinOp::ADD,
        MIRFloatBinOp::FSUB => LMIRFloatBinOp::SUB,
        MIRFloatBinOp::FMUL => LMIRFloatBinOp::FMUL,
        MIRFloatBinOp::FDIV => LMIRFloatBinOp::FDIV,
        MIRFloatBinOp::EQ => LMIRFloatBinOp::EQ,
        MIRFloatBinOp::NEQ => LMIRFloatBinOp::NEQ,
        MIRFloatBinOp::FLT => LMIRFloatBinOp::FLT,
        MIRFloatBinOp::FLE => LMIRFloatBinOp::FLE,
        MIRFloatBinOp::FGT => LMIRFloatBinOp::FGT,
        MIRFloatBinOp::FGE => LMIRFloatBinOp::FGE,
    }
}
