//! Binary and unary operation lowering

use cx_bytecode_data::{
    types::{BCIntegerType, BCType},
    BCInstructionKind, BCIntBinOp, BCFloatBinOp, BCPtrBinOp, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRBinOp, MIRExpression, MIRFloatBinOp, MIRIntegerBinOp, MIRPtrBinOp, MIRPtrDiffBinOp, MIRUnOp},
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::BCBuilder;
use super::expressions::lower_expression;

/// Lower a binary operation
pub fn lower_binary_op(
    builder: &mut BCBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRBinOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
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
            BCInstructionKind::IntegerBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Float { op, .. } => {
            let bc_op = convert_float_binop(op);
            BCInstructionKind::FloatBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::PtrDiff { op, ptr_inner } => {
            let bc_inner_type = builder.convert_cx_type(ptr_inner);
            let ptr_op = match op {
                MIRPtrDiffBinOp::ADD => BCPtrBinOp::ADD,
                MIRPtrDiffBinOp::SUB => BCPtrBinOp::SUB,
            };
            BCInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: bc_inner_type.clone(),
                type_padded_size: ptr_inner.padded_size() as u64,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Pointer { op } => {
            let ptr_op = match op {
                MIRPtrBinOp::EQ => BCPtrBinOp::EQ,
                MIRPtrBinOp::NE => BCPtrBinOp::NE,
                MIRPtrBinOp::LT => BCPtrBinOp::LT,
                MIRPtrBinOp::GT => BCPtrBinOp::GT,
                MIRPtrBinOp::LE => BCPtrBinOp::LE,
                MIRPtrBinOp::GE => BCPtrBinOp::GE,
            };
            BCInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: BCType::default_pointer(),
                type_padded_size: 1,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

/// Lower a logical operation (AND or OR) with short-circuit evaluation
///
/// For LOR (`a || b`):
///   - If `a` is true, skip `b` and return true
///   - If `a` is false, evaluate `b` and return its result
///
/// For LAND (`a && b`):
///   - If `a` is false, skip `b` and return false
///   - If `a` is true, evaluate `b` and return its result
fn lower_logical_op(
    builder: &mut BCBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRIntegerBinOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
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
                BCInstructionKind::Branch {
                    condition: lhs_result,
                    true_block: merge_block.clone(),
                    false_block: continue_block.clone(),
                },
                BCType::unit(),
                false,
            )?;
        }
        MIRIntegerBinOp::LAND => {
            // If LHS is false, go to merge (short-circuit)
            // If LHS is true, go to continue (evaluate RHS)
            builder.add_new_instruction(
                BCInstructionKind::Branch {
                    condition: lhs_result,
                    true_block: continue_block.clone(),
                    false_block: merge_block.clone(),
                },
                BCType::unit(),
                false,
            )?;
        }
        _ => unreachable!("lower_logical_op called with non-logical operation"),
    }

    // Continue block: evaluate RHS and jump to merge
    builder.set_current_block(continue_block.clone());
    let rhs_result = lower_expression(builder, rhs)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: merge_block.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Move merge block to the end and set it as current
    builder.move_block_to_end(&merge_block);
    builder.set_current_block(merge_block.clone());

    // Create phi node at merge block
    // For LOR: from entry->true (1), from continue->rhs_result
    // For LAND: from entry->false (0), from continue->rhs_result
    let short_circuit_value = match op {
        MIRIntegerBinOp::LOR => BCValue::IntImmediate {
            val: 1,
            _type: BCIntegerType::I1,
        },
        MIRIntegerBinOp::LAND => BCValue::IntImmediate {
            val: 0,
            _type: BCIntegerType::I1,
        },
        _ => unreachable!(),
    };

    let phi_result = builder.add_new_instruction(
        BCInstructionKind::Phi {
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
    builder: &mut BCBuilder,
    operand: &MIRExpression,
    op: &MIRUnOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRUnOp::LNOT => BCInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_bytecode_data::BCIntUnOp::LNOT,
        },
        MIRUnOp::BNOT => BCInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_bytecode_data::BCIntUnOp::BNOT,
        },
        MIRUnOp::NEG => {
            let zero = BCValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            BCInstructionKind::IntegerBinOp {
                op: BCIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }
        MIRUnOp::FNEG => {
            BCInstructionKind::FloatUnOp {
                op: cx_bytecode_data::BCFloatUnOp::NEG,
                value: bc_operand,
            }
        }
        MIRUnOp::INEG => {
            let zero = BCValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            BCInstructionKind::IntegerBinOp {
                op: BCIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }

        MIRUnOp::PostIncrement(amt) | MIRUnOp::PreIncrement(amt) => {
            let loaded_val = builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: bc_operand.clone(),
                    _type: bc_result_type.clone(),
                },
                bc_result_type.clone(),
                true,
            )?;

            let result = builder.add_new_instruction(
                BCInstructionKind::IntegerBinOp {
                    op: BCIntBinOp::ADD,
                    left: loaded_val.clone(),
                    right: BCValue::IntImmediate {
                        val: *amt as i64,
                        _type: match &bc_result_type.kind {
                            cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                            _ => panic!("PreIncrement requires integer type"),
                        },
                    },
                },
                bc_result_type.clone(),
                true,
            )?;

            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: bc_operand,
                    value: result.clone(),
                    _type: bc_result_type.clone(),
                },
                BCType::unit(),
                false,
            )?;

            return match op {
                MIRUnOp::PreIncrement(_) => Ok(result),
                MIRUnOp::PostIncrement(_) => Ok(loaded_val),
                _ => unreachable!(),
            };
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

fn convert_int_binop(op: &MIRIntegerBinOp) -> BCIntBinOp {
    match op {
        MIRIntegerBinOp::ADD => BCIntBinOp::ADD,
        MIRIntegerBinOp::SUB => BCIntBinOp::SUB,
        MIRIntegerBinOp::MUL => BCIntBinOp::MUL,
        MIRIntegerBinOp::IMUL => BCIntBinOp::IMUL,
        MIRIntegerBinOp::DIV => BCIntBinOp::UDIV,
        MIRIntegerBinOp::IDIV => BCIntBinOp::IDIV,
        MIRIntegerBinOp::MOD => BCIntBinOp::UREM,
        MIRIntegerBinOp::IMOD => BCIntBinOp::IREM,
        MIRIntegerBinOp::EQ => BCIntBinOp::EQ,
        MIRIntegerBinOp::NE => BCIntBinOp::NE,
        MIRIntegerBinOp::LT => BCIntBinOp::ULT,
        MIRIntegerBinOp::LE => BCIntBinOp::ULE,
        MIRIntegerBinOp::GT => BCIntBinOp::UGT,
        MIRIntegerBinOp::GE => BCIntBinOp::UGE,
        MIRIntegerBinOp::ILT => BCIntBinOp::ILT,
        MIRIntegerBinOp::ILE => BCIntBinOp::ILE,
        MIRIntegerBinOp::IGT => BCIntBinOp::IGT,
        MIRIntegerBinOp::IGE => BCIntBinOp::IGE,
        // Note: LAND and LOR are now handled by lower_logical_op for short-circuit evaluation
        MIRIntegerBinOp::BAND => BCIntBinOp::BAND,
        MIRIntegerBinOp::BOR => BCIntBinOp::BOR,
        MIRIntegerBinOp::BXOR => BCIntBinOp::BXOR,
        _ => panic!("Logical operators (LAND, LOR) should be handled by lower_logical_op"),
    }
}

fn convert_float_binop(op: &MIRFloatBinOp) -> BCFloatBinOp {
    match op {
        MIRFloatBinOp::FADD => BCFloatBinOp::ADD,
        MIRFloatBinOp::FSUB => BCFloatBinOp::SUB,
        MIRFloatBinOp::FMUL => BCFloatBinOp::FMUL,
        MIRFloatBinOp::FDIV => BCFloatBinOp::FDIV,
        MIRFloatBinOp::EQ => BCFloatBinOp::EQ,
        MIRFloatBinOp::NEQ => BCFloatBinOp::NEQ,
        MIRFloatBinOp::FLT => BCFloatBinOp::FLT,
        MIRFloatBinOp::FLE => BCFloatBinOp::FLE,
        MIRFloatBinOp::FGT => BCFloatBinOp::FGT,
        MIRFloatBinOp::FGE => BCFloatBinOp::FGE,
    }
}
