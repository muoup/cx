//! Binary and unary operation lowering

use cx_lmir::{
    types::{LMIRType, TypeSize},
    LMIRFloatBinOp, LMIRInstructionKind, LMIRIntBinOp, LMIRPtrBinOp, LMIRValue,
};
use cx_log::CXResult;
use cx_thir::{
    thir::{
        data::{THIRType, THIRTypeKind},
        expression::{
            THIRBinOp, THIRExpression, THIRFloatBinOp, THIRIntBinOp, THIRPtrBinOp, THIRPtrDiffBinOp,
            THIRUnOp,
        },
    },
    type_context::THIRTypeContext,
};

use super::expressions::lower_expression;
use crate::builder::LMIRBuilder;

/// Lower a binary operation
pub fn lower_binary_op(
    builder: &mut LMIRBuilder,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRBinOp,
    result_type: &THIRType,
) -> CXResult<LMIRValue> {
    // Handle logical AND and OR with short-circuit evaluation
    if let THIRBinOp::Integer { op, .. } = op {
        if matches!(op, THIRIntBinOp::LAND | THIRIntBinOp::LOR) {
            return lower_logical_op(builder, lhs, rhs, op, result_type);
        }
    }

    let bc_lhs = lower_expression(builder, lhs)?;
    let bc_rhs = lower_expression(builder, rhs)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        THIRBinOp::Integer { op, .. } => {
            let bc_op = convert_int_binop(op);
            LMIRInstructionKind::IntegerBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        THIRBinOp::Float { op, .. } => {
            let bc_op = convert_float_binop(op);
            LMIRInstructionKind::FloatBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        THIRBinOp::PtrDiff { op, ptr_inner } => {
            let bc_inner_type = builder.convert_cx_type(ptr_inner);
            let inner_layout = builder.type_layout(ptr_inner);
            let ptr_op = match op {
                THIRPtrDiffBinOp::ADD => LMIRPtrBinOp::ADD,
                THIRPtrDiffBinOp::SUB => LMIRPtrBinOp::SUB,
            };

            LMIRInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: bc_inner_type.clone(),
                type_size: TypeSize::from(inner_layout.size),
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        THIRBinOp::Pointer { op } => {
            let ptr_op = match op {
                THIRPtrBinOp::EQ => LMIRPtrBinOp::EQ,
                THIRPtrBinOp::NE => LMIRPtrBinOp::NE,
                THIRPtrBinOp::LT => LMIRPtrBinOp::LT,
                THIRPtrBinOp::GT => LMIRPtrBinOp::GT,
                THIRPtrBinOp::LE => LMIRPtrBinOp::LE,
                THIRPtrBinOp::GE => LMIRPtrBinOp::GE,
            };
            LMIRInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: LMIRType::default_pointer(builder.architecture()),
                type_size: TypeSize::from(1),
                left: bc_lhs,
                right: bc_rhs,
            }
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

fn lower_logical_op(
    builder: &mut LMIRBuilder,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRIntBinOp,
    result_type: &THIRType,
) -> CXResult<LMIRValue> {
    let bc_result_type = builder.convert_cx_type(result_type);

    // Create the continue and merge blocks
    let (continue_name, merge_name) = match op {
        THIRIntBinOp::LOR => ("lor_continue", "lor_merge"),
        THIRIntBinOp::LAND => ("land_continue", "land_merge"),
        _ => unreachable!("lower_logical_op called with non-logical operation"),
    };

    let continue_block = builder.create_block(Some(continue_name));
    let merge_block = builder.create_block(Some(merge_name));

    // Evaluate LHS
    let lhs_result = lower_expression(builder, lhs)?;
    let lhs_block = builder.current_block();

    // Branch based on the operation type
    match op {
        THIRIntBinOp::LOR => {
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
        THIRIntBinOp::LAND => {
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
    let rhs_block = builder.current_block();
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
    // For LOR: from lhs_block->true (1), from rhs_block->rhs_result
    // For LAND: from lhs_block->false (0), from rhs_block->rhs_result
    let short_circuit_value = match op {
        THIRIntBinOp::LOR => LMIRValue::IntImmediate {
            val: 1,
            _type: bc_result_type.clone(),
        },
        THIRIntBinOp::LAND => LMIRValue::IntImmediate {
            val: 0,
            _type: bc_result_type.clone(),
        },
        _ => unreachable!(),
    };

    let phi_result = builder.add_new_instruction(
        LMIRInstructionKind::Phi {
            predecessors: vec![(short_circuit_value, lhs_block), (rhs_result, rhs_block)],
        },
        bc_result_type,
        true,
    )?;

    Ok(phi_result)
}

/// Lower a unary operation
pub fn lower_unary_op(
    builder: &mut LMIRBuilder,
    operand: &THIRExpression,
    op: &THIRUnOp,
    result_type: &THIRType,
) -> CXResult<LMIRValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        THIRUnOp::LNOT => LMIRInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_lmir::LMIRIntUnOp::LNOT,
        },
        THIRUnOp::BNOT => LMIRInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_lmir::LMIRIntUnOp::BNOT,
        },
        THIRUnOp::NEG => {
            let zero = LMIRValue::IntImmediate {
                val: 0,
                _type: bc_result_type.clone(),
            };
            LMIRInstructionKind::IntegerBinOp {
                op: LMIRIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }
        THIRUnOp::FNEG => LMIRInstructionKind::FloatUnOp {
            op: cx_lmir::LMIRFloatUnOp::NEG,
            value: bc_operand,
        },
        THIRUnOp::INEG => {
            let zero = LMIRValue::IntImmediate {
                val: 0,
                _type: bc_result_type.clone(),
            };
            LMIRInstructionKind::IntegerBinOp {
                op: LMIRIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }

        THIRUnOp::PostIncrement(amt) => {
            let pre_loaded_val = builder.add_new_instruction(
                LMIRInstructionKind::Load {
                    memory: bc_operand.clone(),
                    _type: bc_result_type.clone(),
                },
                bc_result_type.clone(),
                true,
            )?;

            let increment_instruction = match &result_type.kind {
                THIRTypeKind::Integer { _type: itype, .. } => {
                    let bc_itype = builder.convert_integer_type(itype);

                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::ADD,
                        left: pre_loaded_val.clone(),
                        right: LMIRValue::IntImmediate {
                            val: *amt as i64,
                            _type: LMIRType::with_implicit_abi(
                                builder.architecture(),
                                cx_lmir::types::LMIRTypeKind::Integer(bc_itype),
                            ),
                        },
                    }
                }

                THIRTypeKind::MemoryReference { inner_type, .. }
                | THIRTypeKind::PointerTo { inner_type, .. } => {
                    let inner_type = builder.registry.resolve_type_id(*inner_type);
                    let bc_inner_type = builder.convert_cx_type(inner_type);
                    let type_size = TypeSize::from(builder.type_layout(inner_type).size);

                    LMIRInstructionKind::PointerBinOp {
                        op: LMIRPtrBinOp::ADD,
                        ptr_type: bc_inner_type,
                        type_size,
                        left: pre_loaded_val.clone(),
                        right: LMIRValue::IntImmediate {
                            val: *amt as i64,
                            _type: LMIRType::with_implicit_abi(
                                builder.architecture(),
                                cx_lmir::types::LMIRTypeKind::Integer(builder.convert_integer_type(
                                    &builder.registry.pointer_integer_type(),
                                )),
                            ),
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

            return Ok(pre_loaded_val);
        }

        THIRUnOp::PreIncrement(amt) => {
            let inner = builder
                .registry
                .mem_ref_inner(result_type)
                .unwrap_or_else(|| {
                    panic!(
                        "Increment operation requires a memory reference type, found: {:?}",
                        result_type
                    )
                })
                .clone();
            let inner_bc = builder.convert_cx_type(&inner);

            let pre_loaded_val = builder.add_new_instruction(
                LMIRInstructionKind::Load {
                    memory: bc_operand.clone(),
                    _type: inner_bc.clone(),
                },
                inner_bc.clone(),
                true,
            )?;

            let increment_instruction = match &inner.kind {
                THIRTypeKind::Integer { _type: itype, .. } => {
                    let bc_itype = builder.convert_integer_type(itype);

                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::ADD,
                        left: pre_loaded_val.clone(),
                        right: LMIRValue::IntImmediate {
                            val: *amt as i64,
                            _type: LMIRType::with_implicit_abi(
                                builder.architecture(),
                                cx_lmir::types::LMIRTypeKind::Integer(bc_itype),
                            ),
                        },
                    }
                }

                THIRTypeKind::MemoryReference { inner_type, .. }
                | THIRTypeKind::PointerTo { inner_type, .. } => {
                    let inner_type = builder.registry.resolve_type_id(*inner_type);
                    let bc_inner_type = builder.convert_cx_type(inner_type);
                    let type_size = TypeSize::from(builder.type_layout(inner_type).size);

                    LMIRInstructionKind::PointerBinOp {
                        op: LMIRPtrBinOp::ADD,
                        ptr_type: bc_inner_type,
                        type_size,
                        left: pre_loaded_val.clone(),
                        right: LMIRValue::IntImmediate {
                            val: *amt as i64,
                            _type: LMIRType::with_implicit_abi(
                                builder.architecture(),
                                cx_lmir::types::LMIRTypeKind::Integer(builder.convert_integer_type(
                                    &builder.registry.pointer_integer_type(),
                                )),
                            ),
                        },
                    }
                }

                _ => unreachable!("Increment operation requires integer or pointer type"),
            };

            let result =
                builder.add_new_instruction(increment_instruction, inner_bc.clone(), true)?;
            
            builder.add_new_instruction(
                LMIRInstructionKind::Store {
                    memory: bc_operand,
                    value: result.clone(),
                    _type: inner_bc.clone(),
                },
                LMIRType::unit(),
                false,
            )?;

            return Ok(result);
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

fn convert_int_binop(op: &THIRIntBinOp) -> LMIRIntBinOp {
    match op {
        THIRIntBinOp::ADD => LMIRIntBinOp::ADD,
        THIRIntBinOp::SUB => LMIRIntBinOp::SUB,
        THIRIntBinOp::MUL => LMIRIntBinOp::MUL,
        THIRIntBinOp::IMUL => LMIRIntBinOp::IMUL,
        THIRIntBinOp::DIV => LMIRIntBinOp::UDIV,
        THIRIntBinOp::IDIV => LMIRIntBinOp::IDIV,
        THIRIntBinOp::MOD => LMIRIntBinOp::UREM,
        THIRIntBinOp::IMOD => LMIRIntBinOp::IREM,
        THIRIntBinOp::EQ => LMIRIntBinOp::EQ,
        THIRIntBinOp::NE => LMIRIntBinOp::NE,
        THIRIntBinOp::LT => LMIRIntBinOp::ULT,
        THIRIntBinOp::LE => LMIRIntBinOp::ULE,
        THIRIntBinOp::GT => LMIRIntBinOp::UGT,
        THIRIntBinOp::GE => LMIRIntBinOp::UGE,
        THIRIntBinOp::ILT => LMIRIntBinOp::ILT,
        THIRIntBinOp::ILE => LMIRIntBinOp::ILE,
        THIRIntBinOp::IGT => LMIRIntBinOp::IGT,
        THIRIntBinOp::IGE => LMIRIntBinOp::IGE,
        THIRIntBinOp::BAND => LMIRIntBinOp::BAND,
        THIRIntBinOp::BOR => LMIRIntBinOp::BOR,
        THIRIntBinOp::BXOR => LMIRIntBinOp::BXOR,
        THIRIntBinOp::SHL => LMIRIntBinOp::SHL,
        THIRIntBinOp::ASHR => LMIRIntBinOp::ASHR,
        THIRIntBinOp::LSHR => LMIRIntBinOp::LSHR,

        _ => unreachable!("Logical operators (LAND, LOR) should be handled by lower_logical_op"),
    }
}

fn convert_float_binop(op: &THIRFloatBinOp) -> LMIRFloatBinOp {
    match op {
        THIRFloatBinOp::FADD => LMIRFloatBinOp::ADD,
        THIRFloatBinOp::FSUB => LMIRFloatBinOp::SUB,
        THIRFloatBinOp::FMUL => LMIRFloatBinOp::FMUL,
        THIRFloatBinOp::FDIV => LMIRFloatBinOp::FDIV,
        THIRFloatBinOp::FEQ => LMIRFloatBinOp::EQ,
        THIRFloatBinOp::FNE => LMIRFloatBinOp::NEQ,
        THIRFloatBinOp::FLT => LMIRFloatBinOp::FLT,
        THIRFloatBinOp::FLE => LMIRFloatBinOp::FLE,
        THIRFloatBinOp::FGT => LMIRFloatBinOp::FGT,
        THIRFloatBinOp::FGE => LMIRFloatBinOp::FGE,
    }
}
