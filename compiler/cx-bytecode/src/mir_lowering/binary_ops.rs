//! Binary and unary operation lowering

use cx_bytecode_data::{
    types::BCType,
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
        MIRIntegerBinOp::LAND => BCIntBinOp::LAND,
        MIRIntegerBinOp::LOR => BCIntBinOp::LOR,
        MIRIntegerBinOp::BAND => BCIntBinOp::BAND,
        MIRIntegerBinOp::BOR => BCIntBinOp::BOR,
        MIRIntegerBinOp::BXOR => BCIntBinOp::BXOR,
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
