use cx_bytecode_data::{
    types::BCType, BCFloatBinOp, BCFunctionPrototype, BCInstructionKind, BCIntBinOp, BCPtrBinOp,
    BCValue,
};
use cx_typechecker_data::mir::{
    expression::{
        MIRBinOp, MIRFloatBinOp, MIRIntegerBinOp, MIRPtrBinOp, MIRPtrDiffBinOp, MIRRegister,
        MIRValue,
    },
    types::MIRType,
};
use cx_util::CXResult;

use crate::{builder::BCBuilder, mir_lowering::instructions::lower_value};

pub(crate) fn lower_binop(
    builder: &mut BCBuilder,
    result: &MIRRegister,
    op: &MIRBinOp,
    lhs: &MIRValue,
    rhs: &MIRValue,
) -> CXResult<BCValue> {
    match op {
        MIRBinOp::Integer { itype: _, op } => lower_int_binop(builder, result, lhs, rhs, op),
        MIRBinOp::Float { ftype: _, op } => lower_float_binop(builder, result, lhs, rhs, op),
        MIRBinOp::PtrDiff { op, ptr_inner } => {
            lower_ptrdiff_binop(builder, result, lhs, rhs, op, ptr_inner)
        }
        MIRBinOp::Pointer { op } => lower_ptr_binop(builder, result, lhs, rhs, op),
    }
}

fn lower_int_binop(
    builder: &mut BCBuilder,
    result: &MIRRegister,
    lhs: &MIRValue,
    rhs: &MIRValue,
    op: &MIRIntegerBinOp,
) -> CXResult<BCValue> {
    enum OperationCategory {
        Arithmetic,
        Comparison,
    }

    let bc_lhs = lower_value(builder, lhs)?;
    let bc_rhs = lower_value(builder, rhs)?;

    let (bc_op, bc_op_category) = match op {
        MIRIntegerBinOp::ADD => (BCIntBinOp::ADD, OperationCategory::Arithmetic),
        MIRIntegerBinOp::SUB => (BCIntBinOp::SUB, OperationCategory::Arithmetic),
        MIRIntegerBinOp::MUL => (BCIntBinOp::MUL, OperationCategory::Arithmetic),
        MIRIntegerBinOp::IMUL => (BCIntBinOp::IMUL, OperationCategory::Arithmetic),
        MIRIntegerBinOp::DIV => (BCIntBinOp::UDIV, OperationCategory::Arithmetic),
        MIRIntegerBinOp::IDIV => (BCIntBinOp::IDIV, OperationCategory::Arithmetic),
        MIRIntegerBinOp::MOD => (BCIntBinOp::UREM, OperationCategory::Arithmetic),
        MIRIntegerBinOp::IMOD => (BCIntBinOp::IREM, OperationCategory::Arithmetic),
        MIRIntegerBinOp::EQ => (BCIntBinOp::EQ, OperationCategory::Comparison),
        MIRIntegerBinOp::NE => (BCIntBinOp::NE, OperationCategory::Comparison),
        MIRIntegerBinOp::LT => (BCIntBinOp::ULT, OperationCategory::Comparison),
        MIRIntegerBinOp::LE => (BCIntBinOp::ULE, OperationCategory::Comparison),
        MIRIntegerBinOp::GT => (BCIntBinOp::UGT, OperationCategory::Comparison),
        MIRIntegerBinOp::GE => (BCIntBinOp::UGE, OperationCategory::Comparison),
        MIRIntegerBinOp::ILT => (BCIntBinOp::ILT, OperationCategory::Comparison),
        MIRIntegerBinOp::ILE => (BCIntBinOp::ILE, OperationCategory::Comparison),
        MIRIntegerBinOp::IGT => (BCIntBinOp::IGT, OperationCategory::Comparison),
        MIRIntegerBinOp::IGE => (BCIntBinOp::IGE, OperationCategory::Comparison),

        MIRIntegerBinOp::BAND => (BCIntBinOp::BAND, OperationCategory::Arithmetic),
        MIRIntegerBinOp::BOR => (BCIntBinOp::BOR, OperationCategory::Arithmetic),
        MIRIntegerBinOp::BXOR => (BCIntBinOp::BXOR, OperationCategory::Arithmetic),
    };

    let result_type = match bc_op_category {
        OperationCategory::Arithmetic => builder.convert_cx_type(&lhs.get_type()),
        OperationCategory::Comparison => BCType::bool(),
    };

    builder.add_instruction_translated(
        BCInstructionKind::IntegerBinOp {
            op: bc_op,
            left: bc_lhs,
            right: bc_rhs,
        },
        result_type,
        Some(result.clone()),
    )
}

fn lower_float_binop(
    builder: &mut BCBuilder,
    result: &MIRRegister,
    lhs: &MIRValue,
    rhs: &MIRValue,
    op: &MIRFloatBinOp,
) -> CXResult<BCValue> {
    let bc_lhs = lower_value(builder, lhs)?;
    let bc_rhs = lower_value(builder, rhs)?;

    let bc_op = match op {
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
    };

    builder.add_instruction_translated(
        BCInstructionKind::FloatBinOp {
            op: bc_op,
            left: bc_lhs,
            right: bc_rhs,
        },
        BCType::bool(),
        Some(result.clone()),
    )
}

fn lower_ptrdiff_binop(
    builder: &mut BCBuilder,
    result: &MIRRegister,
    lhs: &MIRValue,
    rhs: &MIRValue,
    op: &MIRPtrDiffBinOp,
    ptr_inner: &MIRType,
) -> CXResult<BCValue> {
    let bc_lhs = lower_value(builder, lhs)?;
    let bc_rhs = lower_value(builder, rhs)?;
    let bc_element_type = builder.convert_cx_type(ptr_inner);

    let bc_op = match op {
        MIRPtrDiffBinOp::ADD => BCPtrBinOp::ADD,
        MIRPtrDiffBinOp::SUB => BCPtrBinOp::SUB,
    };

    builder.add_instruction_translated(
        BCInstructionKind::PointerBinOp {
            op: bc_op,
            ptr_type: bc_element_type,
            type_padded_size: ptr_inner.padded_size(),
            left: bc_lhs,
            right: bc_rhs,
        },
        BCType::default_pointer(),
        Some(result.clone()),
    )
}

fn lower_ptr_binop(
    builder: &mut BCBuilder,
    result: &MIRRegister,
    lhs: &MIRValue,
    rhs: &MIRValue,
    op: &MIRPtrBinOp,
) -> CXResult<BCValue> {
    let bc_lhs = lower_value(builder, lhs)?;
    let bc_rhs = lower_value(builder, rhs)?;

    let bc_op = match op {
        MIRPtrBinOp::EQ => BCPtrBinOp::EQ,
        MIRPtrBinOp::NE => BCPtrBinOp::NE,
        MIRPtrBinOp::LT => BCPtrBinOp::LT,
        MIRPtrBinOp::GT => BCPtrBinOp::GT,
        MIRPtrBinOp::LE => BCPtrBinOp::LE,
        MIRPtrBinOp::GE => BCPtrBinOp::GE,
    };

    builder.add_instruction_translated(
        BCInstructionKind::PointerBinOp {
            op: bc_op,
            ptr_type: BCType::default_pointer(),
            type_padded_size: 0,
            left: bc_lhs,
            right: bc_rhs,
        },
        BCType::bool(),
        Some(result.clone()),
    )
}

pub fn lower_call_params(
    builder: &mut BCBuilder,
    params: &[MIRValue],
    prototype: &BCFunctionPrototype,
) -> CXResult<Vec<BCValue>> {
    let mut lowered_params = Vec::new();

    if let Some(buffer_type) = prototype.temp_buffer.as_ref() {
        let temp_buffer = builder.add_new_instruction(
            BCInstructionKind::Allocate {
                _type: buffer_type.clone(),
                alignment: buffer_type.alignment(),
            },
            BCType::default_pointer(),
            true,
        )?;

        lowered_params.push(temp_buffer);
    }

    for param in params.iter() {
        let bc_value = lower_value(builder, param)?;
        lowered_params.push(bc_value);
    }

    Ok(lowered_params)
}
