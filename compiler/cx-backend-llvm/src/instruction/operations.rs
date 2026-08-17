use super::inst_num;
use crate::arithmetic::{generate_int_binop, generate_ptr_binop};
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::types::{LMIRType, TypeSize};
use cx_lmir::{LMIRFloatBinOp, LMIRFloatUnOp, LMIRIntBinOp, LMIRIntUnOp, LMIRPtrBinOp, LMIRValue};
use inkwell::AddressSpace;
use inkwell::values::{AnyValue, AnyValueEnum};

pub(super) fn generate_pointer_binop<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    left: &LMIRValue,
    right: &LMIRValue,
    type_size: TypeSize,
    op: LMIRPtrBinOp,
) -> Option<CodegenValue<'a>> {
    let left = function_state.get_value(left)?.get_value();
    let right = function_state.get_value(right)?.get_value();
    generate_ptr_binop(
        global_state,
        function_state,
        usize::from(type_size) as u64,
        left,
        right,
        op,
    )
}

pub(super) fn generate_integer_unop<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    op: LMIRIntUnOp,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    Some(CodegenValue::Value(match op {
        LMIRIntUnOp::NEG => function_state
            .builder
            .build_int_neg(value, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
        LMIRIntUnOp::BNOT => function_state
            .builder
            .build_not(value, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
        LMIRIntUnOp::LNOT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                value,
                value.get_type().const_int(0, false),
                inst_num().as_str(),
            )
            .unwrap()
            .as_any_value_enum(),
    }))
}

pub(super) fn generate_integer_binop<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    left: &LMIRValue,
    right: &LMIRValue,
    op: LMIRIntBinOp,
) -> Option<CodegenValue<'a>> {
    let left = function_state.get_value(left)?.get_value().into_int_value();
    let right = function_state
        .get_value(right)?
        .get_value()
        .into_int_value();
    generate_int_binop(global_state, function_state, left, right, op)
}

pub(super) fn generate_float_unop<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    op: LMIRFloatUnOp,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_float_value();
    Some(CodegenValue::Value(match op {
        LMIRFloatUnOp::NEG => function_state
            .builder
            .build_float_neg(value, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
    }))
}

pub(super) fn generate_float_binop<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    left: &LMIRValue,
    right: &LMIRValue,
    op: LMIRFloatBinOp,
) -> Option<CodegenValue<'a>> {
    let left = function_state
        .get_value(left)?
        .get_value()
        .into_float_value();
    let right = function_state
        .get_value(right)?
        .get_value()
        .into_float_value();
    Some(CodegenValue::Value(match op {
        LMIRFloatBinOp::ADD => function_state
            .builder
            .build_float_add(left, right, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
        LMIRFloatBinOp::SUB => function_state
            .builder
            .build_float_sub(left, right, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
        LMIRFloatBinOp::FMUL => function_state
            .builder
            .build_float_mul(left, right, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
        LMIRFloatBinOp::FDIV => function_state
            .builder
            .build_float_div(left, right, inst_num().as_str())
            .unwrap()
            .as_any_value_enum(),
        LMIRFloatBinOp::EQ
        | LMIRFloatBinOp::NEQ
        | LMIRFloatBinOp::FLT
        | LMIRFloatBinOp::FLE
        | LMIRFloatBinOp::FGT
        | LMIRFloatBinOp::FGE => {
            let predicate = match op {
                LMIRFloatBinOp::EQ => inkwell::FloatPredicate::OEQ,
                LMIRFloatBinOp::NEQ => inkwell::FloatPredicate::ONE,
                LMIRFloatBinOp::FLT => inkwell::FloatPredicate::OLT,
                LMIRFloatBinOp::FLE => inkwell::FloatPredicate::OLE,
                LMIRFloatBinOp::FGT => inkwell::FloatPredicate::OGT,
                LMIRFloatBinOp::FGE => inkwell::FloatPredicate::OGE,
                _ => unreachable!(),
            };
            function_state
                .builder
                .build_float_compare(predicate, left, right, inst_num().as_str())
                .unwrap()
                .as_any_value_enum()
        }
    }))
}

pub(super) fn generate_bit_cast<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state.get_value(value)?.get_value();
    if let AnyValueEnum::PointerValue(value) = value {
        return Some(CodegenValue::Value(value.as_any_value_enum()));
    }
    let value = any_to_basic_val(value)?;
    let target = any_to_basic_type(bc_llvm_type(global_state.context, target_type)?)?;
    let value = function_state
        .builder
        .build_bit_cast(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_int_to_ptr<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let value = function_state
        .builder
        .build_int_to_ptr(
            value,
            global_state.context.ptr_type(AddressSpace::from(0)),
            inst_num().as_str(),
        )
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_zextend<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_int_z_extend(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_sextend<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_int_s_extend(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_trunc<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_int_truncate(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_int_to_float<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
    sextend: bool,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_float_type();
    let value = if sextend {
        function_state
            .builder
            .build_signed_int_to_float(value, target, inst_num().as_str())
            .unwrap()
    } else {
        function_state
            .builder
            .build_unsigned_int_to_float(value, target, inst_num().as_str())
            .unwrap()
    };
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_float_to_int<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
    sextend: bool,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_float_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = if sextend {
        function_state
            .builder
            .build_float_to_signed_int(value, target, inst_num().as_str())
            .unwrap()
    } else {
        function_state
            .builder
            .build_float_to_unsigned_int(value, target, inst_num().as_str())
            .unwrap()
    };
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_ptr_to_int<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_pointer_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_ptr_to_int(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_float_cast<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_float_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_float_type();
    let value = function_state
        .builder
        .build_float_cast(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}
