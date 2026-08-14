use super::support::inst_num;
use crate::arithmetic::{generate_int_binop, generate_ptr_binop};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::types::TypeSize;
use cx_lmir::{LMIRFloatBinOp, LMIRFloatUnOp, LMIRIntBinOp, LMIRIntUnOp, LMIRPtrBinOp, LMIRValue};
use inkwell::values::AnyValue;

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
