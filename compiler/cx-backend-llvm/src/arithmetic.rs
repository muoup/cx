use crate::typing::{any_to_basic_type, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_mir_data::types::MIRType;
use cx_mir_data::{MIRPtrBinOp, MIRIntBinOp};
use inkwell::values::{AnyValue, AnyValueEnum, IntValue};

pub(crate) fn generate_ptr_binop<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    ptr_type: &MIRType,
    left_value: AnyValueEnum<'a>,
    right_value: AnyValueEnum<'a>,
    op: MIRPtrBinOp,
) -> Option<CodegenValue<'a>> {
    let ptr_type = bc_llvm_type(global_state.context, ptr_type)?;

    Some(CodegenValue::Value(match op {
        MIRPtrBinOp::ADD => unsafe {
            let basic_type = any_to_basic_type(ptr_type).unwrap_or_else(|| {
                panic!("Expected a basic type for pointer addition, found: {ptr_type:?}")
            });

            function_state
                .builder
                .build_in_bounds_gep(
                    basic_type,
                    left_value.into_pointer_value(),
                    &[right_value.into_int_value()],
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum()
        },
        MIRPtrBinOp::SUB => unsafe {
            let basic_type = any_to_basic_type(ptr_type).unwrap_or_else(|| {
                panic!("Expected a basic type for pointer subtraction, found: {ptr_type:?}")
            });

            let negative = function_state
                .builder
                .build_int_neg(
                    right_value.into_int_value(),
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum();

            function_state
                .builder
                .build_in_bounds_gep(
                    basic_type,
                    left_value.into_pointer_value(),
                    &[negative.into_int_value()],
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum()
        },
        MIRPtrBinOp::EQ => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()
            .unwrap()
            .as_any_value_enum(),
        MIRPtrBinOp::NE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRPtrBinOp::LT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRPtrBinOp::LE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULE,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRPtrBinOp::GT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGT,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRPtrBinOp::GE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGE,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
    }))
}

pub(crate) fn generate_int_binop<'a, 'b>(
    _: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    left_value: IntValue<'a>,
    right_value: IntValue<'a>,
    op: MIRIntBinOp,
    signed: bool,
) -> Option<CodegenValue<'a>> {
    let inst_return = match op {
        MIRIntBinOp::ADD if signed => function_state
            .builder
            .build_int_nsw_add(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::ADD => function_state
            .builder
            .build_int_add(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::SUB if signed => function_state
            .builder
            .build_int_nsw_sub(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::SUB => function_state
            .builder
            .build_int_sub(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::MUL if signed => function_state
            .builder
            .build_int_nsw_mul(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::MUL => function_state
            .builder
            .build_int_mul(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::IDIV => function_state
            .builder
            .build_int_signed_div(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::UDIV => function_state
            .builder
            .build_int_unsigned_div(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::IREM => function_state
            .builder
            .build_int_signed_rem(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::UREM => function_state
            .builder
            .build_int_unsigned_rem(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::IGT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SGT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::UGT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::IGE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SGE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::UGE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::ILT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::ULT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::ILE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),

        MIRIntBinOp::ASHR => function_state
            .builder
            .build_right_shift(
                left_value,
                right_value,
                true,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::LSHR => function_state
            .builder
            .build_right_shift(
                left_value,
                right_value,
                false,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),

        MIRIntBinOp::SHL => function_state
            .builder
            .build_left_shift(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),

        MIRIntBinOp::BAND => function_state
            .builder
            .build_and(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::BOR => function_state
            .builder
            .build_or(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::BXOR => function_state
            .builder
            .build_xor(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::LAND => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::LOR => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::EQ => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::NE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        MIRIntBinOp::ULE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
    };

    Some(CodegenValue::Value(inst_return))
}
