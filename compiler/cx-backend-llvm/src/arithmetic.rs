use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRPtrBinOp, LMIRIntBinOp};
use inkwell::values::{AnyValue, AnyValueEnum, IntValue};

pub(crate) fn generate_ptr_binop<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    type_padded_size: u64,
    left_value: AnyValueEnum<'a>,
    right_value: AnyValueEnum<'a>,
    op: LMIRPtrBinOp,
) -> Option<CodegenValue<'a>> {
    Some(CodegenValue::Value(match op {
        LMIRPtrBinOp::ADD => unsafe {
            let i8_type = global_state.context.i8_type();
            let scaled_right = function_state
                .builder
                .build_int_mul(
                    right_value.into_int_value(),
                    global_state.context
                        .i64_type()
                        .const_int(type_padded_size, false),
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum();

            function_state
                .builder
                .build_in_bounds_gep(
                    i8_type,
                    left_value.into_pointer_value(),
                    &[scaled_right.into_int_value()],
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum()
        },
        LMIRPtrBinOp::SUB => unsafe {
            let negative = function_state
                .builder
                .build_int_neg(
                    right_value.into_int_value(),
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum();
            
            let scaled_right = function_state
                .builder
                .build_int_mul(
                    negative.into_int_value(),
                    global_state.context
                        .i64_type()
                        .const_int(type_padded_size, false),
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum();

            function_state
                .builder
                .build_in_bounds_gep(
                    global_state.context.i8_type(),
                    left_value.into_pointer_value(),
                    &[scaled_right.into_int_value()],
                    crate::instruction::inst_num().as_str(),
                )
                .ok()?
                .as_any_value_enum()
        },
        LMIRPtrBinOp::EQ => function_state
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
        LMIRPtrBinOp::NE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRPtrBinOp::LT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRPtrBinOp::LE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULE,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRPtrBinOp::GT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGT,
                left_value.into_pointer_value(),
                right_value.into_pointer_value(),
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRPtrBinOp::GE => function_state
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
    op: LMIRIntBinOp
) -> Option<CodegenValue<'a>> {
    let inst_return = match op {
        // LMIRIntBinOp::ADD if signed => function_state
        //     .builder
        //     .build_int_nsw_add(
        //         left_value,
        //         right_value,
        //         crate::instruction::inst_num().as_str(),
        //     )
        //     .ok()?
        //     .as_any_value_enum(),
        LMIRIntBinOp::ADD => function_state
            .builder
            .build_int_add(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        // LMIRIntBinOp::SUB if signed => function_state
        //     .builder
        //     .build_int_nsw_sub(
        //         left_value,
        //         right_value,
        //         crate::instruction::inst_num().as_str(),
        //     )
        //     .ok()?
        //     .as_any_value_enum(),
        LMIRIntBinOp::SUB => function_state
            .builder
            .build_int_sub(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::IMUL => function_state
            .builder
            .build_int_nsw_mul(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::MUL => function_state
            .builder
            .build_int_mul(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::IDIV => function_state
            .builder
            .build_int_signed_div(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::UDIV => function_state
            .builder
            .build_int_unsigned_div(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::IREM => function_state
            .builder
            .build_int_signed_rem(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::UREM => function_state
            .builder
            .build_int_unsigned_rem(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::IGT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SGT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::UGT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::IGE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SGE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::UGE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::UGE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::ILT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::ULT => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::ULT,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::ILE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::SLE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),

        LMIRIntBinOp::ASHR => function_state
            .builder
            .build_right_shift(
                left_value,
                right_value,
                true,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::LSHR => function_state
            .builder
            .build_right_shift(
                left_value,
                right_value,
                false,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),

        LMIRIntBinOp::SHL => function_state
            .builder
            .build_left_shift(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),

        LMIRIntBinOp::BAND => function_state
            .builder
            .build_and(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::BOR => function_state
            .builder
            .build_or(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::BXOR => function_state
            .builder
            .build_xor(
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::LAND => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::LOR => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::EQ => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::NE => function_state
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                left_value,
                right_value,
                crate::instruction::inst_num().as_str(),
            )
            .ok()?
            .as_any_value_enum(),
        LMIRIntBinOp::ULE => function_state
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
