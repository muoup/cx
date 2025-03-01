use crate::log_error;
use crate::parse::ast::{Expression, RValueExpression, ValueType};
use crate::parse::verify::bytecode::{BlockInstruction, BytecodeBuilder, ValueID, VirtualInstruction};
use crate::parse::verify::context::VerifyContext;
use crate::parse::verify::verify_expression::{verify_expression, verify_rvalue};
use crate::parse::verify::verify_type::same_type;

pub(crate) fn struct_assignment(context: &mut VerifyContext, builder: &mut BytecodeBuilder, struct_pointer: ValueID, rval: &Expression) -> Option<ValueID> {
    match rval {
        Expression::RValue(
            RValueExpression::DirectFunctionCall { name, args }
        ) => {
            let fn_ret_type = context.get_function(name)?.return_type.clone();

            if !same_type(&context.type_map, builder.get_type(struct_pointer)?, &fn_ret_type) {
                log_error!("Function call has wrong return type: {:?}", rval);
            }

            // Store hidden pointer to struct in first argument
            let mut call_args = Vec::new();
            call_args.push(struct_pointer);

            for arg in args {
                let arg = verify_expression(context, builder, arg)?;
                call_args.push(arg);
            }

            builder.add_instruction(
                context,
                VirtualInstruction::DirectCall {
                    function: name.clone(),
                    args: call_args
                },
                fn_ret_type
            )
        },

        _ => log_error!("Cannot assign struct to expression: {:?}", rval)
    }
}

pub(crate) fn struct_return(context: &mut VerifyContext, builder: &mut BytecodeBuilder, struct_pointer: ValueID) -> Option<ValueID> {
    let dest = builder.add_instruction(
        context,
        VirtualInstruction::FunctionParameter {
            param_index: 0
        },
        ValueType::PointerTo(Box::new(ValueType::Unit))
    )?;

    let struct_type = builder.get_type(struct_pointer)?.clone();

    builder.add_instruction(
        context,
        VirtualInstruction::Store {
            value: struct_pointer,
            memory: dest
        },
        struct_type
    );

    builder.add_instruction(
        context,
        VirtualInstruction::Return {
            value: Some(dest)
        },
        context.current_return_type.clone()?
    )
}