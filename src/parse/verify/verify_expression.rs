use cranelift::codegen::ir;
use crate::log_error;
use crate::parse::ast::{ControlExpression, Expression, LValueExpression, LiteralExpression, RValueExpression, ValueType};
use crate::parse::verify::bytecode::{BytecodeBuilder, ValueID, VirtualInstruction, VirtualValue};
use crate::parse::verify::context::VerifyContext;

pub(crate) fn verify_expression(context: &mut VerifyContext, builder: &mut BytecodeBuilder,
                                expression: &Expression) -> Option<ValueID> {
    match expression {
        Expression::Literal(literal) =>
            verify_literal(context, builder, literal),

        Expression::Control(control) =>
            verify_control(context, builder, control),

        Expression::LValue(lvalue) =>
            verify_lvalue(context, builder, lvalue),

        Expression::RValue(rvalue) =>
            verify_rvalue(context, builder, rvalue),

        Expression::Unit =>
            builder.add_instruction(
                VirtualInstruction::NOP,
                ValueType::Unit
            )
    }
}

pub(crate) fn verify_literal(_: &mut VerifyContext, builder: &mut BytecodeBuilder, literal: &LiteralExpression) -> Option<ValueID> {
    match literal {
        LiteralExpression::IntLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::Literal { val: *val as u64 },
                ValueType::Integer { bytes: *bytes, signed: true }
            )
        },

        LiteralExpression::FloatLiteral { val, bytes } => {
            let as_bytes = unsafe { std::mem::transmute::<f64, [u8; 8]>(*val) };

            builder.add_instruction(
                VirtualInstruction::Literal { val: u64::from_le_bytes(as_bytes) },
                ValueType::Float { bytes: *bytes }
            )
        },

        LiteralExpression::StringLiteral(val) => {
            let str_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                VirtualInstruction::StringLiteral { str_id },
                ValueType::PointerTo(Box::new(ValueType::Integer { bytes: 8, signed: false }))
            )
        }
    }
}

pub(crate) fn verify_rvalue(context: &mut VerifyContext, builder: &mut BytecodeBuilder, rvalue: &RValueExpression) -> Option<ValueID> {
    match rvalue {
        RValueExpression::DirectFunctionCall {
            name, args
        } => {
            let args = args.iter()
                .map(|arg| verify_expression(context, builder, arg))
                .collect::<Option<Vec<_>>>()?;

            let Some(function) = context.get_function(name.as_str()) else {
                log_error!("Function {} not found", name);
            };

            builder.add_instruction(
                VirtualInstruction::DirectCall { function: name.clone(), args },
                function.return_type.clone()
            )
        },

        RValueExpression::BinaryOperation {
            operator, left, right
        } => {
            let left = verify_expression(context, builder, left)?;
            let right = verify_expression(context, builder, right)?;

            let Some(left_type) = builder.get_type(left) else {
                log_error!("Type not found for {:?}", left);
            };
            let Some(right_type) = builder.get_type(right) else {
                log_error!("Type not found for {:?}", right);
            };

            let (left, right, result_type) = coerce_bin_op(context, builder, left, right, left_type.clone(), right_type.clone())?;

            match result_type {
                ValueType::Integer { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            op: *operator,
                            left,
                            right
                        },
                        result_type
                    )
                },

                ValueType::Float { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::FloatBinOp {
                            op: *operator,
                            left,
                            right
                        },
                        result_type
                    )
                },

                _ => unimplemented!("{:?}", result_type)
            }
        },

        RValueExpression::Identifier(name) => {
            let Some(id) = context.get_variable(name.as_str()) else {
                log_error!("Variable {} not found", name);
            };

            let Some(var_type) = builder.get_type(*id).cloned() else {
                log_error!("Variable {} not found in builder.get_type(*id)", name);
            };

            builder.add_instruction(
                VirtualInstruction::Load {
                    value: *id,
                    type_: var_type.clone()
                },
                var_type
            )
        },

        _ => unimplemented!("{:?}", rvalue)
    }
}

pub(crate) fn verify_lvalue(context: &mut VerifyContext, builder: &mut BytecodeBuilder, lvalue: &LValueExpression) -> Option<ValueID> {
    match lvalue {
        _ => unimplemented!("{:?}", lvalue)
    }
}

pub(crate) fn verify_control(context: &mut VerifyContext, builder: &mut BytecodeBuilder, control: &ControlExpression) -> Option<ValueID> {
    match control {
        ControlExpression::Return(expr) => {
            let value = match expr {
                Some(expr) => Some(verify_expression(context, builder, expr)?),
                None => None
            };

            builder.add_instruction(
                VirtualInstruction::Return { value },
                ValueType::Unit
            )
        },

        ControlExpression::If { condition, then, else_ } => {
            let condition = verify_expression(context, builder, condition)?;

            let then_block = builder.create_block();
            let merge_block = builder.create_block();
            let else_block = if !else_.is_empty() {
                Some(builder.create_block())
            } else {
                None
            };

            let branch = VirtualInstruction::Branch {
                condition,
                true_block: then_block,
                false_block: else_block.unwrap_or(merge_block)
            };

            let branch_id = builder.add_instruction(
                branch,
                ValueType::Unit
            )?;

            builder.set_current_block(then_block);
            then.iter()
                .map(|expr| verify_expression(context, builder, expr))
                .collect::<Option<Vec<_>>>()?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block },
                ValueType::Unit
            );

            if !else_.is_empty() {
                builder.set_current_block(else_block.unwrap());
                else_.iter()
                    .map(|expr| verify_expression(context, builder, expr))
                    .collect::<Option<Vec<_>>>()?;
                builder.add_instruction(
                    VirtualInstruction::Jump { target: merge_block },
                    ValueType::Unit
                );
            }

            builder.set_current_block(merge_block);
            Some(branch_id)
        }

        _ => unimplemented!("{:?}", control)
    }
}

pub(crate) fn coerce_bin_op(context: &mut VerifyContext, builder: &mut BytecodeBuilder,
                            left_id: ValueID, right_id: ValueID,
                            left_type: ValueType, right_type: ValueType) -> Option<(ValueID, ValueID, ValueType)> {
    match (&left_type, &right_type) {
        (ValueType::Integer { bytes: lbytes, signed: lsigned },
         ValueType::Integer { bytes: rbytes, signed: rsigned }) => {
            assert_eq!(lbytes, rbytes);
            assert_eq!(lsigned, rsigned);

            Some((left_id, right_id, left_type))
        },

        (ValueType::Float { bytes: lbytes },
         ValueType::Float { bytes: rbytes }) => {
            assert_eq!(lbytes, rbytes);

            Some((left_id, right_id, left_type))
        },

        (ValueType::Identifier(name), _) => {
            let left = context.get_type(name.as_str())?;

            coerce_bin_op(context, builder, left_id, right_id, left.clone(), right_type)
        },

        (_, ValueType::Identifier(name)) => {
            let right = context.get_type(name.as_str())?;

            coerce_bin_op(context, builder, left_id, right_id, left_type, right.clone())
        },

        _ => unimplemented!("{:?} {:?}", left_type, right_type)
    }
}