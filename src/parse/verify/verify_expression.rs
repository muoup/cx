use cranelift::codegen::ir;
use crate::log_error;
use crate::parse::ast::{ControlExpression, Expression, LValueExpression, LiteralExpression, RValueExpression, ValueType, VarInitialization};
use crate::parse::verify::bytecode::{BytecodeBuilder, ValueID, VirtualInstruction, VirtualValue};
use crate::parse::verify::context::VerifyContext;
use crate::parse::verify::verify_type::{get_intrinsic_type, get_type_size};

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
                context,
                VirtualInstruction::NOP,
                ValueType::Unit
            )
    }
}

pub(crate) fn verify_literal(context: &mut VerifyContext, builder: &mut BytecodeBuilder, literal: &LiteralExpression) -> Option<ValueID> {
    match literal {
        LiteralExpression::IntLiteral { val, bytes } => {
            builder.add_instruction(
                context,
                VirtualInstruction::Literal { val: *val as u64 },
                ValueType::Integer { bytes: *bytes, signed: true }
            )
        },

        LiteralExpression::FloatLiteral { val, bytes } => {
            let as_bytes = unsafe { std::mem::transmute::<f64, [u8; 8]>(*val) };

            builder.add_instruction(
                context,
                VirtualInstruction::Literal { val: u64::from_le_bytes(as_bytes) },
                ValueType::Float { bytes: *bytes }
            )
        },

        LiteralExpression::StringLiteral(val) => {
            let str_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                context,
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
                .map(|arg| {
                    verify_expression(context, builder, arg)
                })
                .collect::<Option<Vec<_>>>()?;

            let Some(function) = context.get_function(name.as_str()) else {
                log_error!("Function {} not found", name);
            };

            builder.add_instruction(
                context,
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
                        context,
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
                        context,
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
                context,
                VirtualInstruction::Load {
                    value: *id
                },
                var_type
            )
        },

        RValueExpression::Assignment {
            operator, left, right
        } => {
            let left = verify_expression(context, builder, left)?;
            let right = verify_expression(context, builder, right)?;

            let right = match operator {
                Some(op) => {
                    let left_type = builder.get_type(left)?.clone();
                    let loaded_left = builder.add_instruction(
                        context,
                        VirtualInstruction::Load {
                            value: left
                        },
                        left_type.clone()
                    )?;

                    builder.add_instruction(
                        context,
                        VirtualInstruction::IntegerBinOp {
                            op: op.clone(),
                            left: loaded_left,
                            right
                        },
                        left_type
                    )?
                },
                None => right
            };

            builder.add_instruction(
                context,
                VirtualInstruction::Assign {
                    target: left,
                    value: right
                },
                builder.get_type(right)?.clone()
            )
        },

        RValueExpression::LoadedLValue {
            lvalue
        } => {
            let lvalue = verify_expression(context, builder, lvalue)?;
            let lvalue_type = builder.get_type(lvalue)?.clone();

            // Structures cannot be loaded; where they would be (i.e. parameters)
            // must be handled as a special case
            if matches!(lvalue_type, ValueType::Structured { .. }) {
                return Some(lvalue);
            }

            let loaded = builder.add_instruction(
                context,
                VirtualInstruction::Load {
                    value: lvalue
                },
                lvalue_type
            )?;

            Some(loaded)
        }

        _ => unimplemented!("{:?}", rvalue)
    }
}

pub(crate) fn verify_lvalue(context: &mut VerifyContext, builder: &mut BytecodeBuilder, lvalue: &LValueExpression) -> Option<ValueID> {
    match lvalue {
        LValueExpression::Initialization(
            VarInitialization { name, type_ }
        ) => {
            let size = get_type_size(&context.type_map, type_)?;
            let alloc = builder.add_instruction(
                context,
                VirtualInstruction::Allocate {
                    size
                },
                type_.clone()
            )?;

            context.insert_variable(name.clone(), alloc);

            Some(alloc)
        },

        LValueExpression::Identifier(name) => {
            let Some(var) = context.get_variable(name) else {
                log_error!("Variable not found: {}", name)
            };

            Some(var.clone())
        }

        LValueExpression::StructField {
            struct_, field_name
        } => {
            let struct_ = verify_expression(context, builder, struct_)?;
            let struct_type = builder.get_type(struct_)?;
            let intrin_type = get_intrinsic_type(&context.type_map, struct_type)?;

            let ValueType::Structured { fields } = intrin_type else {
                log_error!("Cannot access field of non-structured type!")
            };

            let mut size_counter = 0usize;

            for (i, field) in fields.iter().enumerate() {
                if &field.name == field_name {
                    return builder.add_instruction(
                        context,
                        VirtualInstruction::StructAccess {
                            struct_,
                            field_index: i,
                            field_offset: size_counter,
                        },
                        field.type_.clone()
                    )
                }
            }

            log_error!("Unknown field {} in struct", field_name)
        },

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
                context,
                VirtualInstruction::Return { value },
                ValueType::Unit
            )
        },

        ControlExpression::If { condition, then, else_ } => {
            context.var_map.push_scope();

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
                context,
                branch,
                ValueType::Unit
            )?;

            builder.set_current_block(then_block);
            context.var_map.push_scope();
            then.iter()
                .map(|expr| verify_expression(context, builder, expr))
                .collect::<Option<Vec<_>>>()?;
            context.var_map.pop_scope();
            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target: merge_block },
                ValueType::Unit
            );

            if !else_.is_empty() {
                builder.set_current_block(else_block.unwrap());
                context.var_map.push_scope();

                else_.iter()
                    .map(|expr| verify_expression(context, builder, expr))
                    .collect::<Option<Vec<_>>>()?;

                context.var_map.pop_scope();
                builder.add_instruction(
                    context,
                    VirtualInstruction::Jump { target: merge_block },
                    ValueType::Unit
                );
            }

            builder.set_current_block(merge_block);

            context.var_map.pop_scope();
            Some(branch_id)
        }

        ControlExpression::ForLoop {
            init, condition, increment, body
        } => {
            let init_block = builder.create_block();
            let condition_block = builder.create_block();
            let body_block = builder.create_block();
            let increment_block = builder.create_block();
            let merge_block = builder.create_block();

            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target: init_block },
                ValueType::Unit
            );

            builder.set_current_block(init_block);
            verify_expression(context, builder, init)?;
            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target: condition_block },
                ValueType::Unit
            );

            builder.set_current_block(condition_block);
            let condition = verify_expression(context, builder, condition)?;
            let branch = VirtualInstruction::Branch {
                condition,
                true_block: body_block,
                false_block: merge_block
            };
            let branch_id = builder.add_instruction(
                context,
                branch,
                ValueType::Unit
            )?;

            builder.set_current_block(body_block);
            body.iter()
                .map(|expr| verify_expression(context, builder, expr))
                .collect::<Option<Vec<_>>>()?;
            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target: increment_block },
                ValueType::Unit
            );

            builder.set_current_block(increment_block);
            verify_expression(context, builder, increment)?;
            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target: condition_block },
                ValueType::Unit
            );

            builder.set_current_block(merge_block);
            Some(branch_id)
        },

        ControlExpression::Loop {
            condition, body, evaluate_condition_first
        } => {
            let body_block = builder.create_block();
            let condition_block = builder.create_block();
            let merge_block = builder.create_block();

            let target = if *evaluate_condition_first {
                condition_block
            } else {
                body_block
            };

            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target },
                ValueType::Unit
            );

            builder.set_current_block(condition_block);
            let condition = verify_expression(context, builder, condition)?;
            let branch = VirtualInstruction::Branch {
                condition,
                true_block: body_block,
                false_block: merge_block
            };
            let branch_id = builder.add_instruction(
                context,
                branch,
                ValueType::Unit
            )?;

            builder.set_current_block(body_block);
            body.iter()
                .map(|expr| verify_expression(context, builder, expr))
                .collect::<Option<Vec<_>>>()?;
            builder.add_instruction(
                context,
                VirtualInstruction::Jump { target: condition_block },
                ValueType::Unit
            );

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
        (ValueType::Identifier(name1), ValueType::Identifier(name2))
        if name1 == name2
            => Some((left_id, right_id, left_type)),

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