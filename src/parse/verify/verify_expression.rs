use std::clone;
use cranelift::codegen::ir;
use crate::lex::token::OperatorType;
use crate::log_error;
use crate::parse::ast::{ControlExpression, Expression, LValueExpression, LiteralExpression, RValueExpression, ValueType, VarInitialization};
use crate::parse::verify::bytecode::{BytecodeBuilder, ValueID, VirtualInstruction, VirtualValue};
use crate::parse::verify::context::VerifyContext;
use crate::parse::verify::name_mangling::member_function_mangle;
use crate::parse::verify::special_exprs::{struct_assignment, struct_return};
use crate::parse::verify::verify_type::{get_intrinsic_type, get_type_size, struct_field_index, struct_field_offset};

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
            let Some(function) = context.get_function(name.as_str()).cloned() else {
                log_error!("Function {} not found", name);
            };

            let args = args.iter()
                .enumerate()
                .map(|(i, arg)| {
                    let expr = verify_expression(context, builder, arg)?;
                    let expr_type = builder.get_type(expr)?.clone();

                    coerce_type(context, builder, expr, expr_type, function.args[i].type_.clone())
                })
                .collect::<Option<Vec<_>>>()?;

            builder.add_instruction(
                context,
                VirtualInstruction::DirectCall { function: name.clone(), args },
                function.return_type.clone()
            )
        },

        RValueExpression::MemberFunctionCall {
            struct_parent, name, args
        } => {
            let struct_parent = verify_expression(context, builder, struct_parent)?;
            let ValueType::Identifier(struct_type) = builder.get_type(struct_parent)? else {
                log_error!("Expected identifier, found {:?}", struct_parent);
            };
            let mangled_name = member_function_mangle(struct_type.as_str(), name.as_str());

            let Some(func) = context.get_function(mangled_name.as_str()).cloned() else {
                log_error!("Function {} not found", name);
            };

            if func.args.len() != args.len() + 1 {
                log_error!("Expected {} arguments, found {}", func.args.len() - 1, args.len());
            }

            let mut args = args.iter()
                .enumerate()
                .map(|(i, arg)| {
                    let arg = verify_expression(context, builder, arg)?;
                    let expected_type = func.args[i + 1].type_.clone();

                    coerce_type(context, builder, arg, builder.get_type(arg)?.clone(), expected_type)
                })
                .collect::<Option<Vec<_>>>()?;

            args.insert(0, struct_parent);

            let struct_type = builder.get_type(struct_parent)?;

            let ValueType::Identifier(struct_name) = struct_type else {
                log_error!("Expected identifier, found {:?}", struct_type);
            };

            let fn_name = member_function_mangle(struct_name.as_str(), name.as_str());

            let Some(member_) = context.get_function(fn_name.as_str()) else {
                log_error!("Function {} not found", fn_name);
            };

            builder.add_instruction(
                context,
                VirtualInstruction::DirectCall { function: fn_name, args },
                member_.return_type.clone()
            )
        }

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

                ValueType::PointerTo { .. } => {
                    builder.add_instruction(
                        context,
                        VirtualInstruction::IntegerBinOp {
                            op: *operator,
                            left,
                            right
                        },
                        result_type
                    )
                }

                _ => unimplemented!("{:?}", result_type)
            }
        },

        RValueExpression::UnaryOperation {
            operator, operand
        } => {
            match operator {
                OperatorType::Multiply => {
                    let Some(operand) = operand else {
                        log_error!("Anonymous unary operations not allowed in expression bodies")
                    };

                    let operand = verify_expression(context, builder, operand.as_ref())?;

                    let type_ = match builder.get_type(operand)? {
                        ValueType::PointerTo(inner) => {
                            inner.as_ref()
                        },

                        _ => unimplemented!()
                    };

                    builder.add_instruction(
                        context,
                        VirtualInstruction::Load {
                            value: operand
                        },
                        type_.clone()
                    )
                },

                _ => unimplemented!("{:?}", operator)
            }
        },

        RValueExpression::Identifier(name) => {
            let Some(id) = context.get_value(builder, name.as_str()) else {
                log_error!("Value {} not found", name);
            };

            let Some(var_type) = builder.get_type(id).cloned() else {
                log_error!("Variable {} not found in builder.get_type(*id)", name);
            };

            builder.add_instruction(
                context,
                VirtualInstruction::Load {
                    value: id
                },
                var_type
            )
        },

        RValueExpression::Assignment {
            operator, left, right
        } => {
            let left = verify_expression(context, builder, left)?;

            if matches!(get_intrinsic_type(&context.type_map, builder.get_type(left)?)?, ValueType::Structured { .. }) {
                return struct_assignment(context, builder, left, right.as_ref());
            }

            let right = verify_expression(context, builder, right)?;

            let left_type = builder.get_type(left).unwrap().clone();
            let right_type = builder.get_type(right).unwrap().clone();

            let (left, right, _) = coerce_bin_op(context, builder, left, right, left_type, right_type)?;

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
                VirtualInstruction::Store {
                    memory: left,
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

        LValueExpression::DereferencedPointer {
            pointer
        } => {
            let pointer = verify_expression(context, builder, pointer)?;
            let pointer_type = builder.get_type(pointer)?;

            let ValueType::PointerTo(inner) = pointer_type else {
                log_error!("Cannot dereference non-pointer type")
            };

            builder.add_instruction(
                context,
                VirtualInstruction::Load {
                    value: pointer
                },
                inner.as_ref().clone()
            )
        }

        LValueExpression::Identifier(name) => {
            let Some(var) = context.get_value(builder, name) else {
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

            let ValueType::Structured { fields } = intrin_type.clone() else {
                log_error!("Cannot access field of non-structured type!")
            };

            let index = struct_field_index(&fields, field_name.as_str())?;
            let offset = struct_field_offset(context, builder, &fields, field_name.as_str())?;

            builder.add_instruction(
                context,
                VirtualInstruction::StructAccess {
                    struct_,
                    field_index: index,
                    field_offset: offset,
                },
                fields[index].type_.clone()
            )
        },

        _ => unimplemented!("{:?}", lvalue)
    }
}

pub(crate) fn verify_control(context: &mut VerifyContext, builder: &mut BytecodeBuilder, control: &ControlExpression) -> Option<ValueID> {
    match control {
        ControlExpression::Return(expr) => {
            let value = match expr {
                Some(expr) => {
                    let expr = verify_expression(context, builder, expr)?;

                    if matches!(builder.get_type(expr)?, ValueType::Structured { .. }) {
                        return struct_return(context, builder, expr);
                    }

                    Some(expr)
                },
                None => None
            };

            builder.add_instruction(
                context,
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

            context.var_map.push_scope();
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
            context.var_map.pop_scope();

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
    if left_type == right_type {
        return Some((left_id, right_id, left_type));
    }

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

        // 64-bit Integer to Pointer
        (ValueType::PointerTo(_), ValueType::Integer { bytes: 8, .. }) => {
            Some((left_id, right_id, left_type))
        }

        // Non-64-bit Integer to Pointer
        (ValueType::PointerTo(_), ValueType::Integer { .. }) => {
            let r_type = builder.add_instruction(
                context,
                VirtualInstruction::ZExtend {
                    value: right_id,
                },
                left_type.clone()
            )?;

            Some((left_id, r_type, left_type))
        },

        (ValueType::PointerTo(_), ValueType::PointerTo(_)) => {
            Some((left_id, right_id, left_type))
        },

        _ => unimplemented!("{:?} {:?}", left_type, right_type)
    }
}

pub(crate) fn coerce_type(context: &mut VerifyContext, builder: &mut BytecodeBuilder,
                          expr_id: ValueID, expr_type: ValueType, goal_type: ValueType) -> Option<ValueID> {
    if expr_type == goal_type {
        return Some(expr_id);
    }

    match (&expr_type, &goal_type) {
        (ValueType::Integer { bytes: lbytes, signed: _ },
         ValueType::Integer { bytes: rbytes, signed: _ }) => {
            if lbytes < rbytes {
                let new_type = builder.add_instruction(
                    context,
                    VirtualInstruction::ZExtend {
                        value: expr_id,
                    },
                    goal_type.clone()
                )?;

                Some(new_type)
            } else {
                Some(expr_id)
            }
        },

        (ValueType::Identifier(name), _) => {
            let expr_type = context.get_type(name.as_str())?.clone();

            coerce_type(context, builder, expr_id, expr_type, goal_type)
        },

        (_, ValueType::Identifier(name)) => {
            let goal_type = context.get_type(name.as_str())?.clone();

            coerce_type(context, builder, expr_id, expr_type, goal_type)
        },

        (ValueType::PointerTo(_), ValueType::PointerTo(_)) => {
            Some(expr_id)
        },

        _ => unimplemented!("coerce_type: {:?} to {:?}", expr_type, goal_type)
    }
}