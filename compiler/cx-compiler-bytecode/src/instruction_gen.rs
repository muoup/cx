use cx_compiler_ast::parse::operators::comma_separated;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::value_type::{get_intrinsic_type, get_type_size, prototype_to_type, struct_field_access, CXValType};
use cx_data_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};
use cx_util::log_error;
use crate::implicit_cast::implicit_cast;

pub fn generate_instruction(
    builder: &mut BytecodeBuilder,
    expr: &CXExpr
) -> Option<ValueID> {
    match expr {
        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let lhs = generate_instruction(builder, lhs.as_ref())?;
            let rhs = generate_instruction(builder, rhs.as_ref())?;
            let assn_type = builder.get_type(lhs)?.clone();

            builder.add_instruction(
                VirtualInstruction::Store {
                    memory: lhs,
                    value: rhs,
                    type_: assn_type
                },
                CXValType::Unit
            )
        },
        CXExpr::VarDeclaration { name, type_ } => {
            let Some(type_size) = get_type_size(&builder.type_map, type_) else {
                log_error!("Invalid type for variable declaration: {type_}");
            };

            let memory = builder.add_instruction(
                VirtualInstruction::Allocate {
                    size: type_size
                },
                type_.clone()
            )?;

            builder.symbol_table.insert(name.to_owned(), memory);

            Some(memory)
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::Access } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let ltype = builder.get_type(left_id)?.clone();

            let CXExpr::Identifier(field_name) = rhs.as_ref() else {
                log_error!("Invalid field access: {rhs}");
            };

            let struct_access = struct_field_access(&builder.type_map, &ltype, field_name.as_str())?;

            builder.add_instruction(
                VirtualInstruction::StructAccess {
                    struct_: left_id,
                    field_offset: struct_access.field_offset,
                    field_index: struct_access.field_index
                },
                struct_access.field_type.clone()
            )
        },

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let rhs = comma_separated(rhs.as_ref());

            let mut args = vec![];
            for arg in rhs {
                let arg_id = generate_instruction(builder, arg)?;
                args.push(arg_id);
            }

            let CXValType::Function { return_type, .. } = builder.get_type(left_id)? else {
                log_error!("Invalid method call: {lhs}");
            };

            builder.add_instruction(
                VirtualInstruction::MethodCall {
                    func: left_id,
                    args
                },
                return_type.as_ref().clone()
            )
        },

        CXExpr::BinOp { lhs, rhs, op } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;
            let lhs_type = builder.get_type(left_id)?.clone();

            match get_intrinsic_type(&builder.type_map, &lhs_type)? {
                CXValType::Integer { .. } |
                CXValType::PointerTo { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            left: left_id,
                            right: right_id,
                            op: op.clone()
                        },
                        lhs_type
                    )
                },
                CXValType::Float { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::FloatBinOp {
                            left: left_id,
                            right: right_id,
                            op: op.clone(),
                        },
                        lhs_type
                    )
                },

                _type =>
                    panic!("Invalid arguments with type for binop not caught by type checker: {_type} \'{lhs}\' \'{rhs}\' {op:?} ")
            }
        },
        CXExpr::Block { exprs, value } => {
            for expr in exprs {
                generate_instruction(builder, expr)?;
            }

            if let Some(value) = value {
                return generate_instruction(builder, value.as_ref());
            }

            Some(ValueID::NULL)
        },
        CXExpr::ImplicitCast { expr, from_type, to_type} => {
            let inner = generate_instruction(builder, expr.as_ref())?;

            implicit_cast(builder, inner, &from_type, &to_type)
        },
        CXExpr::ImplicitLoad { expr, loaded_type } => {
            let inner = generate_instruction(builder, expr.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Load {
                    value: inner
                },
                loaded_type.clone()
            )
        },

        CXExpr::IntLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::Immediate {
                    value: *val as i32
                },
                CXValType::Integer {
                    bytes: *bytes,
                    signed: true
                }
            )
        },

        CXExpr::Identifier(val) => {
            if let Some(id) = builder.symbol_table.get(val.as_str()) {
                Some(id.clone())
            } else if let Some(func) = builder.fn_map.get(val.as_str()) {
                builder.add_instruction(
                    VirtualInstruction::FunctionReference {
                        name: val.to_owned()
                    },
                    prototype_to_type(func)?
                )
            } else {
                log_error!("Unknown identifier {val}")
            }
        },

        CXExpr::Return { value } => {
            let value = match value {
                Some(value) => generate_instruction(builder, value.as_ref()),
                None => None
            };

            builder.add_instruction(
                VirtualInstruction::Return { value },
                CXValType::Unit
            )
        },

        CXExpr::StringLiteral { val, .. } => {
            let string_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                VirtualInstruction::StringLiteral {
                    str_id: string_id,
                },
                CXValType::PointerTo(Box::new(CXValType::Identifier(CXIdent::from("char"))))
            )
        },

        CXExpr::UnOp { operator, operand } => {
            match operator {
                CXUnOp::Dereference => {
                    generate_instruction(builder, operand.as_ref())
                },
                CXUnOp::AddressOf => {
                    let value = generate_instruction(builder, operand.as_ref())?;

                    builder.add_instruction(
                        VirtualInstruction::AddressOf { value },
                        CXValType::PointerTo(Box::new(builder.get_type(value)?.clone()))
                    )
                },
                CXUnOp::PreIncrement(off) => {
                    let value = generate_instruction(builder, operand.as_ref())?;
                    let val_type = builder.get_type(value)?.clone();
                    let value = builder.add_instruction(
                        VirtualInstruction::Load {
                            value: value.clone()
                        },
                        val_type.clone()
                    )?;

                    let one = builder.add_instruction(
                        VirtualInstruction::Immediate {
                            value: off.clone() as i32
                        },
                        val_type.clone()
                    )?;

                    let incremented = builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            left: value,
                            right: one,
                            op: CXBinOp::Add
                        },
                        val_type.clone()
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: value,
                            value: incremented.clone(),
                            type_: val_type
                        },
                        CXValType::Unit
                    )?;

                    Some(incremented)
                },
                CXUnOp::PostIncrement(off) => {
                    let value = generate_instruction(builder, operand.as_ref())?;
                    let val_type = builder.get_type(value)?.clone();
                    let loaded_val = builder.add_instruction(
                        VirtualInstruction::Load {
                            value: value.clone()
                        },
                        val_type.clone()
                    )?;

                    let one = builder.add_instruction(
                        VirtualInstruction::Immediate {
                            value: off.clone() as i32
                        },
                        val_type.clone()
                    )?;

                    let incremented = builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            left: loaded_val,
                            right: one,
                            op: CXBinOp::Add
                        },
                        val_type.clone()
                    )?;
                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: value,
                            value: incremented,
                            type_: val_type
                        },
                        CXValType::Unit
                    )?;

                    Some(loaded_val)
                },

                _ => todo!("generate_instruction for {:?}", operator)
            }
        },

        CXExpr::If { condition, then_branch, else_branch } => {
            let condition = generate_instruction(builder, condition.as_ref())?;

            let then_block = builder.create_block();
            let else_block = builder.create_block();
            let merge_block = builder.create_block();

            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition,
                    true_block: then_block.clone(),
                    false_block:
                        match else_branch {
                            Some(_) => else_block.clone(),
                            None => merge_block.clone()
                        }
                },
                CXValType::Unit
            );

            builder.set_current_block(then_block);
            generate_instruction(builder, then_branch.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block.clone() },
                CXValType::Unit
            );

            if let Some(else_branch) = else_branch {
                builder.set_current_block(else_block);
                generate_instruction(builder, else_branch.as_ref());
                builder.add_instruction(
                    VirtualInstruction::Jump { target: merge_block.clone() },
                    CXValType::Unit
                );
            }

            builder.set_current_block(merge_block);

            Some(ValueID::NULL)
        },

        CXExpr::While { condition, body } => {
            let condition_block = builder.create_block();
            let body_block = builder.create_block();
            let merge_block = builder.create_block();

            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::Unit
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block.clone(),
                    false_block: merge_block.clone()
                },
                CXValType::Unit
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::Unit
            );

            builder.set_current_block(merge_block);

            Some(ValueID::NULL)
        },

        CXExpr::For { init, condition, increment, body } => {
            let init_block = builder.create_block();
            let condition_block = builder.create_block();
            let body_block = builder.create_block();
            let increment_block = builder.create_block();
            let merge_block = builder.create_block();

            generate_instruction(builder, init.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::Unit
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block.clone(),
                    false_block: merge_block.clone()
                },
                CXValType::Unit
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: increment_block.clone() },
                CXValType::Unit
            );

            builder.set_current_block(increment_block);
            generate_instruction(builder, increment.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::Unit
            );

            builder.set_current_block(merge_block);

            Some(ValueID::NULL)
        },

        _ => todo!("generate_instruction for {:?}", expr)
    }
}