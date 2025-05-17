use std::clone;
use std::mem::forget;
use cx_compiler_ast::parse::operators::comma_separated;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXFunctionPrototype, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::value_type::{get_intrinsic_type, get_type_size, prototype_to_type, struct_field_access, CXTypeUnion, CXValType, CX_CONST};
use cx_data_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};
use cx_data_bytecode::types::type_to_prototype;
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
                CXValType::unit()
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

        CXExpr::BinOp { lhs, rhs, op: CXBinOp::ArrayIndex } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;

            let intrinsic_type = builder.get_type(left_id)?
                .intrinsic_type(&builder.type_map)?.clone();
            let lhs_inner = match intrinsic_type {
                CXTypeUnion::PointerTo(inner) => inner,
                CXTypeUnion::Array { _type, .. } => _type,
                _ => panic!("Invalid array index type: {intrinsic_type}"),
            };

            builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: CXBinOp::Add
                },
                CXValType::new(
                    0,
                    CXTypeUnion::MemoryAlias(lhs_inner)
                )
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

            let method_sig = builder.get_type(left_id)?.clone();

            match get_intrinsic_type(&builder.type_map, &method_sig)? {
                CXTypeUnion::Function { return_type, args: _ } => {
                    builder.add_instruction(
                        VirtualInstruction::DirectCall {
                            func: left_id,
                            args,
                            method_sig: type_to_prototype(
                                &builder.type_map,
                                &method_sig
                            )
                        },
                        return_type.as_ref().clone()
                    )
                },
                CXTypeUnion::PointerTo(inner) => {
                    let Some(CXTypeUnion::Function { return_type, args: _ }) =
                        inner.intrinsic_type(&builder.type_map) else {
                        log_error!("Invalid function pointer type: {inner}");
                    };

                    builder.add_instruction(
                        VirtualInstruction::IndirectCall {
                            func_ptr: left_id,
                            args,
                            method_sig: type_to_prototype(
                                &builder.type_map,
                                inner.as_ref()
                            )
                        },
                        return_type.as_ref().clone()
                    )
                },

                type_ => log_error!("Invalid function pointer type: {type_}")
            }
        },

        CXExpr::BinOp { lhs, rhs, op } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;
            let lhs_type = builder.get_type(left_id)?.clone();

            match get_intrinsic_type(&builder.type_map, &lhs_type)? {
                CXTypeUnion::Integer { .. } |
                CXTypeUnion::PointerTo { .. } => {
                    builder.add_instruction(
                        VirtualInstruction::IntegerBinOp {
                            left: left_id,
                            right: right_id,
                            op: op.clone()
                        },
                        lhs_type
                    )
                },
                CXTypeUnion::Float { .. } => {
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
        CXExpr::ImplicitCast { expr, from_type, to_type, cast_type} => {
            let inner = generate_instruction(builder, expr.as_ref())?;

            implicit_cast(builder, inner, &from_type, &to_type, cast_type)
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
        CXExpr::GetFunctionAddr { func_name, func_sig } => {
            let func_name = generate_instruction(builder, func_name.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::GetFunctionAddr {
                    func_name
                },
                func_sig.clone()
            )
        },

        CXExpr::IntLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::Immediate {
                    value: *val as i32
                },
                CXValType::new(
                    0,
                    CXTypeUnion::Integer {
                        bytes: *bytes,
                        signed: true
                    }
                )
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

            let final_block = builder.create_block();
            builder.add_instruction(
                VirtualInstruction::Jump { target: final_block.clone() },
                CXValType::unit()
            );

            builder.set_current_block(final_block);
            builder.add_instruction(
                VirtualInstruction::Return { value },
                CXValType::unit()
            )
        },

        CXExpr::StringLiteral { val, .. } => {
            let string_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                VirtualInstruction::StringLiteral {
                    str_id: string_id,
                },

                CXTypeUnion::PointerTo(
                    Box::new(
                        CXValType::new(
                            CX_CONST,
                            CXTypeUnion::Identifier(CXIdent::from("char"))
                        )
                    )
                ).to_val_type()
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
                        CXTypeUnion::PointerTo(
                            Box::new(
                                builder.get_type(value)?.clone()
                            )
                        ).to_val_type()
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
                        CXValType::unit()
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
                        CXValType::unit()
                    )?;

                    Some(loaded_val)
                },

                CXUnOp::ExplicitCast(_) =>
                    generate_instruction(builder, operand.as_ref()),

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
                    false_block: else_block.clone()
                },
                CXValType::unit()
            );

            builder.set_current_block(then_block);
            generate_instruction(builder, then_branch.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block.clone() },
                CXValType::unit()
            );

            builder.set_current_block(else_block);
            if let Some(else_branch) = else_branch {
                generate_instruction(builder, else_branch.as_ref());
            }
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block.clone() },
                CXValType::unit()
            );

            builder.set_current_block(merge_block);
            Some(ValueID::NULL)
        },

        CXExpr::While { condition, body, pre_eval } => {
            let condition_block = builder.start_cond_point();
            let body_block = builder.create_block();
            let merge_block = builder.start_scope();

            let first_block = if *pre_eval {
                condition_block.clone()
            } else {
                body_block.clone()
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: first_block },
                CXValType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block.clone(),
                    false_block: merge_block.clone()
                },
                CXValType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::unit()
            );

            builder.end_scope();
            builder.end_cond();
            Some(ValueID::NULL)
        },

        CXExpr::For { init, condition, increment, body } => {
            let condition_block = builder.start_cond_point();
            let body_block = builder.create_block();
            let increment_block = builder.create_block();
            let merge_block = builder.start_scope();

            generate_instruction(builder, init.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block.clone(),
                    false_block: merge_block.clone()
                },
                CXValType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: increment_block.clone() },
                CXValType::unit()
            );

            builder.set_current_block(increment_block);
            generate_instruction(builder, increment.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXValType::unit()
            );

            builder.end_scope();
            builder.end_cond();
            Some(ValueID::NULL)
        },

        CXExpr::Break => {
            let Some(merge) = builder.get_merge() else {
                log_error!("TYPE ERROR: Invalid break in outermost scope");
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: merge },
                CXValType::unit()
            );

            Some(ValueID::NULL)
        },

        CXExpr::Continue => {
            let Some(cond) = builder.get_continue() else {
                log_error!("TYPE ERROR: Invalid continue in outermost scope");
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: cond },
                CXValType::unit()
            );

            Some(ValueID::NULL)
        },

        _ => todo!("generate_instruction for {:?}", expr)
    }
}

pub(crate) fn implicit_return(
    builder: &mut BytecodeBuilder,
    prototype: &CXFunctionPrototype,
) -> Option<()> {
    let last_instruction = builder.last_instruction();

    if let Some(last_instruction) = last_instruction {
        if let VirtualInstruction::Return { .. } = last_instruction.instruction {
            return Some(());
        }
    }

    let return_block = builder.create_block();
    builder.add_instruction(
        VirtualInstruction::Jump {
            target: return_block.clone()
        },
        CXValType::unit()
    );
    builder.set_current_block(return_block);

    if prototype.name.data == "main" {
        let zero = builder.add_instruction(
            VirtualInstruction::Immediate {
                value: 0
            },
            CXTypeUnion::Integer { bytes: 4, signed: true }.to_val_type()
        )?;

        builder.add_instruction(
            VirtualInstruction::Return {
                value: Some(zero)
            },
            CXValType::unit()
        );
    } else if prototype.return_type.is_void(&builder.type_map) {
        builder.add_instruction(
            VirtualInstruction::Return {
                value: None
            },
            CXValType::unit()
        )?;
    } else {
        let last_instruction = builder.last_instruction();
        log_error!("Function {} has a return type but no return statement\nLast Statement found: {:?}", prototype.name, last_instruction);
    }

    Some(())
}