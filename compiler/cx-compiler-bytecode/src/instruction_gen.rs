use cx_compiler_ast::parse::operators::comma_separated;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXFunctionPrototype, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::value_type::{get_type_size, CXTypeKind, CXType, CX_CONST};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCIntBinOp, BCIntUnOp, BCPtrBinOp, ValueID, VirtualInstruction};
use cx_util::log_error;
use crate::aux_routines::{get_struct_field, get_union_field};
use crate::builder::BytecodeBuilder;
use crate::cx_maps::convert_cx_type_kind;
use crate::implicit_cast::implicit_cast;

pub fn generate_instruction(
    builder: &mut BytecodeBuilder,
    expr: &CXExpr
) -> Option<ValueID> {
    match &expr.kind {
        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;
            let lhs_type = builder.get_expr_type(lhs.as_ref())?
                .clone();
            let CXTypeKind::MemoryAlias(inner) = lhs_type.intrinsic_type(&builder.cx_type_map)?
                else { unreachable!("generate_instruction: Expected memory alias type for expr, found {lhs_type}") };

            builder.add_instruction(
                VirtualInstruction::Store {
                    memory: left_id,
                    value: right_id,
                    type_: builder.convert_cx_type(inner.as_ref())?
                },
                CXType::unit()
            )
        },
        CXExprKind::VarDeclaration { name, type_ } => {
            let Some(type_size) = get_type_size(&builder.cx_type_map, type_) else {
                log_error!("Invalid type for variable declaration: {type_}");
            };

            let memory = builder.add_instruction_bt(
                VirtualInstruction::Allocate {
                    size: type_size
                },
                BCTypeKind::Pointer.into()
            )?;

            builder.symbol_table.insert(name.as_string(), memory);

            Some(memory)
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Access } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let lhs_type = builder.get_expr_intrinsic_type(lhs.as_ref())?
                .clone();
            let ltype = builder.convert_cx_type(&lhs_type.to_val_type())?;

            let CXExprKind::Identifier(field_name) = &rhs.as_ref().kind else {
                panic!("PANIC: Attempting to access struct field with rhs: {rhs:?}");
            };
            
            match ltype.kind {
                BCTypeKind::Struct { .. } => {
                    let struct_access = get_struct_field(
                        builder,
                        &ltype,
                        field_name.as_str()
                    ).unwrap_or_else(|| {
                        panic!("PANIC: Attempting to access non-existent field {field_name} in struct {ltype:?}");
                    });

                    builder.add_instruction_bt(
                        VirtualInstruction::StructAccess {
                            struct_: left_id,
                            struct_type: ltype,
                            field_offset: struct_access.offset,
                            field_index: struct_access.index,
                        },
                        struct_access._type
                    )
                },
                
                BCTypeKind::Union { .. } => Some(left_id),
                
                _ => unreachable!("generate_instruction: Expected structured type for access, found {ltype}")
            }
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::ArrayIndex } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;
            let l_type = builder.get_expr_intrinsic_type(lhs.as_ref())?.clone();

            let lhs_inner = match l_type {
                CXTypeKind::PointerTo(inner) => inner,
                CXTypeKind::Array { _type, .. } => _type,
                _ => panic!("Invalid array index type: {l_type}"),
            };
            
            let inner_as_bc = convert_cx_type_kind(&builder.cx_type_map, &lhs_inner.kind)?;

            builder.add_instruction(
                VirtualInstruction::PointerBinOp {
                    left: left_id,
                    right: right_id,
                    ptr_type: BCType::from(inner_as_bc),
                    op: BCPtrBinOp::ADD
                },
                CXType::new(
                    0,
                    CXTypeKind::MemoryAlias(lhs_inner)
                )
            )
        },

        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let rhs = comma_separated(rhs.as_ref());

            let mut args = vec![];
            for arg in rhs {
                let arg_id = generate_instruction(builder, arg)?;
                args.push(arg_id);
            }

            match builder.get_expr_intrinsic_type(lhs)?.clone() {
                CXTypeKind::Function { prototype } => {
                    builder.add_instruction(
                        VirtualInstruction::DirectCall {
                            func: left_id,
                            args,
                            method_sig: builder.convert_cx_prototype(prototype.as_ref())?
                        },
                        prototype.return_type.clone()
                    )
                },
                CXTypeKind::PointerTo(inner) => {
                    let Some(CXTypeKind::Function { prototype }) =
                        inner.intrinsic_type(&builder.cx_type_map) else {
                        log_error!("Invalid function pointer type: {inner}");
                    };

                    builder.add_instruction(
                        VirtualInstruction::IndirectCall {
                            func_ptr: left_id,
                            args,
                            method_sig: builder.convert_cx_prototype(prototype.as_ref())?
                        },
                        prototype.return_type.clone()
                    )
                },

                type_ => log_error!("Invalid function pointer type: {type_}")
            }
        },

        CXExprKind::BinOp { lhs, rhs, op } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;
            let cx_lhs_type = builder.get_expr_type(lhs.as_ref())?;
            
            generate_binop(
                builder,
                &cx_lhs_type,
                left_id,
                right_id,
                op
            )
        },
        CXExprKind::Block { exprs, value } => {
            for expr in exprs {
                generate_instruction(builder, expr)?;
            }

            if let Some(value) = value {
                return generate_instruction(builder, value.as_ref());
            }

            Some(ValueID::NULL)
        },
        CXExprKind::ImplicitCast { expr, from_type, to_type, cast_type} => {
            let inner = generate_instruction(builder, expr.as_ref())?;

            implicit_cast(builder, inner, &from_type, &to_type, cast_type)
        },
        CXExprKind::ImplicitLoad { expr, loaded_type } => {
            let inner = generate_instruction(builder, expr.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Load {
                    value: inner
                },
                loaded_type.clone()
            )
        },
        CXExprKind::GetFunctionAddr { func_name, func_sig } => {
            let func_name = generate_instruction(builder, func_name.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::GetFunctionAddr {
                    func_name
                },
                func_sig.clone()
            )
        },

        CXExprKind::IntLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::Immediate {
                    value: *val as i32
                },
                CXTypeKind::Integer {
                    bytes: *bytes,
                    signed: true
                }.to_val_type()
            )
        },
        
        CXExprKind::FloatLiteral { val, bytes } => {
            builder.add_instruction(
                VirtualInstruction::FloatImmediate {
                    value: *val
                },
                CXTypeKind::Float { bytes: *bytes }.to_val_type()
            )
        },

        CXExprKind::Identifier(val) => {
            if let Some(id) = builder.symbol_table.get(val.as_str()) {
                Some(id.clone())
            } else if builder.fn_map.contains_key(val.as_str()) {
                builder.add_instruction_bt(
                    VirtualInstruction::FunctionReference {
                        name: val.as_string()
                    },
                    BCTypeKind::Pointer.into()
                )
            } else {
                log_error!("Unknown identifier {val}")
            }
        },

        CXExprKind::Return { value } => {
            let value = match value {
                Some(value) => generate_instruction(builder, value.as_ref()),
                None => None
            };

            let final_block = builder.create_block();
            builder.add_instruction(
                VirtualInstruction::Jump { target: final_block.clone() },
                CXType::unit()
            );

            builder.set_current_block(final_block);
            builder.add_instruction(
                VirtualInstruction::Return { value },
                CXType::unit()
            )
        },

        CXExprKind::StringLiteral { val, .. } => {
            let string_id = builder.create_global_string(val.clone());

            builder.add_instruction(
                VirtualInstruction::StringLiteral {
                    str_id: string_id,
                },

                CXTypeKind::PointerTo(
                    Box::new(
                        CXType::new(
                            CX_CONST,
                            "char".into()
                        )
                    )
                ).to_val_type()
            )
        },

        CXExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::Negative => {
                    let operand = generate_instruction(builder, operand.as_ref())?;
                    let op_type = builder.get_type(operand)?.clone();
                    
                    builder.add_instruction_bt(
                        VirtualInstruction::IntegerUnOp {
                            value: operand,
                            op: BCIntUnOp::NEG
                        },
                        op_type
                    )
                },
                CXUnOp::LNot => {
                    let operand = generate_instruction(builder, operand.as_ref())?;
                    let op_type = builder.get_type(operand)?.clone();

                    builder.add_instruction_bt(
                        VirtualInstruction::IntegerUnOp {
                            value: operand,
                            op: BCIntUnOp::LNOT
                        },
                        op_type
                    )
                },
                
                CXUnOp::Dereference |
                CXUnOp::AddressOf => generate_instruction(builder, operand.as_ref()),
                
                CXUnOp::PreIncrement(off) => {
                    let value = generate_instruction(builder, operand.as_ref())?;
                    let val_type = builder.get_expr_intrinsic_type(operand)?
                        .clone();

                    let CXTypeKind::MemoryAlias(inner) = val_type
                        else { unreachable!("generate_instruction: Expected memory alias type for expr, found {val_type}") };

                    let loaded_val = builder.add_instruction(
                        VirtualInstruction::Load {
                            value: value.clone()
                        },
                        inner.as_ref().clone()
                    )?;

                    let bytes = match inner.as_ref().intrinsic_type(&builder.cx_type_map)? {
                        CXTypeKind::Integer { bytes, .. } => *bytes,
                        CXTypeKind::PointerTo(_) => 8,
                        _ => panic!("Invalid type for post increment: {inner:?}")
                    };

                    let one = builder.add_instruction_bt(
                        VirtualInstruction::Immediate {
                            value: *off as i32
                        },
                        BCTypeKind::Signed { bytes }.into()
                    )?;

                    let incremented = generate_binop(
                        builder,
                        inner.as_ref(),
                        loaded_val,
                        one,
                        &CXBinOp::Add
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: loaded_val,
                            value: incremented.clone(),
                            type_: builder.convert_cx_type(inner.as_ref())?
                        },
                        CXType::unit()
                    )?;

                    Some(incremented)
                },
                CXUnOp::PostIncrement(off) => {
                    let value = generate_instruction(builder, operand.as_ref())?;
                    let val_type = builder.get_expr_intrinsic_type(operand)?
                        .clone();

                    let CXTypeKind::MemoryAlias(inner) = val_type
                        else { unreachable!("generate_instruction: Expected memory alias type for expr, found {val_type}") };

                    let loaded_val = builder.add_instruction(
                        VirtualInstruction::Load {
                            value: value.clone()
                        },
                        inner.as_ref().clone()
                    )?;

                    let bytes = match inner.as_ref().intrinsic_type(&builder.cx_type_map)? {
                        CXTypeKind::Integer { bytes, .. } => *bytes,
                        CXTypeKind::PointerTo(_) => 8,
                        _ => panic!("Invalid type for post increment: {inner:?}")
                    };

                    let one = builder.add_instruction_bt(
                        VirtualInstruction::Immediate {
                            value: *off as i32
                        },
                        BCTypeKind::Signed { bytes }.into()
                    )?;

                    let incremented = generate_binop(
                        builder,
                        inner.as_ref(),
                        loaded_val,
                        one,
                        &CXBinOp::Add
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: value,
                            value: incremented,
                            type_: builder.convert_cx_type(inner.as_ref())?
                        },
                        CXType::unit()
                    )?;

                    Some(loaded_val)
                },

                CXUnOp::ExplicitCast(_) =>
                    generate_instruction(builder, operand.as_ref()),

                _ => todo!("generate_instruction for {:?}", operator)
            }
        },

        CXExprKind::If { condition, then_branch, else_branch } => {
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
                CXType::unit()
            );

            builder.set_current_block(then_block);
            generate_instruction(builder, then_branch.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block.clone() },
                CXType::unit()
            );

            builder.set_current_block(else_block);
            if let Some(else_branch) = else_branch {
                generate_instruction(builder, else_branch.as_ref());
            }
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block.clone() },
                CXType::unit()
            );

            builder.set_current_block(merge_block);
            Some(ValueID::NULL)
        },

        CXExprKind::While { condition, body, pre_eval } => {
            let condition_block = builder.start_cont_point();
            let body_block = builder.create_block();
            let merge_block = builder.start_scope();

            let first_block = if *pre_eval {
                condition_block.clone()
            } else {
                body_block.clone()
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: first_block },
                CXType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block.clone(),
                    false_block: merge_block.clone()
                },
                CXType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXType::unit()
            );

            builder.end_scope();
            builder.end_cond();
            Some(ValueID::NULL)
        },

        CXExprKind::Switch { condition, block, cases, default_case } => {
            let condition_value = generate_instruction(builder, condition);
            
            let default_block = if default_case.is_some() {
                Some(builder.create_named_block("switch default"))
            } else {
                None
            };
            let merge_block = builder.start_scope();
            let case_blocks = cases
                .iter()
                .map(|_| builder.create_block())
                .collect::<Vec<_>>();

            let mut sorted_cases = cases.clone();
            sorted_cases.sort_by(|a, b| a.1.cmp(&b.1));
            
            builder.add_instruction(
                VirtualInstruction::JumpTable {
                    value: condition_value.unwrap_or(ValueID::NULL),
                    targets: sorted_cases.iter()
                        .enumerate()
                        .map(|(i, (case, _))| (*case, case_blocks[i]))
                        .collect(),
                    default: default_block.clone().unwrap_or(merge_block.clone())
                },
                CXType::unit()
            );
            
            let mut case_iter = sorted_cases.iter()
                .map(|(_, i)| *i);
            let mut case_block_iter = case_blocks.iter();
            let mut next_index = case_iter.next();
            
            for (i, expr) in block.iter().enumerate() {
                while next_index == Some(i) {
                    let case_block = case_block_iter.next().unwrap();
                    builder.add_instruction(
                        VirtualInstruction::Jump { target: case_block.clone() },
                        CXType::unit()
                    );
                    next_index = case_iter.next();
                    builder.set_current_block(case_block.clone());
                }
                
                if *default_case == Some(i) {
                    let block = default_block.unwrap();
                    builder.add_instruction(
                        VirtualInstruction::Jump { target: block },
                        CXType::unit()
                    );
                    builder.set_current_block(block);
                }
                
                generate_instruction(builder, expr)?;
            }
            
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block.clone() },
                CXType::unit()
            );
            
            builder.end_scope();
            Some(ValueID::NULL)
        },

        CXExprKind::For { init, condition, increment, body } => {
            let condition_block = builder.create_block();
            let body_block = builder.create_block();
            let increment_block = builder.start_cont_point();
            let merge_block = builder.start_scope();

            generate_instruction(builder, init.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block.clone(),
                    false_block: merge_block.clone()
                },
                CXType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: increment_block.clone() },
                CXType::unit()
            );

            builder.set_current_block(increment_block);
            generate_instruction(builder, increment.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block.clone() },
                CXType::unit()
            );

            builder.end_scope();
            builder.end_cond();
            Some(ValueID::NULL)
        },

        CXExprKind::Break => {
            let Some(merge) = builder.get_merge() else {
                log_error!("TYPE ERROR: Invalid break in outermost scope");
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: merge },
                CXType::unit()
            );

            Some(ValueID::NULL)
        },

        CXExprKind::Continue => {
            let Some(cond) = builder.get_continue() else {
                log_error!("TYPE ERROR: Invalid continue in outermost scope");
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: cond },
                CXType::unit()
            );

            Some(ValueID::NULL)
        },

        _ => todo!("generate_instruction for {:?}", expr)
    }
}

pub(crate) fn generate_binop(
    builder: &mut BytecodeBuilder,
    cx_lhs_type: &CXType,
    left_id: ValueID,
    right_id: ValueID,
    op: &CXBinOp
) -> Option<ValueID> {
    let lhs_type = builder.get_type(left_id)?.clone();
    
    match lhs_type.kind {
        BCTypeKind::Signed { .. } => {
            builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_i_binop(op)?
                },
                lhs_type
            )
        },

        BCTypeKind::Unsigned { .. } => {
            builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_u_binop(op)?
                },
                lhs_type
            )
        },

        BCTypeKind::Pointer { .. } => {
            let CXTypeKind::PointerTo(left_inner) = &cx_lhs_type.intrinsic_type(&builder.cx_type_map)?
                else { unreachable!("generate_binop: Expected pointer type for {left_id}, found {cx_lhs_type}") };

            builder.add_instruction_bt(
                VirtualInstruction::PointerBinOp {
                    left: left_id,
                    right: right_id,
                    ptr_type: builder.convert_cx_type(&left_inner)?,
                    op: builder.cx_ptr_binop(op)?
                },
                lhs_type
            )
        },

        BCTypeKind::Float { .. } => {
            builder.add_instruction_bt(
                VirtualInstruction::FloatBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_float_binop(op)?
                },
                lhs_type
            )
        },

        _type =>
            panic!("Invalid arguments with type for binop not caught by type checker")
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
        CXType::unit()
    );
    builder.set_current_block(return_block);

    if prototype.name.data == "main" {
        let zero = builder.add_instruction(
            VirtualInstruction::Immediate {
                value: 0
            },
            CXTypeKind::Integer { bytes: 4, signed: true }.to_val_type()
        )?;

        builder.add_instruction(
            VirtualInstruction::Return {
                value: Some(zero)
            },
            CXType::unit()
        );
    } else if prototype.return_type.is_void(&builder.cx_type_map) {
        builder.add_instruction(
            VirtualInstruction::Return {
                value: None
            },
            CXType::unit()
        )?;
    } else {
        let last_instruction = builder.last_instruction();
        log_error!("Function {} has a return type but no return statement\nLast Statement found: {:?}", prototype.name, last_instruction);
    }

    Some(())
}