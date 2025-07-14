use cx_compiler_ast::parse::operators::comma_separated;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXFunctionPrototype, CXUnOp};
use cx_data_ast::parse::value_type::{CXTypeKind, CXType, CX_CONST};
use cx_data_bytecode::types::{BCType, BCTypeKind, BCTypeSize};
use cx_data_bytecode::{BCFunctionPrototype, BCIntUnOp, BCPtrBinOp, BlockID, ElementID, ValueID, VirtualInstruction};
use cx_util::log_error;
use crate::aux_routines::{allocate_variable, get_cx_struct_field_by_index, get_struct_field};
use crate::builder::BytecodeBuilder;
use crate::cx_maps::convert_fixed_type_kind;
use crate::deconstructor::deconstruct_variable;
use crate::implicit_cast::implicit_cast;

pub fn generate_instruction(
    builder: &mut BytecodeBuilder,
    expr: &CXExpr
) -> Option<ValueID> {
    match &expr.kind {
        CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Assign(_) } => {
            let left_id = generate_instruction(builder, lhs)?;
            let right_id = generate_instruction(builder, rhs)?;
            let lhs_type = builder.get_expr_type(lhs)?
                .clone();

            let CXTypeKind::MemoryAlias(inner) = lhs_type.intrinsic_type_kind(&builder.cx_type_map)?.clone()
                else { unreachable!("generate_instruction: Expected memory alias type for expr, found {lhs_type}") };

            if !matches!(lhs.as_ref().kind, CXExprKind::VarDeclaration { .. }) {
                deconstruct_variable(
                    builder, left_id, inner.as_ref(), true
                )?;
            }
            
            let inner_type = builder.convert_fixed_cx_type(inner.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Store {
                    memory: left_id,
                    value: right_id,
                    type_: inner_type,
                },
                CXType::unit()
            )
        },
        CXExprKind::VarDeclaration { name, type_ } =>
            allocate_variable(name.as_str(), builder, type_),

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
                CXTypeKind::PointerTo { inner, .. } => inner,
                
                CXTypeKind::VariableLengthArray { _type, .. } |
                CXTypeKind::Array { _type, .. } => _type,
                
                _ => panic!("Invalid array index type: {l_type}"),
            };
            
            let inner_as_bc = convert_fixed_type_kind(&builder.cx_type_map, &lhs_inner.kind)?;

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
            
            let (prototype, direct_call) = match builder.get_expr_intrinsic_type(lhs.as_ref())? {
                CXTypeKind::Function { prototype } => (*prototype, true),
                CXTypeKind::PointerTo { inner, .. } => {
                    let Some(CXTypeKind::Function { prototype }) =
                        inner.intrinsic_type_kind(&builder.cx_type_map) else {
                        log_error!("Invalid function pointer type: {inner}");
                    };
                    (*prototype.clone(), false)
                },
                type_ => log_error!("Invalid function pointer type: {type_}"),
            };
            
            let mut args = vec![];
            
            if prototype.return_type.is_structured(&builder.cx_type_map) {
                let buffer_type = builder.convert_cx_type(&prototype.return_type)?;
                
                let buffer = builder.add_instruction_bt(
                    VirtualInstruction::Allocate {
                        size: buffer_type.fixed_size(),
                        alignment: buffer_type.alignment(),
                    },
                    BCType::from(BCTypeKind::Pointer { nullable: false, dereferenceable: buffer_type.fixed_size() as u32 })
                )?;
                
                args.push(buffer);
            }

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
                CXTypeKind::PointerTo { inner, .. } => {
                    let Some(CXTypeKind::Function { prototype }) =
                        inner.intrinsic_type_kind(&builder.cx_type_map) else {
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
            let return_type = builder.get_expr_type(expr)?;
            let as_bc = builder.convert_cx_type(&return_type)?;
            
            generate_binop(builder, lhs.as_ref(), rhs.as_ref(), as_bc, op)
        }
        
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

            implicit_cast(builder, inner, from_type, to_type, cast_type)
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
                    func: func_name
                },
                func_sig.clone()
            )
        },

        CXExprKind::IntLiteral { val, bytes } => {
            builder.int_const(*val as i32, *bytes, true)
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
                Some(*id)
            } else if let Some(val) = builder.fn_ref(val.as_str())? {
                Some(val)
            } else {
                log_error!("Unknown identifier {val}")
            }
        },

        CXExprKind::Return { value } => {
            if value.is_none() {
                builder.add_return(None);
                return Some(ValueID::NULL);
            }
            
            let value = value.as_ref().unwrap().as_ref();
            let value_id = generate_instruction(builder, value)?;
            let returned_type = builder.get_expr_type(value)?
                .clone();
            
            if returned_type.is_structured(&builder.cx_type_map) {
                let first_param = builder.add_instruction_bt(
                    VirtualInstruction::FunctionParameter {
                        param_index: 0,
                    },
                    BCType::from(BCTypeKind::Pointer { nullable: false, dereferenceable: 0 })
                )?;
                
                builder.add_instruction(
                    VirtualInstruction::Store {
                        memory: first_param,
                        value: value_id,
                        type_: builder.convert_fixed_cx_type(&returned_type)?
                    },
                    CXType::unit()
                )?;
                
                builder.add_return(Some(first_param));
            } else {
                builder.add_return(Some(value_id));
            }
            
            Some(ValueID::NULL)
        },
        
        CXExprKind::Defer { expr } => {
            let previous_block = builder.current_block();
            
            builder.enter_deferred_logic();
            generate_instruction(builder, expr.as_ref())?;
            builder.exit_deferred_logic();
            
            builder.set_current_block(previous_block);
            
            Some(ValueID::NULL)
        }

        CXExprKind::StringLiteral { val, .. } => {
            let string_id = builder.create_global_string(val.clone());

            builder.add_instruction_bt(
                VirtualInstruction::StringLiteral {
                    str_id: string_id,
                },
                BCType::default_pointer()
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
                            value
                        },
                        inner.as_ref().clone()
                    )?;

                    let bytes = match inner.as_ref().intrinsic_type_kind(&builder.cx_type_map)? {
                        CXTypeKind::Integer { bytes, .. } => *bytes,
                        CXTypeKind::PointerTo { .. } => 8,
                        _ => panic!("Invalid type for post increment: {inner:?}")
                    };

                    let offset = builder.int_const(*off as i32, bytes, true)?;

                    let incremented = generate_algebraic_binop(
                        builder, inner.as_ref(), loaded_val,
                        offset, builder.convert_fixed_cx_type(inner.as_ref())?,
                        &CXBinOp::Add
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: loaded_val,
                            value: incremented,
                            type_: builder.convert_fixed_cx_type(inner.as_ref())?
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
                            value
                        },
                        inner.as_ref().clone()
                    )?;

                    let bytes = match inner.as_ref().intrinsic_type_kind(&builder.cx_type_map)? {
                        CXTypeKind::Integer { bytes, .. } => *bytes,
                        CXTypeKind::PointerTo { .. } => 8,
                        _ => panic!("Invalid type for post increment: {inner:?}")
                    };

                    let one = builder.int_const(*off as i32, bytes, true)?;

                    let incremented = generate_algebraic_binop(
                        builder, inner.as_ref(),
                        loaded_val, one, builder.convert_fixed_cx_type(inner.as_ref())?,
                        &CXBinOp::Add
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: value,
                            value: incremented,
                            type_: builder.convert_fixed_cx_type(inner.as_ref())?
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
                    true_block: then_block,
                    false_block: else_block
                },
                CXType::unit()
            );

            builder.set_current_block(then_block);
            generate_instruction(builder, then_branch.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block },
                CXType::unit()
            );

            builder.set_current_block(else_block);
            if let Some(else_branch) = else_branch {
                generate_instruction(builder, else_branch.as_ref());
            }
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block },
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
                condition_block
            } else {
                body_block
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
                    true_block: body_block,
                    false_block: merge_block
                },
                CXType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref());
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block },
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
                    default: default_block.unwrap_or(merge_block)
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
                        VirtualInstruction::Jump { target: *case_block },
                        CXType::unit()
                    );
                    next_index = case_iter.next();
                    builder.set_current_block(*case_block);
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
                VirtualInstruction::Jump { target: merge_block },
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
                VirtualInstruction::Jump { target: condition_block },
                CXType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block,
                    false_block: merge_block
                },
                CXType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: increment_block },
                CXType::unit()
            );

            builder.set_current_block(increment_block);
            generate_instruction(builder, increment.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block },
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
        
        CXExprKind::SizeOf { expr } => {
            let type_ = builder.get_expr_type(expr.as_ref())?;
            let type_size = builder.convert_cx_type(&type_)?
                .size();
            
            match type_size {
                BCTypeSize::Fixed(size) 
                    => builder.int_const(size as i32, 8, true),
                BCTypeSize::Variable(size_expr) 
                    => Some(size_expr)
            }
        },

        CXExprKind::Move { expr } => {
            let memory = generate_instruction(builder, expr.as_ref())?;
            
            let value = builder.add_instruction_bt(
                VirtualInstruction::Load {
                    value: memory
                },
                BCType::default_pointer()
            )?;
            
            let zero = builder.int_const(0, 8, true)?;
            let zero_as_ptr = builder.add_instruction(
                VirtualInstruction::IntToPtr {
                    value: zero
                },
                CXTypeKind::Integer { bytes: 8, signed: true }.to_val_type()
            )?;

            builder.add_instruction(
                VirtualInstruction::Store {
                    memory,
                    value: zero_as_ptr,
                    type_: BCType::default_pointer()
                },
                CXType::unit()
            )?;

            Some(value)
        },
        
        CXExprKind::New { _type, array_length } => {
            const STANDARD_ALLOC : &str = "__stdalloc";
            const STANDARD_ARRAY_ALLOC : &str = "__stdallocarray";
            
            let type_as_bc = builder.convert_cx_type(_type)?;
            let type_size = type_as_bc.fixed_size();
            
            let size_imm = builder.int_const(type_size as i32, 8, true)?;
            
            match array_length {
                Some(len) => {
                    let func = builder.fn_ref(STANDARD_ARRAY_ALLOC)?
                        .expect("INTERNAL PANIC: Standard array alloc function not found");
                    let len = generate_instruction(builder, len.as_ref())?;
                    
                    builder.add_instruction_bt(
                        VirtualInstruction::DirectCall {
                            func,
                            args: vec![size_imm, len],
                            method_sig: builder.fn_map.get(STANDARD_ARRAY_ALLOC).unwrap().clone(),
                        },
                        BCType::default_pointer()
                    )
                },
                
                None => {
                    let func = builder.fn_ref(STANDARD_ALLOC)?
                        .expect("INTERNAL PANIC: Standard alloc function not found");
                    
                    builder.add_instruction_bt(
                        VirtualInstruction::DirectCall {
                            func,
                            args: vec![size_imm],
                            method_sig: builder.fn_map.get(STANDARD_ALLOC).unwrap().clone(),
                        },
                        BCType::default_pointer()
                    )
                },
            }
        },
        
        CXExprKind::InitializerList { indices } => {
            let generated_type = builder.get_expr_type(expr)?;
            let as_bc = builder.convert_cx_type(&generated_type)?;
            
            let alloc = builder.add_instruction_bt(
                VirtualInstruction::Allocate {
                    size: as_bc.fixed_size(),
                    alignment: as_bc.alignment(),
                },
                BCType::default_pointer()
            )?;
            
            builder.add_instruction(
                VirtualInstruction::ZeroMemory {
                    memory: alloc,
                    _type: as_bc.clone(),
                },
                CXType::unit()
            );
            
            for index in indices.iter() {
                let value = generate_instruction(builder, &index.value)?;
             
                let access = get_cx_struct_field_by_index(
                    builder,
                    &as_bc,
                    index.index
                ).unwrap_or_else(|| {
                    panic!("PANIC: Attempting to access non-existent field at index {} in struct {as_bc:?}", index.index);
                });
             
                let struct_access = builder.add_instruction_bt(
                    VirtualInstruction::StructAccess {
                        struct_: alloc,
                        struct_type: as_bc.clone(),
                        field_offset: access.offset,
                        field_index: access.index,
                    },
                    BCType::default_pointer()
                )?;
                
                builder.add_instruction_bt(
                    VirtualInstruction::Store {
                        memory: struct_access,
                        value,
                        type_: access._type.clone()
                    },
                    BCType::unit()
                )?;
            }
            
            Some(alloc)
        },
        
        CXExprKind::Taken => panic!("PANIC: Attempting to generate instruction for `Taken` expression, which should have been removed by the type checker."),
        CXExprKind::Unit => panic!("PANIC: Attempting to generate instruction for `Unit` expression, which should have been removed by the type checker."),
    }
}

pub(crate) fn generate_binop(
    builder: &mut BytecodeBuilder, 
    lhs: &CXExpr, rhs: &CXExpr, 
    return_type: BCType,
    op: &CXBinOp
) -> Option<ValueID> {
    match op {
        CXBinOp::LAnd | CXBinOp::LOr => {
            // Short circuit evaluation for logical operators
            let previous_block = builder.current_block();
            let match_type = builder.get_expr_bc_type(lhs).unwrap();
            let false_imm = builder.int_const_match(0, &match_type)?;
            
            let left_id = generate_instruction(builder, lhs)?;
            let left_cmp = builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: false_imm,
                    op: builder.cx_i_binop(&CXBinOp::NotEqual).unwrap()
                },
                BCType::from(BCTypeKind::Bool)
            )?;
            
            let no_short_circuit_block = builder.create_block();
            let merge_block = builder.create_block();
            
            let (true_block, false_block) = match op {
                CXBinOp::LAnd => (no_short_circuit_block, merge_block),
                CXBinOp::LOr => (merge_block, no_short_circuit_block),
                _ => unreachable!("generate_binop: Expected logical operator, found {op}"),
            };
            
            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: left_cmp,
                    true_block,
                    false_block
                },
                CXType::unit()
            );
            
            builder.set_current_block(no_short_circuit_block);
            
            let right_id = generate_instruction(builder, rhs)?;
            let right_cmp = builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: right_id,
                    right: false_imm,
                    op: builder.cx_i_binop(&CXBinOp::NotEqual).unwrap()
                },
                BCType::from(BCTypeKind::Bool)
            )?;
            
            builder.add_instruction(
                VirtualInstruction::Jump {
                    target: merge_block
                },
                CXType::unit()
            );
            
            builder.set_current_block(merge_block);
            
            builder.add_instruction_bt(
                VirtualInstruction::Phi {
                    predecessors: vec![
                        (left_cmp, previous_block),
                        (right_cmp, no_short_circuit_block),
                    ]
                },
                return_type
            )
        },
        
        _ => {
            let left_id = generate_instruction(builder, lhs)?;
            let right_id = generate_instruction(builder, rhs)?;
            let cx_lhs_type = builder.get_expr_intrinsic_type(lhs)?.to_val_type();
            
            generate_algebraic_binop(
                builder,
                &cx_lhs_type,
                left_id, right_id,
                return_type,
                op
            )
        },
    }
}

pub(crate) fn generate_algebraic_binop(
    builder: &mut BytecodeBuilder,
    cx_lhs_type: &CXType,
    left_id: ValueID,
    right_id: ValueID,
    return_type: BCType,
    op: &CXBinOp
) -> Option<ValueID> {
    match return_type.kind {
        BCTypeKind::Signed { .. } => {
            builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_i_binop(op)?
                },
                return_type
            )
        },

        BCTypeKind::Unsigned { .. } => {
            builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_u_binop(op)?
                },
                return_type
            )
        },
        
        BCTypeKind::Bool => {
            builder.add_instruction_bt(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_i_binop(op)?
                },
                return_type
            )
        },

        BCTypeKind::Pointer { .. } => {
            let CXTypeKind::PointerTo { inner: left_inner, .. }
                = &cx_lhs_type.intrinsic_type_kind(&builder.cx_type_map)?
            else { 
                builder.dump_current_fn();
                unreachable!("generate_binop: Expected pointer type for {left_id}, found {cx_lhs_type}") 
            };

            builder.add_instruction_bt(
                VirtualInstruction::PointerBinOp {
                    left: left_id,
                    right: right_id,
                    ptr_type: builder.convert_fixed_cx_type(left_inner)?,
                    op: builder.cx_ptr_binop(op)?
                },
                return_type
            )
        },

        BCTypeKind::Float { .. } => {
            builder.add_instruction_bt(
                VirtualInstruction::FloatBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_float_binop(op)?
                },
                return_type
            )
        },

        _type =>
            panic!("Invalid arguments with type for binop not caught by type checker")
    }
}

pub(crate) fn implicit_return(
    builder: &mut BytecodeBuilder,
    prototype: &BCFunctionPrototype,
) -> Option<()> {
    let last_instruction = builder.last_instruction();

    if let Some(last_instruction) = last_instruction {
        match last_instruction.instruction {
            VirtualInstruction::Return { .. } => {
                // If the last instruction is already a return, do nothing
                return Some(());
            },
            VirtualInstruction::GotoDefer { .. } => {
                // If the last instruction is a goto defer, we can also ignore it
                return Some(());
            },
            
            _ => {}
        }
    }

    let return_block = builder.create_block();
    builder.add_instruction(
        VirtualInstruction::Jump {
            target: return_block
        },
        CXType::unit()
    );
    builder.set_current_block(return_block);

    if prototype.name == "main" {
        let zero = builder.add_instruction(
            VirtualInstruction::Immediate {
                value: 0
            },
            CXTypeKind::Integer { bytes: 4, signed: true }.to_val_type()
        )?;

        builder.add_return(Some(zero));
    } else if prototype.return_type.is_void() {
        builder.add_return(None);
    } else {
        let last_instruction = builder.last_instruction();
        builder.dump_current_fn();
        log_error!("Function {} has a return type but no return statement\nLast Statement found: {:?}", prototype.name, last_instruction);
    }

    Some(())
}

pub(crate) fn implicit_defer_return(
    builder: &mut BytecodeBuilder,
    prototype: &BCFunctionPrototype,
) -> Option<()> {
    if builder.function_defers() {
        let return_value = if prototype.return_type.is_void() {
            None
        } else {
            // Phi node for the return value
            Some(
                ValueID {
                    block_id: BlockID {
                        id: 0,
                        in_deferral: true
                    },
                    value_id: 0
                }
            )
        };

        builder.enter_deferred_logic();
        builder.add_instruction(
            VirtualInstruction::Return {
                value: return_value,
            },
            CXType::unit()
        )?;
    }
    
    Some(())
}