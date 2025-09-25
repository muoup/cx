use std::any::type_name_of_val;
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXUnOp};
use cx_data_typechecker::cx_types::{CXTypeKind, CXType};
use cx_data_mir::types::{MIRType, MIRTypeKind, BCTypeSize};
use cx_data_mir::{MIRFunctionPrototype, MIRGlobalType, MIRGlobalValue, BCIntUnOp, MIRParameter, BCPtrBinOp, BlockID, LinkageType, MIRValue, VirtualInstruction};
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_util::{bytecode_error_log, log_error};
use cx_util::mangling::mangle_deconstructor;
use crate::aux_routines::{allocate_variable, get_cx_struct_field_by_index, get_struct_field, try_access_field};
use crate::builder::MIRBuilder;
use crate::cx_maps::{convert_cx_prototype, convert_fixed_type_kind};
use crate::deconstructor::deconstruct_variable;
use crate::implicit_cast::implicit_cast;

pub fn generate_instruction(
    builder: &mut MIRBuilder,
    expr: &TCExpr
) -> Option<MIRValue> {
    match &expr.kind {
        TCExprKind::VariableDeclaration { name, type_ } =>
            allocate_variable(name.as_str(), builder, type_),

        TCExprKind::Assignment { target, value, additional_op } => {
            let left_id = generate_instruction(builder, target)?;
            let right_id = generate_instruction(builder, value)?;
            
            if additional_op.is_some() { todo!("compound assignment") }

            if !matches!(target.kind, TCExprKind::VariableDeclaration { .. }) {
                deconstruct_variable(builder, &left_id, &target._type)?;
            }

            let Some(inner) = target._type.mem_ref_inner()
                else { unreachable!("generate_instruction: Expected memory alias type for expr, found {}", target._type) };

            let inner_type = builder.convert_fixed_cx_type(inner)?;

            builder.add_instruction(
                VirtualInstruction::Store {
                    memory: left_id,
                    value: right_id,
                    type_: inner_type,
                },
                MIRType::unit()
            )
        },

        TCExprKind::Access { target, field } => {
            let left_id = generate_instruction(builder, target.as_ref())?;
            let ltype = builder.convert_cx_type(&target._type)?;

            if let Some(id) = try_access_field(builder, &ltype, left_id, field.as_str()) {
                return Some(id);
            }

            bytecode_error_log!(builder, "Invalid access operation on {} with {field}", target._type);
        },

        TCExprKind::BinOp { lhs, rhs, op: CXBinOp::ArrayIndex } => {
            let left_id = generate_instruction(builder, lhs.as_ref())?;
            let right_id = generate_instruction(builder, rhs.as_ref())?;

            let lhs_inner = match &lhs._type.kind {
                CXTypeKind::PointerTo { inner_type: inner, .. } => inner,
                CXTypeKind::VariableLengthArray { _type, .. } |
                CXTypeKind::Array { inner_type: _type, .. } => _type,
                
                _ => panic!("Invalid array index type: {}", lhs._type),
            };
            
            let inner_as_bc = convert_fixed_type_kind(&lhs_inner.kind)?;

            builder.add_instruction(
                VirtualInstruction::PointerBinOp {
                    left: left_id,
                    right: right_id,
                    ptr_type: MIRType::from(inner_as_bc),
                    op: BCPtrBinOp::ADD
                },
                MIRType::default_pointer()
            )
        },

        TCExprKind::FunctionCall { function, arguments, direct_call } => {
            let prototype = match &function._type.kind {
                CXTypeKind::Function { prototype }
                    => *prototype.clone(),
                CXTypeKind::PointerTo { inner_type: inner, .. } => {
                    let CXTypeKind::Function { prototype } = &inner.kind else {
                        log_error!("Invalid function pointer type: {inner}");
                    };

                    *prototype.clone()
                },
                type_ => log_error!("Invalid function pointer type: {type_}"),
            };
            
            let mut args = vec![];

            if prototype.return_type.is_structured() {
                let buffer_type = builder.convert_cx_type(&prototype.return_type)?;

                let buffer = builder.add_instruction(
                    VirtualInstruction::Allocate {
                        _type: buffer_type.clone(),
                        alignment: buffer_type.alignment(),
                    },
                    MIRType::from(MIRTypeKind::Pointer { nullable: false, dereferenceable: buffer_type.fixed_size() as u32 })
                )?;

                args.push(buffer);
            }

            // Functions that require special intrinsic parameters
            match &function.kind {
                TCExprKind::MemberFunctionReference { target, mangled_name: name } => {
                    args.push(generate_instruction(builder, target.as_ref())?);
                },

                _ => {}
            }

            for arg in arguments.iter() {
                let arg_id = generate_instruction(builder, arg)?;
                args.push(arg_id);
            }

            match direct_call {
                true => {
                    builder.add_instruction_cxty(
                        VirtualInstruction::DirectCall {
                            args,
                            method_sig: convert_cx_prototype(&prototype)?
                        },
                        prototype.return_type.clone()
                    )
                },
                false => {
                    let left_id = generate_instruction(builder, function.as_ref())?;

                    builder.add_instruction_cxty(
                        VirtualInstruction::IndirectCall {
                            func_ptr: left_id,
                            args,
                            method_sig: convert_cx_prototype(&prototype)?
                        },
                        prototype.return_type.clone()
                    )
                },

                type_ => log_error!("Invalid function pointer type: {type_}")
            }
        },

        TCExprKind::BinOp { lhs, rhs, op } => {
            let return_type = builder.convert_cx_type(&expr._type)?;
            
            generate_binop(builder, lhs.as_ref(), rhs.as_ref(), return_type, op)
        }
        
        TCExprKind::Block { statements } => {
            for expr in statements.iter() {
                generate_instruction(builder, expr)?;
            }

            Some(MIRValue::NULL)
        },

        TCExprKind::Coercion { operand, cast_type } => {
            let inner = generate_instruction(builder, operand.as_ref())?;

            implicit_cast(builder, inner, &operand._type, &expr._type, cast_type)
        },

        TCExprKind::ImplicitLoad { operand } => {
            let inner = generate_instruction(builder, operand.as_ref())?;

            // A memory reference to a struct operationally is the same as a struct itself,
            // therefore loading it will actually cause issues.
            match &expr._type.kind {
                CXTypeKind::Structured { .. } |
                CXTypeKind::Union { .. } => Some(inner),

                _ => {
                    let bc_type = builder.convert_cx_type(&expr._type)?;

                    Some(MIRValue::LoadOf(bc_type, Box::new(inner)))
                }
            }
        },

        TCExprKind::IntLiteral { value } => {
            let bc_type = builder.convert_cx_type(&expr._type)?;
            Some(builder.match_int_const(*value as i32, &bc_type))
        },
        
        TCExprKind::FloatLiteral { value } => {
            let bc_type = builder.convert_cx_type(&expr._type)?;

            Some(builder.match_float_const(*value, &bc_type))
        },

        TCExprKind::VariableReference { name } => {
            let Some(val) = builder.get_symbol(name.as_str()) else {
                log_error!("Variable {name} not found in current scope");
            };

            Some(val)
        },

        TCExprKind::GlobalVariableReference { name } => {
            if !builder.global_symbol_exists(name.as_str()) {
                let type_ = builder.convert_cx_type(&expr._type)?;

                builder.insert_global_symbol(
                    MIRGlobalValue {
                        name: name.clone(),
                        linkage: LinkageType::External,
                        _type: MIRGlobalType::Variable {
                            _type: type_,
                            initial_value: None
                        }
                    }
                );
            }

            builder.get_symbol(name.as_str())
        },

        TCExprKind::MemberFunctionReference { mangled_name: name, .. } |
        TCExprKind::FunctionReference { name } => {
            unreachable!("INTERNAL ERROR: Function references should not be used in instruction generation directly!");
        },

        TCExprKind::TemporaryBuffer { _type } => {
            let type_as_bc = builder.convert_cx_type(_type)?;

            builder.add_instruction(
                VirtualInstruction::Allocate {
                    alignment: type_as_bc.alignment(),
                    _type: type_as_bc.clone(),
                },
                MIRType::from(MIRTypeKind::Pointer { nullable: false, dereferenceable: type_as_bc.fixed_size() as u32 })
            )
        },

        TCExprKind::Return { value } => {
            if value.is_none() {
                builder.add_return(None);
                return Some(MIRValue::NULL);
            }
            
            let value = value.as_ref().unwrap().as_ref();
            let value_id = generate_instruction(builder, value)?;
            let returned_type = builder.convert_cx_type(&value._type)?;
            
            if returned_type.is_structure() {
                builder.add_instruction(
                    VirtualInstruction::Store {
                        memory: MIRValue::ParameterRef(0),
                        value: value_id,
                        type_: returned_type
                    },
                    MIRType::unit()
                )?;
                
                builder.add_return(Some(MIRValue::ParameterRef(0)));
            } else {
                builder.add_return(Some(value_id));
            }
            
            Some(MIRValue::NULL)
        },
        
        TCExprKind::Defer { operand } => {
            let previous_block = builder.current_block();
            
            builder.enter_deferred_logic();
            generate_instruction(builder, operand.as_ref())?;
            builder.exit_deferred_logic();
            
            builder.set_current_block(previous_block);
            
            Some(MIRValue::NULL)
        }

        TCExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::Negative => {
                    let operand_type = builder.convert_cx_type(&operand._type)?;
                    let operand = generate_instruction(builder, operand.as_ref())?;

                    builder.add_instruction(
                        VirtualInstruction::IntegerUnOp {
                            value: operand,
                            op: BCIntUnOp::NEG
                        },
                        operand_type
                    )
                },
                CXUnOp::LNot => {
                    let operand = generate_instruction(builder, operand.as_ref())?;

                    builder.add_instruction(
                        VirtualInstruction::IntegerUnOp {
                            value: operand,
                            op: BCIntUnOp::LNOT
                        },
                        MIRType::from(MIRTypeKind::Bool)
                    )
                },
                
                // No-op - only changes the type information for the typechecker
                CXUnOp::AddressOf |
                
                // No-op - handled by a typechecker-generated implicit load
                CXUnOp::Dereference => generate_instruction(builder, operand.as_ref()),
                
                CXUnOp::PreIncrement(off) => {
                    let value = generate_instruction(builder, operand.as_ref())?;
                    let val_type = &operand._type.kind;

                    let CXTypeKind::MemoryReference(inner) = val_type
                        else { unreachable!("generate_instruction: Expected memory alias type for expr, found {val_type}") };

                    let bc_type = builder.convert_fixed_cx_type(inner.as_ref())?;

                    let loaded_val = MIRValue::LoadOf(bc_type, value.into());
                    let offset = builder.int_const(*off as i32, 8, true);

                    let incremented = generate_algebraic_binop(
                        builder, inner.as_ref(), loaded_val.clone(),
                        offset, builder.convert_fixed_cx_type(inner.as_ref())?,
                        &CXBinOp::Add
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: loaded_val,
                            value: incremented.clone(),
                            type_: builder.convert_fixed_cx_type(inner.as_ref())?
                        },
                        MIRType::unit()
                    )?;

                    Some(incremented)
                },
                CXUnOp::PostIncrement(off) => {
                    let value = generate_instruction(builder, operand.as_ref())?;
                    let val_type = &operand._type;

                    let CXTypeKind::MemoryReference(inner) = &val_type.kind
                        else { unreachable!("generate_instruction: Expected memory alias type for expr, found {val_type}") };

                    let bc_type = builder.convert_fixed_cx_type(inner.as_ref())?;

                    let loaded_val = MIRValue::LoadOf(bc_type, Box::new(value.clone()));

                    let bytes = match &inner.kind {
                        CXTypeKind::Integer { bytes, .. } => *bytes,
                        CXTypeKind::PointerTo { .. } => 8,
                        _ => panic!("Invalid type for post increment: {inner:?}")
                    };

                    let one = MIRValue::IntImmediate {
                        val: *off as i64,
                        type_: MIRTypeKind::Signed { bytes }.into()
                    };

                    let incremented = generate_algebraic_binop(
                        builder, inner.as_ref(),
                        loaded_val.clone(), one, builder.convert_fixed_cx_type(inner.as_ref())?,
                        &CXBinOp::Add
                    )?;

                    builder.add_instruction(
                        VirtualInstruction::Store {
                            memory: value,
                            value: incremented,
                            type_: builder.convert_fixed_cx_type(inner.as_ref())?
                        },
                        MIRType::unit()
                    )?;

                    Some(loaded_val)
                },

                CXUnOp::ExplicitCast(_) =>
                    generate_instruction(builder, operand.as_ref()),

                _ => todo!("generate_instruction for {:?}", operator)
            }
        },

        TCExprKind::If { condition, then_branch, else_branch } => {
            builder.push_scope();
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
                MIRType::unit()
            );

            builder.set_current_block(then_block);
            builder.generate_scoped(then_branch.as_ref());

            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block },
                MIRType::unit()
            );

            builder.set_current_block(else_block);

            if let Some(else_branch) = else_branch {
                builder.generate_scoped(else_branch.as_ref());
            }

            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block },
                MIRType::unit()
            );

            builder.pop_scope();
            builder.set_current_block(merge_block);
            Some(MIRValue::NULL)
        },

        TCExprKind::While { condition, body, pre_eval } => {
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
                MIRType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = builder.generate_scoped(condition.as_ref())?;

            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block,
                    false_block: merge_block
                },
                MIRType::unit()
            );

            builder.set_current_block(body_block);
            builder.generate_scoped(body.as_ref());

            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block },
                MIRType::unit()
            );

            builder.end_scope();
            builder.end_cond();
            Some(MIRValue::NULL)
        },

        TCExprKind::Switch { condition, block, cases, default_case } => {
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
                    value: condition_value.unwrap_or(MIRValue::NULL),
                    targets: sorted_cases.iter()
                        .enumerate()
                        .map(|(i, (case, _))| (*case, case_blocks[i]))
                        .collect(),
                    default: default_block.unwrap_or(merge_block)
                },
                MIRType::unit()
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
                        MIRType::unit()
                    );
                    next_index = case_iter.next();
                    builder.set_current_block(*case_block);
                }
                
                if *default_case == Some(i) {
                    let block = default_block.unwrap();
                    builder.add_instruction(
                        VirtualInstruction::Jump { target: block },
                        MIRType::unit()
                    );
                    builder.set_current_block(block);
                }
                
                generate_instruction(builder, expr)?;
            }
            
            builder.add_instruction(
                VirtualInstruction::Jump { target: merge_block },
                MIRType::unit()
            );
            
            builder.end_scope();
            Some(MIRValue::NULL)
        },

        TCExprKind::For { init, condition, increment, body } => {
            let condition_block = builder.create_block();
            let body_block = builder.create_block();
            let increment_block = builder.start_cont_point();
            let merge_block = builder.start_scope();

            generate_instruction(builder, init.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block },
                MIRType::unit()
            );

            builder.set_current_block(condition_block);
            let condition_value = generate_instruction(builder, condition.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: condition_value,
                    true_block: body_block,
                    false_block: merge_block
                },
                MIRType::unit()
            );

            builder.set_current_block(body_block);
            generate_instruction(builder, body.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: increment_block },
                MIRType::unit()
            );

            builder.set_current_block(increment_block);
            generate_instruction(builder, increment.as_ref())?;
            builder.add_instruction(
                VirtualInstruction::Jump { target: condition_block },
                MIRType::unit()
            );

            builder.end_scope();
            builder.end_cond();
            Some(MIRValue::NULL)
        },

        TCExprKind::Break => {
            let Some(merge) = builder.get_merge() else {
                log_error!("TYPE ERROR: Invalid break in outermost scope");
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: merge },
                MIRType::unit()
            );

            Some(MIRValue::NULL)
        },

        TCExprKind::Continue => {
            let Some(cond) = builder.get_continue() else {
                log_error!("TYPE ERROR: Invalid continue in outermost scope");
            };

            builder.add_instruction(
                VirtualInstruction::Jump { target: cond },
                MIRType::unit()
            );

            Some(MIRValue::NULL)
        },
        
        TCExprKind::SizeOf { _type } => {
            let type_size = builder.convert_cx_type(&expr._type)?
                .size();
            
            match type_size {
                BCTypeSize::Fixed(size) 
                    => Some(builder.int_const(size as i32, 8, true)),
                BCTypeSize::Variable(size_expr) 
                    => Some(size_expr)
            }
        },

        TCExprKind::Move { operand } => {
            let memory = generate_instruction(builder, operand.as_ref())?;
            let value = builder.add_instruction(
                VirtualInstruction::Temp {
                    value: MIRValue::LoadOf(MIRType::default_pointer(), Box::new(memory.clone()))
                },
                MIRType::default_pointer()
            )?;

            builder.add_instruction(
                VirtualInstruction::ZeroMemory {
                    memory,
                    _type: MIRType::default_pointer()
                },
                MIRType::unit()
            )?;

            Some(value)
        }
        
        TCExprKind::New { _type, array_length } => {
            const STANDARD_ALLOC : &str = "__stdalloc";
            const STANDARD_ARRAY_ALLOC : &str = "__stdallocarray";
            
            let type_as_bc = builder.convert_cx_type(_type)?;
            let type_size = type_as_bc.fixed_size();
            
            let size_imm = builder.int_const(type_size as i32, 8, true);
            
            match array_length {
                Some(len) => {
                    let func = builder.fn_ref(STANDARD_ARRAY_ALLOC)
                        .expect("INTERNAL PANIC: Standard array alloc function not found");
                    let len = generate_instruction(builder, len.as_ref())?;
                    
                    builder.add_instruction(
                        VirtualInstruction::DirectCall {
                            args: vec![size_imm, len],
                            method_sig: builder.fn_map.get(STANDARD_ARRAY_ALLOC).unwrap().clone(),
                        },
                        MIRType::default_pointer()
                    )
                },
                
                None => {
                    let func = builder.fn_ref(STANDARD_ALLOC)
                        .expect("INTERNAL PANIC: Standard alloc function not found");
                    
                    builder.add_instruction(
                        VirtualInstruction::DirectCall {
                            args: vec![size_imm],
                            method_sig: builder.fn_map.get(STANDARD_ALLOC).unwrap().clone(),
                        },
                        MIRType::default_pointer()
                    )
                },
            }
        },
        
        TCExprKind::InitializerList { indices } => {
            let expr_type = builder.convert_cx_type(&expr._type)?;
            
            let alloc = builder.add_instruction(
                VirtualInstruction::Allocate {
                    alignment: expr_type.alignment(),
                    _type: expr_type.clone(),
                },
                MIRType::default_pointer()
            )?;
            
            builder.add_instruction(
                VirtualInstruction::ZeroMemory {
                    memory: alloc.clone(),
                    _type: expr_type.clone(),
                },
                MIRType::unit()
            );
            
            for index in indices.iter() {
                let value = generate_instruction(builder, &index.value)?;
             
                let access = get_cx_struct_field_by_index(
                    builder,
                    &expr_type,
                    index.index
                ).unwrap_or_else(|| {
                    panic!("PANIC: Attempting to access non-existent field at index {} in struct {expr_type:?}", index.index);
                });
             
                let struct_access = builder.add_instruction(
                    VirtualInstruction::StructAccess {
                        struct_: alloc.clone(),
                        struct_type: expr_type.clone(),
                        field_offset: access.offset,
                        field_index: access.index,
                    },
                    MIRType::default_pointer()
                )?;
                
                builder.add_instruction(
                    VirtualInstruction::Store {
                        memory: struct_access,
                        value,
                        type_: access._type.clone()
                    },
                    MIRType::unit()
                )?;
            }
            
            Some(alloc)
        },

        TCExprKind::Taken |
        TCExprKind::Unit => unreachable!("generate_instruction: Expected expression, found {:?}", expr._type.kind),
    }
}

pub(crate) fn generate_binop(
    builder: &mut MIRBuilder,
    lhs: &TCExpr, rhs: &TCExpr,
    return_type: MIRType,
    op: &CXBinOp
) -> Option<MIRValue> {
    match op {
        CXBinOp::LAnd | CXBinOp::LOr => {
            // Short circuit evaluation for logical operators
            let previous_block = builder.current_block();
            let match_type = builder.convert_cx_type(&lhs._type).unwrap();
            let false_imm = builder.match_int_const(0, &match_type);

            let left_id = generate_instruction(builder, lhs)?;
            let left_cmp = builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: false_imm.clone(),
                    op: builder.cx_i_binop(&CXBinOp::NotEqual).unwrap()
                },
                MIRType::from(MIRTypeKind::Bool)
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
                    condition: left_cmp.clone(),
                    true_block,
                    false_block
                },
                MIRType::unit()
            );
            
            builder.set_current_block(no_short_circuit_block);
            
            let right_id = generate_instruction(builder, rhs)?;
            let right_cmp = builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: right_id,
                    right: false_imm,
                    op: builder.cx_i_binop(&CXBinOp::NotEqual).unwrap()
                },
                MIRType::from(MIRTypeKind::Bool)
            )?;
            
            builder.add_instruction(
                VirtualInstruction::Jump {
                    target: merge_block
                },
                MIRType::unit()
            );
            
            builder.set_current_block(merge_block);
            
            builder.add_instruction(
                VirtualInstruction::Phi {
                    predecessors: vec![
                        (left_cmp, previous_block),
                        (right_cmp, no_short_circuit_block),
                    ]
                },
                MIRType::from(MIRTypeKind::Bool)
            )
        },
        
        _ => {
            let left_id = generate_instruction(builder, lhs)?;
            let right_id = generate_instruction(builder, rhs)?;

            generate_algebraic_binop(
                builder,
                &lhs._type,
                left_id, right_id,
                return_type,
                op
            )
        },
    }
}

pub(crate) fn generate_algebraic_binop(
    builder: &mut MIRBuilder,
    cx_lhs_type: &CXType,
    left_id: MIRValue,
    right_id: MIRValue,
    return_type: MIRType,
    op: &CXBinOp
) -> Option<MIRValue> {
    match &cx_lhs_type.kind {
        CXTypeKind::Integer { signed: true, .. } => {
            builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_i_binop(op)?
                },
                return_type
            )
        },

        CXTypeKind::Integer { signed: false, .. } => {
            builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_u_binop(op)?
                },
                return_type
            )
        },
        
        CXTypeKind::Bool => {
            builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: left_id,
                    right: right_id,
                    op: builder.cx_i_binop(op)?
                },
                return_type
            )
        },

        CXTypeKind::PointerTo { .. } => {
            let CXTypeKind::PointerTo { inner_type: left_inner, .. } = &cx_lhs_type.kind else {
                builder.dump_current_fn();
                unreachable!("generate_binop: Expected pointer type for {left_id}, found {cx_lhs_type}") 
            };

            builder.add_instruction(
                VirtualInstruction::PointerBinOp {
                    left: left_id,
                    right: right_id,
                    ptr_type: builder.convert_fixed_cx_type(left_inner)?,
                    op: builder.cx_ptr_binop(op)?
                },
                return_type
            )
        },

        CXTypeKind::Float { .. } => {
            builder.add_instruction(
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
    builder: &mut MIRBuilder,
    prototype: &MIRFunctionPrototype,
) -> Option<()> {
    let last_instruction = builder.last_instruction();

    if let Some(last_instruction) = last_instruction {
        match last_instruction.instruction {
            VirtualInstruction::Return { .. } => {
                // If the last instruction is already a return, do nothing
                return Some(());
            },
            VirtualInstruction::GotoDefer => {
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
        MIRType::unit()
    );
    builder.set_current_block(return_block);

    if prototype.name == "main" {
        builder.add_return(Some(
            MIRValue::IntImmediate {
                val: 0,
                type_: MIRTypeKind::Signed { bytes: 4 }.into()
            }
        ));
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
    builder: &mut MIRBuilder,
    prototype: &MIRFunctionPrototype,
) -> Option<()> {
    if builder.function_defers() {
        let return_value = if prototype.return_type.is_void() {
            None
        } else {
            // Phi node for the return value
            Some(
                MIRValue::BlockResult {
                    block_id: BlockID::DeferredBlock(0),
                    value_id: 0
                }
            )
        };

        builder.enter_deferred_logic();
        builder.add_instruction(
            VirtualInstruction::Return {
                value: return_value,
            },
            MIRType::unit()
        )?;
    }
    
    Some(())
}