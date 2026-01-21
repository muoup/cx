//! Expression tree lowering from MIRExpression to bytecode
//!
//! This module handles lowering of MIRExpression (AST-style IR) to bytecode,
//! replacing the old instruction-based lowering which worked with SSA-style MIR.

use cx_bytecode_data::{
    types::{BCFloatType, BCIntegerType, BCType, BCTypeKind},
    BCInstructionKind, BCPtrBinOp, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRPtrBinOp, MIRPtrDiffBinOp, MIRUnOp},
    program::MIRFunction,
    types::{MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::builder::BCBuilder;

/// Main entry point for expression lowering
///
/// Lowers a MIRExpression tree to a bytecode value, generating SSA registers
/// for intermediate results as needed.
pub fn lower_expression(builder: &mut BCBuilder, expr: &MIRExpression) -> CXResult<BCValue> {
    match &expr.kind {
        // ===== Literals =====
        MIRExpressionKind::BoolLiteral(value) => Ok(BCValue::IntImmediate {
            val: if *value { 1 } else { 0 },
            _type: BCIntegerType::I1,
        }),

        MIRExpressionKind::IntLiteral(val, _type, _signed) => {
            let bc_type = builder.convert_integer_type(_type);
            Ok(BCValue::IntImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::FloatLiteral(val, _type) => {
            let bc_type = builder.convert_float_type(_type);
            Ok(BCValue::FloatImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::Unit => Ok(BCValue::NULL),
        MIRExpressionKind::Null => Ok(BCValue::NULL),

        MIRExpressionKind::Variable(name) => {
            if let Some(local_value) = builder.get_symbol(name) {
                return Ok(local_value);
            }

            if let Some(global_value) = builder.get_global_symbol(name.as_str()) {
                return Ok(global_value);
            }

            unreachable!("Variable '{}' not found in symbol table", name);
        }

        MIRExpressionKind::FunctionReference { .. } => {
            let MIRTypeKind::Function { prototype } = &expr._type.kind else {
                unreachable!("FunctionReference must have function type");
            };

            Ok(BCValue::FunctionRef(prototype.name.clone()))
        }

        // ===== Arithmetic & Logic =====
        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            lower_binary_op(builder, lhs, rhs, op, &expr._type)
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            lower_unary_op(builder, operand, op, &expr._type)
        }

        // ===== Memory Operations =====
        MIRExpressionKind::MemoryRead { source } => {
            let bc_source = lower_expression(builder, source)?;
            let bc_type = builder.convert_cx_type(&expr._type);

            builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: bc_source,
                    _type: bc_type,
                },
                builder.convert_cx_type(&expr._type),
                true,
            )
        }

        MIRExpressionKind::Typechange(inner) => lower_expression(builder, inner),

        MIRExpressionKind::CreateStackVariable { name, _type } => {
            let bc_type = builder.convert_cx_type(_type);

            let result = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: bc_type.alignment(),
                    _type: bc_type,
                },
                BCType::default_pointer(),
                true,
            )?;

            if let Some(name) = name {
                builder.insert_symbol(name.clone(), result.clone());
            }

            Ok(result)
        }

        MIRExpressionKind::MemoryWrite { target, value } => {
            let bc_target = lower_expression(builder, target)?;
            let bc_value = lower_expression(builder, value)?;
            let mir_value_type = &value._type;
            let bc_type = builder.convert_cx_type(mir_value_type);

            if mir_value_type.is_memory_resident() {
                builder.add_new_instruction(
                    BCInstructionKind::Memcpy {
                        dest: bc_target,
                        src: bc_value,
                        size: BCValue::IntImmediate {
                            val: bc_type.size() as i64,
                            _type: BCIntegerType::I64,
                        },
                        alignment: bc_type.alignment(),
                    },
                    BCType::unit(),
                    false,
                )
            } else {
                builder.add_new_instruction(
                    BCInstructionKind::Store {
                        memory: bc_target,
                        value: bc_value,
                        _type: bc_type,
                    },
                    BCType::unit(),
                    false,
                )
            }
        }

        MIRExpressionKind::Move { source } => {
            // Move is essentially an alias - just return the source expression
            lower_expression(builder, source)
        }

        MIRExpressionKind::CopyRegion { source, _type } => {
            let new_region = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: builder.convert_cx_type(_type).alignment(),
                    _type: builder.convert_cx_type(_type),
                },
                BCType::default_pointer(),
                true,
            )?;
            
            let bc_source = lower_expression(builder, source)?;
            
            builder.add_new_instruction(
                BCInstructionKind::Memcpy {
                    dest: new_region.clone(),
                    src: bc_source.clone(),
                    size: BCValue::IntImmediate {
                        val: builder.convert_cx_type(_type).size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: builder.convert_cx_type(_type).alignment(),
                },
                BCType::unit(),
                false,
            )?;
            
            Ok(bc_source)
        }

        // ===== Control Flow =====
        MIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(
            builder,
            condition,
            then_branch,
            else_branch.as_deref(),
            &expr._type,
        ),

        MIRExpressionKind::While {
            condition,
            body,
            pre_eval,
        } => lower_while(builder, condition, body, *pre_eval),

        MIRExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => lower_for(builder, init, condition, increment, body),

        MIRExpressionKind::Return { value } => lower_return(builder, value.as_deref()),

        MIRExpressionKind::Block { statements } => lower_block(builder, statements),

        // ===== Function Calls =====
        MIRExpressionKind::CallFunction {
            function,
            arguments,
        } => {
            let return_type = builder.convert_cx_type(&expr._type);

            let fn_val = lower_expression(builder, function)?;
            let mut args = arguments
                .iter()
                .map(|arg| lower_expression(builder, arg))
                .collect::<CXResult<Vec<BCValue>>>()?;

            let MIRTypeKind::Function { prototype } = &function._type.kind else {
                unreachable!("CallFunction must have function type");
            };

            let bc_prototype = builder.convert_cx_prototype(prototype);

            let return_buffer = if prototype.return_type.is_memory_resident() {
                let buffer = builder.add_new_instruction(
                    BCInstructionKind::Allocate {
                        alignment: return_type.alignment(),
                        _type: return_type.clone(),
                    },
                    BCType::default_pointer(),
                    true,
                )?;
                args.insert(0, buffer.clone());
                Some(buffer)
            } else {
                None
            };

            let value = if let BCValue::FunctionRef(_) = &fn_val {
                builder.add_new_instruction(
                    BCInstructionKind::DirectCall {
                        args,
                        method_sig: bc_prototype,
                    },
                    if return_buffer.is_some() {
                        BCType::default_pointer()
                    } else {
                        return_type
                    },
                    true,
                )?
            } else {
                builder.add_new_instruction(
                    BCInstructionKind::IndirectCall {
                        method_sig: bc_prototype,
                        func_ptr: fn_val,
                        args,
                    },
                    if return_buffer.is_some() {
                        BCType::default_pointer()
                    } else {
                        return_type
                    },
                    true,
                )?
            };

            Ok(value)
        }

        // ===== Type Conversion =====
        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => lower_type_conversion(builder, operand, *conversion, &expr._type),

        // ===== Lifetime Management =====
        // These are optimization hints - currently treated as no-ops
        MIRExpressionKind::LifetimeStart { variable: _, _type: _ } => {
            Ok(BCValue::NULL)
        }

        MIRExpressionKind::LifetimeEnd { variable: _, _type: _ } => {
            Ok(BCValue::NULL)
        }

        MIRExpressionKind::LeakLifetime { expression } => {
            lower_expression(builder, expression)
        }

        // ===== Aggregate Access =====
        MIRExpressionKind::StructFieldAccess {
            base,
            field_index,
            field_offset,
            struct_type,
        } => {
            let bc_base = lower_expression(builder, base)?;
            let bc_struct_type = builder.convert_cx_type(struct_type);

            builder.add_new_instruction(
                BCInstructionKind::StructAccess {
                    struct_: bc_base,
                    struct_type: bc_struct_type,
                    field_index: *field_index,
                    field_offset: *field_offset,
                },
                BCType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::UnionAliasAccess { base, .. } => lower_expression(builder, base),

        MIRExpressionKind::ArrayAccess {
            array,
            index,
            element_type,
        } => {
            let bc_array = lower_expression(builder, array)?;
            let bc_index = lower_expression(builder, index)?;
            let bc_element_type = builder.convert_cx_type(element_type);
            let element_size = bc_element_type.size() as u64;

            builder.add_new_instruction(
                BCInstructionKind::PointerBinOp {
                    op: BCPtrBinOp::ADD,
                    ptr_type: bc_element_type,
                    type_padded_size: element_size,
                    left: bc_array,
                    right: bc_index,
                },
                BCType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::TaggedUnionTag { value, sum_type } => {
            let bc_value = lower_expression(builder, value)?;
            let bc_sum_type = builder.convert_cx_type(sum_type);

            let BCTypeKind::Struct { fields, .. } = &bc_sum_type.kind else {
                unreachable!("TaggedUnion must lower to struct type");
            };

            let tag_offset = fields[0].1.size();

            builder.add_new_instruction(
                BCInstructionKind::StructAccess {
                    struct_: bc_value.clone(),
                    struct_type: bc_sum_type,
                    field_index: 1,
                    field_offset: tag_offset,
                },
                BCType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::TaggedUnionGet { value, .. } => {
            let bc_value = lower_expression(builder, value)?;
            Ok(bc_value)
        }

        MIRExpressionKind::TaggedUnionSet {
            target,
            variant_index,
            inner_value,
            sum_type,
        } => {
            let bc_target = lower_expression(builder, target)?;
            let bc_sum_type = builder.convert_cx_type(sum_type);

            let BCTypeKind::Struct { fields, .. } = &bc_sum_type.kind else {
                unreachable!("TaggedUnion must lower to struct type");
            };

            let tag_offset = fields[0].1.size();

            let tag_addr = builder.add_new_instruction(
                BCInstructionKind::StructAccess {
                    struct_: bc_target.clone(),
                    struct_type: bc_sum_type,
                    field_index: 1,
                    field_offset: tag_offset,
                },
                BCType::default_pointer(),
                true,
            )?;

            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: tag_addr,
                    value: BCValue::IntImmediate {
                        val: *variant_index as i64,
                        _type: BCIntegerType::I8,
                    },
                    _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                },
                BCType::unit(),
                false,
            )?;

            let bc_inner = lower_expression(builder, inner_value)?;
            let inner_type = builder.get_value_type(&bc_inner);

            if inner_type.is_structure() {
                builder.add_new_instruction(
                    BCInstructionKind::Memcpy {
                        dest: bc_target.clone(),
                        src: bc_inner,
                        size: BCValue::IntImmediate {
                            val: inner_type.size() as i64,
                            _type: BCIntegerType::I64,
                        },
                        alignment: inner_type.alignment(),
                    },
                    BCType::unit(),
                    false,
                )?;
            } else if !inner_type.is_void() {
                builder.add_new_instruction(
                    BCInstructionKind::Store {
                        memory: bc_target.clone(),
                        value: bc_inner,
                        _type: inner_type,
                    },
                    BCType::unit(),
                    false,
                )?;
            }

            Ok(bc_target)
        }

        MIRExpressionKind::ConstructTaggedUnion {
            variant_index,
            value,
            sum_type,
        } => {
            let bc_sum_type = builder.convert_cx_type(sum_type);

            let allocation = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: bc_sum_type.alignment(),
                    _type: bc_sum_type.clone(),
                },
                BCType::default_pointer(),
                true,
            )?;

            let BCTypeKind::Struct { fields, .. } = &bc_sum_type.kind else {
                unreachable!("TaggedUnion must lower to struct type");
            };

            let tag_offset = fields[0].1.size();

            let tag_addr = builder.add_new_instruction(
                BCInstructionKind::StructAccess {
                    struct_: allocation.clone(),
                    struct_type: bc_sum_type,
                    field_index: 1,
                    field_offset: tag_offset,
                },
                BCType::default_pointer(),
                true,
            )?;

            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: tag_addr,
                    value: BCValue::IntImmediate {
                        val: *variant_index as i64,
                        _type: BCIntegerType::I8,
                    },
                    _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                },
                BCType::unit(),
                false,
            )?;

            let bc_inner = lower_expression(builder, value)?;
            let inner_type = builder.get_value_type(&bc_inner);

            if inner_type.is_structure() {
                builder.add_new_instruction(
                    BCInstructionKind::Memcpy {
                        dest: allocation.clone(),
                        src: bc_inner,
                        size: BCValue::IntImmediate {
                            val: inner_type.size() as i64,
                            _type: BCIntegerType::I64,
                        },
                        alignment: inner_type.alignment(),
                    },
                    BCType::unit(),
                    false,
                )?;
            } else if !inner_type.is_void() {
                builder.add_new_instruction(
                    BCInstructionKind::Store {
                        memory: allocation.clone(),
                        value: bc_inner,
                        _type: inner_type,
                    },
                    BCType::unit(),
                    false,
                )?;
            }

            Ok(allocation)
        }

        MIRExpressionKind::ArrayInitializer {
            elements,
            element_type,
        } => {
            let bc_element_type = builder.convert_cx_type(element_type);
            let element_size = bc_element_type.size() as u64;

            let array_type = BCType::from(BCTypeKind::Array {
                element: Box::new(bc_element_type.clone()),
                size: elements.len(),
            });

            let allocation = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: array_type.alignment(),
                    _type: array_type,
                },
                BCType::default_pointer(),
                true,
            )?;

            for (i, elem) in elements.iter().enumerate() {
                let bc_elem = lower_expression(builder, elem)?;
                let elem_type = builder.get_value_type(&bc_elem);

                let elem_addr = builder.add_new_instruction(
                    BCInstructionKind::PointerBinOp {
                        op: BCPtrBinOp::ADD,
                        ptr_type: bc_element_type.clone(),
                        type_padded_size: element_size,
                        left: allocation.clone(),
                        right: BCValue::IntImmediate {
                            val: i as i64,
                            _type: BCIntegerType::I64,
                        },
                    },
                    BCType::default_pointer(),
                    true,
                )?;

                if elem_type.is_structure() {
                    builder.add_new_instruction(
                        BCInstructionKind::Memcpy {
                            dest: elem_addr,
                            src: bc_elem,
                            size: BCValue::IntImmediate {
                                val: elem_type.size() as i64,
                                _type: BCIntegerType::I64,
                            },
                            alignment: elem_type.alignment(),
                        },
                        BCType::unit(),
                        false,
                    )?;
                } else {
                    builder.add_new_instruction(
                        BCInstructionKind::Store {
                            memory: elem_addr,
                            value: bc_elem,
                            _type: elem_type,
                        },
                        BCType::unit(),
                        false,
                    )?;
                }
            }

            Ok(allocation)
        }

        MIRExpressionKind::StructInitializer {
            initializations,
            struct_type,
        } => {
            let bc_struct_type = builder.convert_cx_type(struct_type);

            let allocation = builder.add_new_instruction(
                BCInstructionKind::Allocate {
                    alignment: bc_struct_type.alignment(),
                    _type: bc_struct_type.clone(),
                },
                BCType::default_pointer(),
                true,
            )?;

            let BCTypeKind::Struct { fields, .. } = &bc_struct_type.kind else {
                unreachable!("StructInitializer must have struct type");
            };

            let mut current_offset = 0usize;
            let field_offsets: Vec<usize> = fields
                .iter()
                .map(|(_, field_type)| {
                    let alignment = field_type.alignment() as usize;
                    let padding = (alignment - (current_offset % alignment)) % alignment;
                    current_offset += padding;
                    let offset = current_offset;
                    current_offset += field_type.size();
                    offset
                })
                .collect();

            for (field_index, init_expr) in initializations {
                let bc_value = lower_expression(builder, init_expr)?;
                let mir_field_type = &init_expr._type;
                let bc_field_type = builder.convert_cx_type(mir_field_type);

                let field_addr = builder.add_new_instruction(
                    BCInstructionKind::StructAccess {
                        struct_: allocation.clone(),
                        struct_type: bc_struct_type.clone(),
                        field_index: *field_index,
                        field_offset: field_offsets[*field_index],
                    },
                    BCType::default_pointer(),
                    true,
                )?;

                if mir_field_type.is_memory_resident() {
                    builder.add_new_instruction(
                        BCInstructionKind::Memcpy {
                            dest: field_addr,
                            src: bc_value,
                            size: BCValue::IntImmediate {
                                val: bc_field_type.size() as i64,
                                _type: BCIntegerType::I64,
                            },
                            alignment: bc_field_type.alignment(),
                        },
                        BCType::unit(),
                        false,
                    )?;
                } else {
                    builder.add_new_instruction(
                        BCInstructionKind::Store {
                            memory: field_addr,
                            value: bc_value,
                            _type: bc_field_type,
                        },
                        BCType::unit(),
                        false,
                    )?;
                }
            }

            Ok(allocation)
        }

        MIRExpressionKind::Break => {
            let Some(to) = builder.get_break_target().cloned() else {
                unreachable!("Break used outside of loop or switch context");
            };
            
            builder.add_new_instruction(
                BCInstructionKind::Jump { target: to },
                BCType::unit(),
                false,
            )
        }

        MIRExpressionKind::Continue => {
            let Some(to) = builder.get_continue_block().cloned() else {
                unreachable!("Continue used outside of loop context");
            };
            
            builder.add_new_instruction(
                BCInstructionKind::Jump { target: to },
                BCType::unit(),
                false,
            )
        }

        MIRExpressionKind::CSwitch {
            condition,
            cases,
            default,
        } => lower_cswitch(builder, condition, cases, default.as_deref()),

        MIRExpressionKind::Match {
            condition,
            arms,
            default,
        } => lower_match(builder, condition, arms, default.as_deref()),

        MIRExpressionKind::Defer { .. } => {
            todo!("Defer lowering - to be implemented")
        }
    }
}

/// Lower a binary operation
fn lower_binary_op(
    builder: &mut BCBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRBinOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_lhs = lower_expression(builder, lhs)?;
    let bc_rhs = lower_expression(builder, rhs)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRBinOp::Integer { op, .. } => {
            let bc_op = convert_int_binop(op);
            BCInstructionKind::IntegerBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Float { op, .. } => {
            let bc_op = convert_float_binop(op);
            BCInstructionKind::FloatBinOp {
                op: bc_op,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::PtrDiff { op, ptr_inner } => {
            let bc_inner_type = builder.convert_cx_type(ptr_inner);
            let ptr_op = match op {
                MIRPtrDiffBinOp::ADD => BCPtrBinOp::ADD,
                MIRPtrDiffBinOp::SUB => BCPtrBinOp::SUB,
            };
            BCInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: bc_inner_type.clone(),
                type_padded_size: bc_inner_type.size() as u64,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
        MIRBinOp::Pointer { op } => {
            let ptr_op = match op {
                MIRPtrBinOp::EQ => BCPtrBinOp::EQ,
                MIRPtrBinOp::NE => BCPtrBinOp::NE,
                MIRPtrBinOp::LT => BCPtrBinOp::LT,
                MIRPtrBinOp::GT => BCPtrBinOp::GT,
                MIRPtrBinOp::LE => BCPtrBinOp::LE,
                MIRPtrBinOp::GE => BCPtrBinOp::GE,
            };
            BCInstructionKind::PointerBinOp {
                op: ptr_op,
                ptr_type: BCType::default_pointer(),
                type_padded_size: 1,
                left: bc_lhs,
                right: bc_rhs,
            }
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

/// Lower a unary operation
fn lower_unary_op(
    builder: &mut BCBuilder,
    operand: &MIRExpression,
    op: &MIRUnOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let instruction_kind = match op {
        MIRUnOp::LNOT => BCInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_bytecode_data::BCIntUnOp::LNOT,
        },
        MIRUnOp::BNOT => BCInstructionKind::IntegerUnOp {
            value: bc_operand,
            op: cx_bytecode_data::BCIntUnOp::BNOT,
        },
        MIRUnOp::NEG => {
            // Integer negation: subtract from zero
            let zero = BCValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            BCInstructionKind::IntegerBinOp {
                op: cx_bytecode_data::BCIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }
        MIRUnOp::FNEG => {
            BCInstructionKind::FloatUnOp {
                op: cx_bytecode_data::BCFloatUnOp::NEG,
                value: bc_operand,
            }
        }
        MIRUnOp::INEG => {
            // Signed integer negation
            let zero = BCValue::IntImmediate {
                val: 0,
                _type: match &bc_result_type.kind {
                    cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                    _ => panic!("Integer negation requires integer type"),
                },
            };
            BCInstructionKind::IntegerBinOp {
                op: cx_bytecode_data::BCIntBinOp::SUB,
                left: zero,
                right: bc_operand,
            }
        }

        MIRUnOp::PostIncrement(amt) | MIRUnOp::PreIncrement(amt) => {
            let loaded_val = builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: bc_operand.clone(),
                    _type: bc_result_type.clone(),
                },
                bc_result_type.clone(),
                true,
            )?;

            let result = builder.add_new_instruction(
                BCInstructionKind::IntegerBinOp {
                    op: cx_bytecode_data::BCIntBinOp::ADD,
                    left: loaded_val.clone(),
                    right: BCValue::IntImmediate {
                        val: *amt as i64,
                        _type: match &bc_result_type.kind {
                            cx_bytecode_data::types::BCTypeKind::Integer(itype) => *itype,
                            _ => panic!("PreIncrement requires integer type"),
                        },
                    },
                },
                bc_result_type.clone(),
                true,
            )?;

            builder.add_new_instruction(
                BCInstructionKind::Store {
                    memory: bc_operand,
                    value: result.clone(),
                    _type: bc_result_type.clone(),
                },
                BCType::unit(),
                false,
            )?;

            return match op {
                MIRUnOp::PreIncrement(_) => Ok(result),
                MIRUnOp::PostIncrement(_) => Ok(loaded_val),
                _ => unreachable!(),
            };
        }
    };

    builder.add_new_instruction(instruction_kind, bc_result_type, true)
}

/// Lower an if expression
fn lower_if(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    then_branch: &MIRExpression,
    else_branch: Option<&MIRExpression>,
    _result_type: &MIRType,
) -> CXResult<BCValue> {
    builder.push_scope(None, None);
    let bc_condition = lower_expression(builder, condition)?;
    builder.pop_scope();
    
    let then_block_id = builder.create_block(Some("then"));
    let else_block_id = builder.create_block(Some("else"));
    let merge_block_id = if else_branch.is_some() {
        builder.create_block(Some("if_merge"))
    } else {
        else_block_id.clone()
    };

    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: then_block_id.clone(),
            false_block: else_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(then_block_id.clone());
    lower_expression(builder, then_branch)?;

    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: merge_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(else_block_id.clone());
    if let Some(else_expr) = else_branch {
        lower_expression(builder, else_expr)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: merge_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(merge_block_id);
    Ok(BCValue::NULL)
}

/// Lower a while loop
fn lower_while(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    body: &MIRExpression,
    pre_eval: bool,
) -> CXResult<BCValue> {
    // Create blocks
    let condition_block_id = builder.create_block(Some("[while] condition"));
    let body_block_id = builder.create_block(Some("[while] body"));
    let exit_block_id = builder.create_block(Some("[while] exit"));

    // Push scope with continue -> condition, break -> exit
    builder.push_scope(Some(condition_block_id.clone()), Some(exit_block_id.clone()));

    // Jump to condition or body based on pre_eval
    if pre_eval {
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: condition_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: body_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    // Condition block
    builder.set_current_block(condition_block_id.clone());
    let bc_condition = lower_expression(builder, condition)?;
    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: body_block_id.clone(),
            false_block: exit_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Body block
    builder.set_current_block(body_block_id.clone());
    lower_expression(builder, body)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Exit block
    builder.set_current_block(exit_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

fn lower_for(
    builder: &mut BCBuilder,
    init: &MIRExpression,
    condition: &MIRExpression,
    increment: &MIRExpression,
    body: &MIRExpression,
) -> CXResult<BCValue> {
    lower_expression(builder, init)?;

    let condition_block_id = builder.create_block(Some("for_condition"));
    let body_block_id = builder.create_block(Some("for_body"));
    let increment_block_id = builder.create_block(Some("for_increment"));
    let merge_block_id = builder.create_block(Some("for_merge"));

    // Push scope with continue -> increment, break -> merge
    builder.push_scope(Some(increment_block_id.clone()), Some(merge_block_id.clone()));

    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(condition_block_id.clone());

    let bc_condition = lower_expression(builder, condition)?;

    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: body_block_id.clone(),
            false_block: merge_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(body_block_id.clone());
    lower_expression(builder, body)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: increment_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(increment_block_id.clone());
    lower_expression(builder, increment)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(merge_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a C-style switch statement
fn lower_cswitch(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    cases: &[(Box<MIRExpression>, Box<MIRExpression>)],
    default: Option<&MIRExpression>,
) -> CXResult<BCValue> {
    let bc_condition = lower_expression(builder, condition)?;

    let exit_block_id = builder.create_block(Some("switch_exit"));
    let default_block_id = if default.is_some() {
        builder.create_block(Some("switch_default"))
    } else {
        exit_block_id.clone()
    };

    // Push scope with break -> exit (C switch allows break but not continue)
    builder.push_scope(None, Some(exit_block_id.clone()));

    // Build targets for jump table
    let mut targets = Vec::new();
    let mut case_blocks = Vec::new();

    for (case_value, _) in cases {
        let case_block_id = builder.create_block(Some("switch_case"));
        case_blocks.push(case_block_id.clone());

        // Extract integer value from case expression
        if let MIRExpressionKind::IntLiteral(value, _, _) = &case_value.kind {
            targets.push((*value as u64, case_block_id));
        } else {
            panic!("CSwitch case must be an integer literal");
        }
    }

    // Emit jump table
    builder.add_new_instruction(
        BCInstructionKind::JumpTable {
            value: bc_condition,
            targets,
            default: default_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // C-style switch: cases fall through to next case unless there's a break
    // Each case body is lowered, and control flows to the next case (not exit)
    for (i, (_, case_body)) in cases.iter().enumerate() {
        builder.set_current_block(case_blocks[i].clone());
        lower_expression(builder, case_body)?;

        // Fall through to next case or exit
        let next_block = if i + 1 < case_blocks.len() {
            case_blocks[i + 1].clone()
        } else if default.is_some() {
            default_block_id.clone()
        } else {
            exit_block_id.clone()
        };

        builder.add_new_instruction(
            BCInstructionKind::Jump { target: next_block },
            BCType::unit(),
            false,
        )?;
    }

    // Default block
    if let Some(default_expr) = default {
        builder.set_current_block(default_block_id);
        lower_expression(builder, default_expr)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: exit_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(exit_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a match expression (non-fallthrough, like Rust match)
fn lower_match(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    arms: &[(Box<MIRExpression>, Box<MIRExpression>)],
    default: Option<&MIRExpression>,
) -> CXResult<BCValue> {
    let bc_condition = lower_expression(builder, condition)?;

    let exit_block_id = builder.create_block(Some("match_exit"));
    let default_block_id = if default.is_some() {
        builder.create_block(Some("match_default"))
    } else {
        exit_block_id.clone()
    };

    // Push scope with break -> exit
    builder.push_scope(None, Some(exit_block_id.clone()));

    // Build targets for jump table
    let mut targets = Vec::new();
    let mut arm_blocks = Vec::new();

    for (pattern, _) in arms {
        let arm_block_id = builder.create_block(Some("match_arm"));
        arm_blocks.push(arm_block_id.clone());

        // Extract integer value from pattern
        if let MIRExpressionKind::IntLiteral(value, _, _) = &pattern.kind {
            targets.push((*value as u64, arm_block_id));
        } else {
            panic!("Match pattern must be an integer literal");
        }
    }

    // Emit jump table
    builder.add_new_instruction(
        BCInstructionKind::JumpTable {
            value: bc_condition,
            targets,
            default: default_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    // Match arms do NOT fall through - each jumps to exit
    for (i, (_, arm_body)) in arms.iter().enumerate() {
        builder.set_current_block(arm_blocks[i].clone());
        lower_expression(builder, arm_body)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: exit_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    // Default block
    if let Some(default_expr) = default {
        builder.set_current_block(default_block_id);
        lower_expression(builder, default_expr)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: exit_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(exit_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a return statement
fn lower_return(builder: &mut BCBuilder, value: Option<&MIRExpression>) -> CXResult<BCValue> {
    let has_return_buffer = builder.current_prototype().temp_buffer.is_some();

    let bc_value = match value {
        Some(v) => Some(lower_expression(builder, v)?),
        None => None,
    };

    if has_return_buffer {
        let return_buffer = BCValue::ParameterRef(0);
        let return_type = builder.current_prototype().temp_buffer.clone().unwrap();

        if let Some(src) = bc_value {
            builder.add_new_instruction(
                BCInstructionKind::Memcpy {
                    dest: return_buffer.clone(),
                    src,
                    size: BCValue::IntImmediate {
                        val: return_type.size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: return_type.alignment(),
                },
                BCType::unit(),
                false,
            )?;
        }

        builder.add_new_instruction(
            BCInstructionKind::Return {
                value: Some(return_buffer),
            },
            BCType::unit(),
            false,
        )
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Return { value: bc_value },
            BCType::unit(),
            false,
        )
    }
}

/// Lower a block expression
fn lower_block(builder: &mut BCBuilder, statements: &[MIRExpression]) -> CXResult<BCValue> {
    builder.push_scope(None, None);

    let mut last_value = BCValue::NULL;

    for stmt in statements {
        last_value = lower_expression(builder, stmt)?;
    }

    builder.pop_scope();

    Ok(last_value)
}

/// Lower a type conversion
fn lower_type_conversion(
    builder: &mut BCBuilder,
    operand: &MIRExpression,
    conversion: MIRCoercion,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let coercion_type = convert_coercion(&conversion, &bc_operand, builder)?;

    builder.add_new_instruction(
        BCInstructionKind::Coercion {
            coercion_type,
            value: bc_operand,
        },
        bc_result_type,
        true,
    )
}

// ===== Helper functions =====

use cx_typechecker_data::mir::expression::{MIRFloatBinOp, MIRIntegerBinOp};

fn convert_int_binop(op: &MIRIntegerBinOp) -> cx_bytecode_data::BCIntBinOp {
    match op {
        MIRIntegerBinOp::ADD => cx_bytecode_data::BCIntBinOp::ADD,
        MIRIntegerBinOp::SUB => cx_bytecode_data::BCIntBinOp::SUB,
        MIRIntegerBinOp::MUL => cx_bytecode_data::BCIntBinOp::MUL,
        MIRIntegerBinOp::IMUL => cx_bytecode_data::BCIntBinOp::IMUL,
        MIRIntegerBinOp::DIV => cx_bytecode_data::BCIntBinOp::UDIV,
        MIRIntegerBinOp::IDIV => cx_bytecode_data::BCIntBinOp::IDIV,
        MIRIntegerBinOp::MOD => cx_bytecode_data::BCIntBinOp::UREM,
        MIRIntegerBinOp::IMOD => cx_bytecode_data::BCIntBinOp::IREM,
        MIRIntegerBinOp::EQ => cx_bytecode_data::BCIntBinOp::EQ,
        MIRIntegerBinOp::NE => cx_bytecode_data::BCIntBinOp::NE,
        MIRIntegerBinOp::LT => cx_bytecode_data::BCIntBinOp::ULT,
        MIRIntegerBinOp::LE => cx_bytecode_data::BCIntBinOp::ULE,
        MIRIntegerBinOp::GT => cx_bytecode_data::BCIntBinOp::UGT,
        MIRIntegerBinOp::GE => cx_bytecode_data::BCIntBinOp::UGE,
        MIRIntegerBinOp::ILT => cx_bytecode_data::BCIntBinOp::ILT,
        MIRIntegerBinOp::ILE => cx_bytecode_data::BCIntBinOp::ILE,
        MIRIntegerBinOp::IGT => cx_bytecode_data::BCIntBinOp::IGT,
        MIRIntegerBinOp::IGE => cx_bytecode_data::BCIntBinOp::IGE,
        MIRIntegerBinOp::LAND => cx_bytecode_data::BCIntBinOp::LAND,
        MIRIntegerBinOp::LOR => cx_bytecode_data::BCIntBinOp::LOR,
        MIRIntegerBinOp::BAND => cx_bytecode_data::BCIntBinOp::BAND,
        MIRIntegerBinOp::BOR => cx_bytecode_data::BCIntBinOp::BOR,
        MIRIntegerBinOp::BXOR => cx_bytecode_data::BCIntBinOp::BXOR,
    }
}

fn convert_float_binop(op: &MIRFloatBinOp) -> cx_bytecode_data::BCFloatBinOp {
    match op {
        MIRFloatBinOp::FADD => cx_bytecode_data::BCFloatBinOp::ADD,
        MIRFloatBinOp::FSUB => cx_bytecode_data::BCFloatBinOp::SUB,
        MIRFloatBinOp::FMUL => cx_bytecode_data::BCFloatBinOp::FMUL,
        MIRFloatBinOp::FDIV => cx_bytecode_data::BCFloatBinOp::FDIV,
        MIRFloatBinOp::EQ => cx_bytecode_data::BCFloatBinOp::EQ,
        MIRFloatBinOp::NEQ => cx_bytecode_data::BCFloatBinOp::NEQ,
        MIRFloatBinOp::FLT => cx_bytecode_data::BCFloatBinOp::FLT,
        MIRFloatBinOp::FLE => cx_bytecode_data::BCFloatBinOp::FLE,
        MIRFloatBinOp::FGT => cx_bytecode_data::BCFloatBinOp::FGT,
        MIRFloatBinOp::FGE => cx_bytecode_data::BCFloatBinOp::FGE,
    }
}

use cx_bytecode_data::BCCoercionType;

fn convert_coercion(
    coercion: &MIRCoercion,
    operand: &BCValue,
    builder: &BCBuilder,
) -> CXResult<BCCoercionType> {
    match coercion {
        MIRCoercion::ReinterpretBits => Ok(BCCoercionType::BitCast),
        MIRCoercion::IntToBool => {
            // Int to bool conversion - check if non-zero
            // For now, treat as truncation to i1
            // TODO: Implement proper int-to-bool comparison
            Ok(BCCoercionType::Trunc)
        }
        MIRCoercion::Integral { sextend, .. } => {
            if *sextend {
                Ok(BCCoercionType::SExtend)
            } else {
                Ok(BCCoercionType::ZExtend)
            }
        }
        MIRCoercion::FloatCast { .. } => {
            let from_type = match operand {
                BCValue::FloatImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Float(ft) = _type.kind {
                        ft
                    } else {
                        BCFloatType::F64
                    }
                }
                _ => BCFloatType::F64,
            };
            Ok(BCCoercionType::FloatCast { from: from_type })
        }
        MIRCoercion::IntToFloat { sextend, .. } => {
            let from_type = match operand {
                BCValue::IntImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Integer(it) = _type.kind {
                        it
                    } else {
                        BCIntegerType::I64
                    }
                }
                _ => BCIntegerType::I64,
            };
            Ok(BCCoercionType::IntToFloat {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::PtrToInt { .. } => Ok(BCCoercionType::PtrToInt),
        MIRCoercion::IntToPtr { sextend } => {
            let from_type = match operand {
                BCValue::IntImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Integer(it) = _type.kind {
                        it
                    } else {
                        BCIntegerType::I64
                    }
                }
                _ => BCIntegerType::I64,
            };
            Ok(BCCoercionType::IntToPtr {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::FloatToInt { sextend, .. } => {
            let from_type = match operand {
                BCValue::FloatImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Float(ft) = _type.kind {
                        ft
                    } else {
                        BCFloatType::F64
                    }
                }
                _ => BCFloatType::F64,
            };
            Ok(BCCoercionType::FloatToInt {
                from: from_type,
                sextend: *sextend,
            })
        }
    }
}

/// Generate bytecode for a MIR function
///
/// This is the main entry point for lowering a complete function from MIR to bytecode.
pub fn lower_function(builder: &mut BCBuilder, mir_fn: &MIRFunction) -> CXResult<()> {
    let prototype = builder.convert_cx_prototype(&mir_fn.prototype);
    builder.new_function(prototype);

    let entry_block = builder.create_block(Some("entry"));
    builder.set_current_block(entry_block);

    let has_return_buffer = mir_fn.prototype.return_type.is_memory_resident();
    let param_offset = if has_return_buffer { 1 } else { 0 };

    for (i, param) in mir_fn.prototype.params.iter().enumerate() {
        if let Some(name) = &param.name {
            let bc_param_type = builder.convert_cx_type(&param._type);

            if !param._type.is_memory_resident() {
                let alloc = builder.add_new_instruction(
                    BCInstructionKind::Allocate {
                        alignment: bc_param_type.alignment(),
                        _type: bc_param_type.clone(),
                    },
                    BCType::default_pointer(),
                    true,
                )?;
                builder.add_new_instruction(
                    BCInstructionKind::Store {
                        memory: alloc.clone(),
                        value: BCValue::ParameterRef((i + param_offset) as u32),
                        _type: bc_param_type,
                    },
                    BCType::unit(),
                    false,
                )?;
                builder.insert_symbol(name.clone(), alloc);
            } else {
                println!("{}: {} -> {}", name, param._type, bc_param_type);

                builder.insert_symbol(
                    name.clone(),
                    BCValue::ParameterRef((i + param_offset) as u32),
                );
            }
        }
    }

    let result = lower_expression(builder, &mir_fn.body)?;

    if has_return_buffer {
        let return_buffer = BCValue::ParameterRef(0);
        let return_type = builder.convert_cx_type(&mir_fn.prototype.return_type);

        builder.add_new_instruction(
            BCInstructionKind::Memcpy {
                dest: return_buffer.clone(),
                src: result,
                size: BCValue::IntImmediate {
                    val: return_type.size() as i64,
                    _type: BCIntegerType::I64,
                },
                alignment: return_type.alignment(),
            },
            BCType::unit(),
            false,
        )?;
        builder.add_new_instruction(
            BCInstructionKind::Return {
                value: Some(return_buffer),
            },
            BCType::unit(),
            false,
        )?;
    } else if !mir_fn.prototype.return_type.is_unit() {
        let return_value = if result == BCValue::NULL {
            BCValue::IntImmediate {
                val: 0,
                _type: BCIntegerType::I32,
            }
        } else {
            result
        };
        builder.add_new_instruction(
            BCInstructionKind::Return {
                value: Some(return_value),
            },
            BCType::unit(),
            false,
        )?;
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Return { value: None },
            BCType::unit(),
            false,
        )?;
    }

    builder.finish_function();
    Ok(())
}
