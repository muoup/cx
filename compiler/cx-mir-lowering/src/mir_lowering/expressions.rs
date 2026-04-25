//! Expression tree lowering from MIRExpression to LMIR
//!
//! This module handles lowering of MIRExpression (AST-style IR) to LMIR.

use cx_lmir::{
    types::{LMIRIntegerType, LMIRType, LMIRTypeKind},
    LMIRInstructionKind, LMIRIntBinOp, LMIRPtrBinOp, LMIRValue,
};
use cx_mir::mir::{
    data::MIRTypeKind,
    expression::{MIRExpression, MIRExpressionKind, MIRFunctionContract, StructInitialization},
    program::MIRFunction,
    r#type::MIRType,
};
use cx_util::CXResult;

use crate::builder::LMIRBuilder;

use super::binary_ops::{lower_binary_op, lower_unary_op};
use super::coercion::lower_type_conversion;
use super::control_flow::{
    lower_cswitch, lower_for, lower_if, lower_match, lower_return, lower_while,
};
use super::tagged_union::{
    get_tagged_union_tag, lower_construct_tagged_union, lower_tagged_union_get,
    lower_tagged_union_set,
};

enum AggregateMemberLayout {
    Standard {
        byte_offset: usize,
    },
    Bitfield {
        storage_byte_offset: usize,
        bit_offset: usize,
        bit_width: usize,
        storage_type: MIRType,
    },
}

fn aggregate_member_layout(
    aggregate_type: &MIRType,
    definitions: &cx_mir::mir::data::MIRTypeContext,
    member_index: usize,
) -> AggregateMemberLayout {
    let aggregate_type = definitions.memory_resident_type(aggregate_type);
    match &aggregate_type.kind {
        MIRTypeKind::Union { .. } => {
            let fields = definitions
                .aggregate_fields(aggregate_type)
                .expect("union type must have fields");
            let field = fields
                .get(member_index)
                .unwrap_or_else(|| panic!("member index {member_index} out of bounds"));
            match field {
                cx_mir::mir::r#type::MIRField::Standard { .. } => {
                    AggregateMemberLayout::Standard { byte_offset: 0 }
                }
                cx_mir::mir::r#type::MIRField::Bitfield {
                    integer_type_id,
                    width,
                    ..
                } => {
                    let storage_type = definitions
                        .get(*integer_type_id)
                        .unwrap_or_else(|| panic!("Unknown type id {}", integer_type_id.0))
                        .clone();
                    AggregateMemberLayout::Bitfield {
                        storage_byte_offset: 0,
                        bit_offset: 0,
                        bit_width: *width,
                        storage_type,
                    }
                }
            }
        }
        MIRTypeKind::Structured { .. } => {
            let fields = definitions
                .aggregate_fields(aggregate_type)
                .expect("structured type must have fields");
            let mut offset = 0usize;
            let mut active_storage: Option<(MIRType, usize, usize)> = None;

            for (index, field) in fields.iter().enumerate() {
                match field {
                    cx_mir::mir::r#type::MIRField::Standard { type_id, .. } => {
                        if let Some((storage_type, storage_offset, used_bits)) =
                            active_storage.take()
                        {
                            offset = storage_offset
                                + (used_bits.div_ceil(storage_type.type_size(definitions) * 8))
                                    * storage_type.type_size(definitions);
                        }

                        let field_type = definitions
                            .get(*type_id)
                            .unwrap_or_else(|| panic!("Unknown type id {}", type_id.0));
                        let alignment = field_type.type_alignment(definitions);
                        offset = offset.div_ceil(alignment) * alignment;

                        if index == member_index {
                            return AggregateMemberLayout::Standard {
                                byte_offset: offset,
                            };
                        }

                        offset += field_type.type_size(definitions);
                    }
                    cx_mir::mir::r#type::MIRField::Bitfield {
                        integer_type_id,
                        width,
                        ..
                    } => {
                        let storage_type = definitions
                            .get(*integer_type_id)
                            .unwrap_or_else(|| panic!("Unknown type id {}", integer_type_id.0))
                            .clone();
                        let storage_bits = storage_type.type_size(definitions) * 8;
                        let storage_align = storage_type.type_alignment(definitions);

                        if *width == 0 {
                            active_storage = None;
                            offset = offset.div_ceil(storage_align) * storage_align;
                            continue;
                        }

                        let (storage_offset, bit_offset) = match active_storage.take() {
                            Some((active_type, storage_offset, used_bits))
                                if active_type.contextual_eq(&storage_type, definitions)
                                    && used_bits + *width <= storage_bits =>
                            {
                                (storage_offset, used_bits)
                            }
                            Some((active_type, storage_offset, used_bits)) => {
                                offset = storage_offset
                                    + (used_bits.div_ceil(active_type.type_size(definitions) * 8))
                                        * active_type.type_size(definitions);
                                offset = offset.div_ceil(storage_align) * storage_align;
                                (offset, 0)
                            }
                            None => {
                                offset = offset.div_ceil(storage_align) * storage_align;
                                (offset, 0)
                            }
                        };

                        if index == member_index {
                            return AggregateMemberLayout::Bitfield {
                                storage_byte_offset: storage_offset,
                                bit_offset,
                                bit_width: *width,
                                storage_type,
                            };
                        }

                        active_storage = Some((storage_type, storage_offset, bit_offset + *width));
                    }
                }
            }

            panic!("member index {member_index} out of bounds");
        }
        _ => panic!("member access on non-aggregate type"),
    }
}

fn lower_member_storage_address(
    builder: &mut LMIRBuilder,
    base: &MIRExpression,
    aggregate_type: &MIRType,
    member_index: usize,
    byte_offset: usize,
) -> CXResult<LMIRValue> {
    let bc_base = lower_expression(builder, base)?;
    if aggregate_type.is_c_union() {
        return Ok(bc_base);
    }

    let bc_struct_type = builder.convert_cx_type(aggregate_type);
    builder.add_new_instruction(
        LMIRInstructionKind::StructAccess {
            struct_: bc_base,
            struct_type: bc_struct_type,
            field_index: member_index,
            field_offset: byte_offset,
        },
        LMIRType::default_pointer(),
        true,
    )
}

fn integer_lmir_type(ty: &LMIRType) -> LMIRIntegerType {
    let LMIRTypeKind::Integer(integer_type) = ty.kind else {
        panic!("bitfield storage type must lower to an integer");
    };
    integer_type
}

fn bit_mask(width: usize) -> i64 {
    if width >= 63 {
        -1
    } else {
        ((1_i64) << width) - 1
    }
}

fn lower_bitfield_read(
    builder: &mut LMIRBuilder,
    base: &MIRExpression,
    aggregate_type: &MIRType,
    member_index: usize,
) -> CXResult<Option<LMIRValue>> {
    let AggregateMemberLayout::Bitfield {
        storage_byte_offset,
        bit_offset,
        bit_width,
        storage_type,
    } = aggregate_member_layout(aggregate_type, &builder.type_definitions, member_index)
    else {
        return Ok(None);
    };

    let storage_addr = lower_member_storage_address(
        builder,
        base,
        aggregate_type,
        member_index,
        storage_byte_offset,
    )?;
    let storage_lmir_type = builder.convert_cx_type(&storage_type);
    let storage_int_type = integer_lmir_type(&storage_lmir_type);
    let loaded = builder.add_new_instruction(
        LMIRInstructionKind::Load {
            memory: storage_addr,
            _type: storage_lmir_type.clone(),
        },
        storage_lmir_type.clone(),
        true,
    )?;
    let shifted = builder.add_new_instruction(
        LMIRInstructionKind::IntegerBinOp {
            op: LMIRIntBinOp::LSHR,
            left: loaded,
            right: LMIRValue::IntImmediate {
                val: bit_offset as i64,
                _type: storage_int_type,
            },
        },
        storage_lmir_type.clone(),
        true,
    )?;
    builder
        .add_new_instruction(
            LMIRInstructionKind::IntegerBinOp {
                op: LMIRIntBinOp::BAND,
                left: shifted,
                right: LMIRValue::IntImmediate {
                    val: bit_mask(bit_width),
                    _type: storage_int_type,
                },
            },
            storage_lmir_type,
            true,
        )
        .map(Some)
}

fn lower_bitfield_write(
    builder: &mut LMIRBuilder,
    target: &MIRExpression,
    value: &MIRExpression,
) -> CXResult<Option<LMIRValue>> {
    let MIRExpressionKind::MemberAccess {
        base,
        member_index,
        aggregate_type,
    } = &target.kind
    else {
        return Ok(None);
    };
    let AggregateMemberLayout::Bitfield {
        storage_byte_offset,
        bit_offset,
        bit_width,
        storage_type,
    } = aggregate_member_layout(aggregate_type, &builder.type_definitions, *member_index)
    else {
        return Ok(None);
    };

    let storage_addr = lower_member_storage_address(
        builder,
        base,
        aggregate_type,
        *member_index,
        storage_byte_offset,
    )?;
    lower_bitfield_write_at_address(
        builder,
        storage_addr,
        &storage_type,
        bit_offset,
        bit_width,
        value,
    )
    .map(Some)
}

fn lower_bitfield_write_at_address(
    builder: &mut LMIRBuilder,
    storage_addr: LMIRValue,
    storage_type: &MIRType,
    bit_offset: usize,
    bit_width: usize,
    value: &MIRExpression,
) -> CXResult<LMIRValue> {
    let storage_lmir_type = builder.convert_cx_type(&storage_type);
    let storage_int_type = integer_lmir_type(&storage_lmir_type);
    let loaded = builder.add_new_instruction(
        LMIRInstructionKind::Load {
            memory: storage_addr.clone(),
            _type: storage_lmir_type.clone(),
        },
        storage_lmir_type.clone(),
        true,
    )?;
    let value = lower_expression(builder, value)?;
    let mask = bit_mask(bit_width);
    let shifted_mask = mask << bit_offset;
    let cleared = builder.add_new_instruction(
        LMIRInstructionKind::IntegerBinOp {
            op: LMIRIntBinOp::BAND,
            left: loaded,
            right: LMIRValue::IntImmediate {
                val: !shifted_mask,
                _type: storage_int_type,
            },
        },
        storage_lmir_type.clone(),
        true,
    )?;
    let masked_value = builder.add_new_instruction(
        LMIRInstructionKind::IntegerBinOp {
            op: LMIRIntBinOp::BAND,
            left: value,
            right: LMIRValue::IntImmediate {
                val: mask,
                _type: storage_int_type,
            },
        },
        storage_lmir_type.clone(),
        true,
    )?;
    let shifted_value = builder.add_new_instruction(
        LMIRInstructionKind::IntegerBinOp {
            op: LMIRIntBinOp::SHL,
            left: masked_value,
            right: LMIRValue::IntImmediate {
                val: bit_offset as i64,
                _type: storage_int_type,
            },
        },
        storage_lmir_type.clone(),
        true,
    )?;
    let merged = builder.add_new_instruction(
        LMIRInstructionKind::IntegerBinOp {
            op: LMIRIntBinOp::BOR,
            left: cleared,
            right: shifted_value,
        },
        storage_lmir_type.clone(),
        true,
    )?;
    builder.add_new_instruction(
        LMIRInstructionKind::Store {
            memory: storage_addr,
            value: merged,
            _type: storage_lmir_type,
        },
        LMIRType::unit(),
        false,
    )
}

pub fn lower_expression(builder: &mut LMIRBuilder, expr: &MIRExpression) -> CXResult<LMIRValue> {
    match &expr.kind {
        MIRExpressionKind::BoolLiteral(value) => Ok(LMIRValue::IntImmediate {
            val: if *value { 1 } else { 0 },
            _type: LMIRIntegerType::I1,
        }),

        MIRExpressionKind::IntLiteral(val, _type, _signed) => {
            let bc_type = builder.convert_integer_type(_type);
            Ok(LMIRValue::IntImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::FloatLiteral(val, _type) => {
            let bc_type = builder.convert_float_type(_type);
            Ok(LMIRValue::FloatImmediate {
                val: *val,
                _type: bc_type,
            })
        }

        MIRExpressionKind::Unit => Ok(LMIRValue::NULL),

        MIRExpressionKind::Variable(name) => {
            if let Some(local_value) = builder.get_symbol(name) {
                return Ok(local_value);
            }

            if let Some(global_value) = builder.get_global_symbol(name.as_str()) {
                return Ok(global_value);
            }

            unreachable!("Variable '{}' not found in symbol table", name);
        }

        MIRExpressionKind::ContractVariable { name, force_param } => {
            if *force_param {
                let idx = builder
                    .current_prototype()
                    .params
                    .iter()
                    .position(|param| {
                        param.name.as_ref().map(String::as_str) == Some(name.as_str())
                    })
                    .expect("Contract variable not found in function parameters")
                    as u32;

                Ok(LMIRValue::ParameterRef(idx))
            } else {
                let Some(local_value) = builder.get_symbol(name) else {
                    unreachable!("Contract variable '{}' not found in symbol table", name);
                };

                Ok(local_value)
            }
        }

        MIRExpressionKind::FunctionReference { name } => Ok(LMIRValue::FunctionRef(name.clone())),

        // ===== Arithmetic & Logic =====
        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            lower_binary_op(builder, lhs, rhs, op, &expr._type)
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            lower_unary_op(builder, operand, op, &expr._type)
        }

        MIRExpressionKind::Typechange(inner) => lower_expression(builder, inner),

        MIRExpressionKind::RegionCreate {
            name,
            _type,
            initial_value,
        } => {
            let bc_type = builder.convert_cx_type(_type);

            let result = if let Some(initial_value) = initial_value {
                if _type.is_memory_resident() {
                    // OPTIMIZATION: The initial value expression returns a pointer to its buffer.
                    // Alias that buffer as the variable's buffer - no allocation or copy needed.
                    let bc_iv = lower_expression(builder, initial_value)?;

                    if let Some(name) = name {
                        builder.insert_symbol(name.clone(), bc_iv.clone());
                    }

                    return Ok(bc_iv);
                } else {
                    // Primitive type - allocate and store the value
                    let alloc = builder.add_new_instruction(
                        LMIRInstructionKind::Allocate {
                            alignment: bc_type.alignment(),
                            _type: bc_type.clone(),
                        },
                        LMIRType::default_pointer(),
                        true,
                    )?;
                    let init_val = lower_expression(builder, initial_value)?;
                    builder.add_new_instruction(
                        LMIRInstructionKind::Store {
                            memory: alloc.clone(),
                            value: init_val,
                            _type: bc_type.clone(),
                        },
                        LMIRType::unit(),
                        false,
                    )?;
                    alloc
                }
            } else {
                // No initialization - just allocate
                builder.add_new_instruction(
                    LMIRInstructionKind::Allocate {
                        alignment: bc_type.alignment(),
                        _type: bc_type,
                    },
                    LMIRType::default_pointer(),
                    true,
                )?
            };

            // Symbol table insertion (for non-memory-resident with init, or no init cases)
            if let Some(name) = name {
                builder.insert_symbol(name.clone(), result.clone());
            }

            Ok(result)
        }

        MIRExpressionKind::MemoryWrite { target, value } => {
            if let Some(result) = lower_bitfield_write(builder, target, value)? {
                return Ok(result);
            }

            let bc_target = lower_expression(builder, target)?;
            let bc_value = lower_expression(builder, value)?;
            let mir_value_type = &value._type;
            let bc_type = builder.convert_cx_type(mir_value_type);

            if mir_value_type.is_memory_resident() {
                builder.add_new_instruction(
                    LMIRInstructionKind::Memcpy {
                        dest: bc_target,
                        src: bc_value,
                        size: LMIRValue::IntImmediate {
                            val: bc_type.size() as i64,
                            _type: LMIRIntegerType::I64,
                        },
                        alignment: bc_type.alignment(),
                    },
                    LMIRType::unit(),
                    false,
                )
            } else {
                builder.add_new_instruction(
                    LMIRInstructionKind::Store {
                        memory: bc_target,
                        value: bc_value,
                        _type: bc_type,
                    },
                    LMIRType::unit(),
                    false,
                )
            }
        }

        MIRExpressionKind::RegionMove { source } => lower_expression(builder, source),

        MIRExpressionKind::RegionDuplicate { source } => {
            if let MIRExpressionKind::MemberAccess {
                base,
                member_index,
                aggregate_type,
            } = &source.kind
            {
                if let Some(result) =
                    lower_bitfield_read(builder, base, aggregate_type, *member_index)?
                {
                    return Ok(result);
                }
            }

            let _type = builder.convert_cx_type(&expr._type);
            let val = lower_expression(builder, source)?;

            if _type.is_memory_resident() {
                let new_region = builder.add_new_instruction(
                    LMIRInstructionKind::Allocate {
                        alignment: _type.alignment(),
                        _type: _type.clone(),
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;

                let bc_source = lower_expression(builder, source)?;
                let literal = builder.int_const(_type.size() as i32, LMIRIntegerType::I64);

                builder.add_new_instruction(
                    LMIRInstructionKind::Memcpy {
                        dest: new_region.clone(),
                        src: bc_source.clone(),
                        size: literal,
                        alignment: _type.alignment(),
                    },
                    LMIRType::unit(),
                    false,
                )?;

                Ok(new_region)
            } else {
                builder.add_new_instruction(
                    LMIRInstructionKind::Load {
                        memory: val,
                        _type: _type.clone(),
                    },
                    _type,
                    true,
                )
            }
        }

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

        MIRExpressionKind::Return {
            value,
            postcondition,
        } => {
            let val = value
                .as_ref()
                .map(|v| lower_expression(builder, v))
                .transpose()?;

            if let Some((binding, postcondition)) = postcondition {
                builder.push_scope(None, None);
                if let Some(binding) = binding {
                    if let Some(val) = val.clone() {
                        builder.insert_symbol(binding.clone(), val);
                    }
                }

                let result = lower_return(builder, val, Some(postcondition.as_ref()));
                builder.pop_scope()?;
                result
            } else {
                lower_return(builder, val, None)
            }
        }

        MIRExpressionKind::Block { statements } => {
            for statement in statements {
                lower_expression(builder, statement)?;
            }

            Ok(LMIRValue::NULL)
        }

        MIRExpressionKind::CallFunction {
            function,
            arguments,
            contract,
        } => lower_call(builder, function, contract, arguments, &expr._type),

        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => lower_type_conversion(builder, operand, *conversion, &expr._type),

        MIRExpressionKind::LifetimeStart {
            variable: _,
            _type: _,
        } => Ok(LMIRValue::NULL),
        MIRExpressionKind::LifetimeEnd {
            variable: _,
            _type: _,
        } => Ok(LMIRValue::NULL),
        MIRExpressionKind::LeakLifetime { expression } => lower_expression(builder, expression),
        MIRExpressionKind::Unsafe { expression } => lower_expression(builder, expression),

        MIRExpressionKind::MemberAccess {
            base,
            member_index,
            aggregate_type,
        } => {
            assert!(
                aggregate_type.is_structure() || aggregate_type.is_c_union(),
                "MemberAccess aggregate_type must be a memory-resident aggregate"
            );

            let bc_base = lower_expression(builder, base)?;
            if aggregate_type.is_union() {
                return Ok(bc_base);
            }

            let bc_struct_type = builder.convert_cx_type(aggregate_type);
            let AggregateMemberLayout::Standard {
                byte_offset: field_offset,
            } = aggregate_member_layout(aggregate_type, &builder.type_definitions, *member_index)
            else {
                panic!("bitfield member access must be loaded or stored directly");
            };

            builder.add_new_instruction(
                LMIRInstructionKind::StructAccess {
                    struct_: bc_base,
                    struct_type: bc_struct_type,
                    field_index: *member_index,
                    field_offset,
                },
                LMIRType::default_pointer(),
                true,
            )
        }

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
                LMIRInstructionKind::PointerBinOp {
                    op: LMIRPtrBinOp::ADD,
                    ptr_type: bc_element_type,
                    type_padded_size: element_size,
                    left: bc_array,
                    right: bc_index,
                },
                LMIRType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::PatternIs {
            lhs,
            sum_type,
            variant_index,
            inner_name,
        } => {
            let bc_lhs = lower_expression(builder, lhs)?;

            let alias = builder.add_new_instruction(
                LMIRInstructionKind::Alias {
                    value: bc_lhs.clone(),
                },
                LMIRType::default_pointer(),
                true,
            )?;

            builder.insert_symbol(inner_name.clone(), alias);
            let tag_ptr = get_tagged_union_tag(builder, bc_lhs, sum_type)?;
            let tag_value = builder.add_new_instruction(
                LMIRInstructionKind::Load {
                    memory: tag_ptr,
                    _type: LMIRType::from(LMIRTypeKind::Integer(LMIRIntegerType::I8)),
                },
                LMIRType::from(LMIRTypeKind::Integer(LMIRIntegerType::I8)),
                true,
            )?;

            let comparison = builder.add_new_instruction(
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::EQ,
                    left: tag_value,
                    right: LMIRValue::IntImmediate {
                        val: *variant_index as i64,
                        _type: LMIRIntegerType::I8,
                    },
                },
                LMIRType::bool(),
                true,
            )?;

            Ok(comparison)
        }

        MIRExpressionKind::TaggedUnionTag { value, sum_type } => {
            let bc_value = lower_expression(builder, value)?;
            get_tagged_union_tag(builder, bc_value, sum_type)
        }

        MIRExpressionKind::TaggedUnionGet { value, .. } => lower_tagged_union_get(builder, value),

        MIRExpressionKind::TaggedUnionSet {
            target,
            variant_index,
            inner_value,
            sum_type,
        } => lower_tagged_union_set(builder, target, *variant_index, inner_value, sum_type),

        MIRExpressionKind::ConstructTaggedUnion {
            variant_index,
            value,
            sum_type,
        } => lower_construct_tagged_union(builder, *variant_index, value, sum_type),

        // ===== Initializers =====
        MIRExpressionKind::ArrayInitializer {
            elements,
            element_type,
        } => lower_array_initializer(builder, elements, element_type),

        MIRExpressionKind::StructInitializer {
            initializations,
            struct_type,
        } => lower_struct_initializer(builder, initializations, struct_type),

        MIRExpressionKind::Break { scope_depth: _ } => {
            let Some(to) = builder.get_break_target().cloned() else {
                unreachable!("Break used outside of loop or switch context");
            };

            builder.add_new_instruction(
                LMIRInstructionKind::Jump { target: to },
                LMIRType::unit(),
                false,
            )
        }

        MIRExpressionKind::Continue { scope_depth: _ } => {
            let Some(to) = builder.get_continue_block().cloned() else {
                unreachable!("Continue used outside of loop context");
            };

            builder.add_new_instruction(
                LMIRInstructionKind::Jump { target: to },
                LMIRType::unit(),
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
    }
}

fn lower_call(
    builder: &mut LMIRBuilder,
    function: &MIRExpression,
    contract: &MIRFunctionContract,
    arguments: &[MIRExpression],
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    let return_type = builder.convert_cx_type(result_type);

    let fn_val = lower_expression(builder, function)?;
    let mut args = arguments
        .iter()
        .map(|arg| lower_expression(builder, arg))
        .collect::<CXResult<Vec<LMIRValue>>>()?;

    let signature = match &function._type.kind {
        MIRTypeKind::Function { signature } => signature.as_ref().clone(),
        MIRTypeKind::PointerTo { inner_type, .. } => {
            let inner_type = builder
                .type_definitions
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0));
            if let MIRTypeKind::Function { signature } = &inner_type.kind {
                signature.as_ref().clone()
            } else {
                unreachable!("Call expression function pointer must point to function type")
            }
        }

        _ => unreachable!("Call expression function must have function type"),
    };

    let bc_signature = builder.convert_cx_signature(&signature);

    if let Some(precondition) = &contract.precondition {
        builder.push_scope(None, None);

        for (arg_expr, param) in args.iter().cloned().zip(signature.params.iter()) {
            if let Some(name) = &param.name {
                builder.insert_symbol((*name).clone(), arg_expr);
            }
        }

        lower_contract_assertion(builder, precondition, "precondition violation")?;

        builder.pop_scope()?;
    }

    // Capture args for postcondition binding before adding return buffer
    let args_cloned = if contract.postcondition.is_some() {
        args.clone()
    } else {
        vec![]
    };

    let return_buffer = if signature.return_type.is_memory_resident() {
        let buffer = builder.add_new_instruction(
            LMIRInstructionKind::Allocate {
                alignment: return_type.alignment(),
                _type: return_type.clone(),
            },
            LMIRType::default_pointer(),
            true,
        )?;
        args.insert(0, buffer.clone());
        Some(buffer)
    } else {
        None
    };

    let value = if let LMIRValue::FunctionRef(func) = &fn_val {
        builder.add_new_instruction(
            LMIRInstructionKind::DirectCall {
                func: func.clone(),
                args,
                method_sig: bc_signature.clone(),
            },
            if return_buffer.is_some() {
                LMIRType::default_pointer()
            } else {
                return_type
            },
            true,
        )?
    } else {
        builder.add_new_instruction(
            LMIRInstructionKind::IndirectCall {
                method_sig: bc_signature,
                func_ptr: fn_val,
                args,
            },
            if return_buffer.is_some() {
                LMIRType::default_pointer()
            } else {
                return_type
            },
            true,
        )?
    };

    if let Some((ret_name, postcondition)) = &contract.postcondition {
        builder.push_scope(None, None);

        for (arg_expr, param) in args_cloned.into_iter().zip(signature.params.iter()) {
            if let Some(name) = &param.name {
                builder.insert_symbol((*name).clone(), arg_expr);
            }
        }

        if let Some(ret_name) = ret_name {
            builder.insert_symbol((*ret_name).clone(), value.clone());
        }

        let assumption = lower_expression(builder, postcondition)?;
        builder.add_new_instruction(
            LMIRInstructionKind::CompilerAssumption {
                condition: assumption,
            },
            LMIRType::unit(),
            false,
        )?;

        builder.pop_scope()?;
    }

    Ok(value)
}

pub(crate) fn lower_contract_assertion(
    builder: &mut LMIRBuilder,
    condition: &MIRExpression,
    message: &str,
) -> CXResult<()> {
    let condition_val = lower_expression(builder, condition)?;
    let message_global = builder.create_static_string(message.to_string());

    let Some(assert_prototype) = builder.get_prototype("__compiler_assert").cloned() else {
        return cx_util::CXError::create_result(
            "Function contract used but __compiler_assert not found. Ensure std::intrinsic::assertion is imported.".to_string()
        );
    };

    builder.add_new_instruction(
        LMIRInstructionKind::DirectCall {
            func: cx_util::identifier::CXIdent::from("__compiler_assert"),
            args: vec![condition_val, message_global],
            method_sig: assert_prototype.signature(),
        },
        LMIRType::unit(),
        false,
    )?;

    Ok(())
}

fn lower_array_initializer(
    builder: &mut LMIRBuilder,
    elements: &[MIRExpression],
    element_type: &cx_mir::mir::data::MIRType,
) -> CXResult<LMIRValue> {
    let bc_element_type = builder.convert_cx_type(element_type);
    let element_size = bc_element_type.size() as u64;

    let array_type = LMIRType::from(LMIRTypeKind::Array {
        element: Box::new(bc_element_type.clone()),
        size: elements.len(),
    });

    let allocation = builder.add_new_instruction(
        LMIRInstructionKind::Allocate {
            alignment: array_type.alignment(),
            _type: array_type,
        },
        LMIRType::default_pointer(),
        true,
    )?;

    for (i, elem) in elements.iter().enumerate() {
        let bc_elem = lower_expression(builder, elem)?;
        let elem_type = builder.get_value_type(&bc_elem);

        let elem_addr = builder.add_new_instruction(
            LMIRInstructionKind::PointerBinOp {
                op: LMIRPtrBinOp::ADD,
                ptr_type: bc_element_type.clone(),
                type_padded_size: element_size,
                left: allocation.clone(),
                right: LMIRValue::IntImmediate {
                    val: i as i64,
                    _type: LMIRIntegerType::I64,
                },
            },
            LMIRType::default_pointer(),
            true,
        )?;

        // Check if bc_elem is a pointer to an in-memory aggregate (struct/union/array)
        // If so, we need to use Memcpy to copy the actual bytes, not Store the pointer
        let bc_elem_type_for_memcpy = builder.convert_cx_type(element_type);
        if element_type.is_memory_resident() {
            builder.add_new_instruction(
                LMIRInstructionKind::Memcpy {
                    dest: elem_addr,
                    src: bc_elem,
                    size: LMIRValue::IntImmediate {
                        val: bc_elem_type_for_memcpy.size() as i64,
                        _type: LMIRIntegerType::I64,
                    },
                    alignment: bc_elem_type_for_memcpy.alignment(),
                },
                LMIRType::unit(),
                false,
            )?;
        } else {
            builder.add_new_instruction(
                LMIRInstructionKind::Store {
                    memory: elem_addr,
                    value: bc_elem,
                    _type: elem_type,
                },
                LMIRType::unit(),
                false,
            )?;
        }
    }

    Ok(allocation)
}

fn lower_struct_initializer(
    builder: &mut LMIRBuilder,
    initializations: &[StructInitialization],
    struct_type: &cx_mir::mir::data::MIRType,
) -> CXResult<LMIRValue> {
    let bc_struct_type = builder.convert_cx_type(struct_type);

    let allocation = builder.add_new_instruction(
        LMIRInstructionKind::Allocate {
            alignment: bc_struct_type.alignment(),
            _type: bc_struct_type.clone(),
        },
        LMIRType::default_pointer(),
        true,
    )?;

    for initialization in initializations {
        let layout = aggregate_member_layout(
            struct_type,
            &builder.type_definitions,
            initialization.field_index,
        );

        let field_offset = match layout {
            AggregateMemberLayout::Standard { byte_offset } => byte_offset,
            AggregateMemberLayout::Bitfield {
                storage_byte_offset,
                bit_offset,
                bit_width,
                storage_type,
            } => {
                let storage_addr = builder.add_new_instruction(
                    LMIRInstructionKind::StructAccess {
                        struct_: allocation.clone(),
                        struct_type: bc_struct_type.clone(),
                        field_index: initialization.field_index,
                        field_offset: storage_byte_offset,
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;
                lower_bitfield_write_at_address(
                    builder,
                    storage_addr,
                    &storage_type,
                    bit_offset,
                    bit_width,
                    &initialization.value,
                )?;
                continue;
            }
        };

        let bc_value = lower_expression(builder, &initialization.value)?;
        let mir_field_type = &initialization.value._type;
        let bc_field_type = builder.convert_cx_type(mir_field_type);

        let field_addr = builder.add_new_instruction(
            LMIRInstructionKind::StructAccess {
                struct_: allocation.clone(),
                struct_type: bc_struct_type.clone(),
                field_index: initialization.field_index,
                field_offset,
            },
            LMIRType::default_pointer(),
            true,
        )?;

        if mir_field_type.is_memory_resident() {
            builder.add_new_instruction(
                LMIRInstructionKind::Memcpy {
                    dest: field_addr,
                    src: bc_value,
                    size: LMIRValue::IntImmediate {
                        val: bc_field_type.size() as i64,
                        _type: LMIRIntegerType::I64,
                    },
                    alignment: bc_field_type.alignment(),
                },
                LMIRType::unit(),
                false,
            )?;
        } else {
            builder.add_new_instruction(
                LMIRInstructionKind::Store {
                    memory: field_addr,
                    value: bc_value,
                    _type: bc_field_type,
                },
                LMIRType::unit(),
                false,
            )?;
        }
    }

    Ok(allocation)
}

/// Generate LMIR for an MIR function
pub fn lower_function(builder: &mut LMIRBuilder, mir_fn: &MIRFunction) -> CXResult<()> {
    let return_buffer_size = if mir_fn.prototype.return_type.is_memory_resident() {
        Some(
            mir_fn
                .prototype
                .return_type
                .type_size(&builder.type_definitions),
        )
    } else {
        None
    };

    builder.new_function(mir_fn.prototype.clone(), return_buffer_size);
    assert_eq!(builder.scope_depth(), 1);

    let entry_block = builder.create_block(Some("entry"));
    builder.set_current_block(entry_block);

    let has_return_buffer = mir_fn.prototype.return_type.is_memory_resident();
    let param_offset = if has_return_buffer { 1 } else { 0 };

    for (i, param) in mir_fn.prototype.params.iter().enumerate() {
        if let Some(name) = &param.name {
            let bc_param_type = builder.convert_cx_type(&param._type);

            if !param._type.is_memory_resident() {
                let alloc = builder.add_new_instruction(
                    LMIRInstructionKind::Allocate {
                        alignment: bc_param_type.alignment(),
                        _type: bc_param_type.clone(),
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;
                builder.add_new_instruction(
                    LMIRInstructionKind::Store {
                        memory: alloc.clone(),
                        value: LMIRValue::ParameterRef((i + param_offset) as u32),
                        _type: bc_param_type,
                    },
                    LMIRType::unit(),
                    false,
                )?;
                builder.insert_symbol(name.clone(), alloc);
            } else {
                builder.insert_symbol(
                    name.clone(),
                    LMIRValue::ParameterRef((i + param_offset) as u32),
                );
            }
        }
    }

    assert_eq!(
        builder.scope_depth(),
        1,
        "Scope should be at function base after parameter insertion"
    );

    lower_expression(builder, &mir_fn.body)?;

    assert_eq!(
        builder.scope_depth(),
        1,
        "Scope should be at function base before finishing function"
    );

    builder.finish_function()?;
    Ok(())
}
