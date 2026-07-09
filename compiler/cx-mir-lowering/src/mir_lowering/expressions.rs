//! Expression tree lowering from MIRExpression to LMIR
//!
//! This module handles lowering of MIRExpression (AST-style IR) to LMIR.

use cx_lmir::{
    types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize},
    LMIRABISlot, LMIRFunctionSignature, LMIRInstructionKind, LMIRIntBinOp, LMIRParameterABI,
    LMIRPtrBinOp, LMIRValue, LinkageType,
};
use cx_log::CXResult;
use cx_mir::{
    mir::{
        data::{MIRFunction, MIRFunctionPrototype, MIRFunctionSignature, MIRTypeKind},
        expression::{
            MIRExpression, MIRExpressionKind, MIRFunctionContract, StructInitialization,
            SymbolValueOrigin,
        },
        pattern::MIRPattern,
        r#type::{MIRField, MIRType},
    },
    registry::MIRDecomposedRegistry,
    type_context::MIRTypeContext,
};
use cx_util::identifier::CXIdent;

use crate::builder::LMIRBuilder;

use super::abi::classify_signature;
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
    definitions: &MIRDecomposedRegistry,
    member_index: usize,
) -> AggregateMemberLayout {
    let aggregate_type = definitions
        .mem_ref_inner(aggregate_type)
        .unwrap_or(aggregate_type);

    match &aggregate_type.kind {
        MIRTypeKind::Union { variants } => {
            let field = variants
                .get(member_index)
                .unwrap_or_else(|| panic!("member index {member_index} out of bounds"));
            match field {
                MIRField::Standard { .. } => AggregateMemberLayout::Standard { byte_offset: 0 },
                MIRField::Bitfield {
                    integer_type_id,
                    width,
                    ..
                } => {
                    let storage_type = definitions.resolve_type_id(*integer_type_id).clone();
                    AggregateMemberLayout::Bitfield {
                        storage_byte_offset: 0,
                        bit_offset: 0,
                        bit_width: *width,
                        storage_type,
                    }
                }
            }
        }
        MIRTypeKind::Structured { fields } => {
            let mut offset = 0usize;
            let mut active_storage: Option<(cx_mir::mir::r#type::MIRTypeId, usize, usize, usize)> =
                None;

            for (index, field) in fields.iter().enumerate() {
                match field {
                    MIRField::Standard { type_id, .. } => {
                        if let Some((_, storage_offset, storage_size, _)) = active_storage.take() {
                            offset = storage_offset + storage_size;
                        }

                        let field_type = definitions.resolve_type_id(*type_id);
                        let field_layout =
                            definitions.type_layout(field_type).unwrap_or_else(|err| {
                                panic!("Failed to calculate field layout: {}", err.message())
                            });
                        offset = offset.div_ceil(field_layout.alignment) * field_layout.alignment;

                        if index == member_index {
                            return AggregateMemberLayout::Standard {
                                byte_offset: offset,
                            };
                        }

                        offset += field_layout.size;
                    }
                    MIRField::Bitfield {
                        integer_type_id,
                        width,
                        ..
                    } => {
                        let storage_type = definitions.resolve_type_id(*integer_type_id).clone();
                        let storage_layout =
                            definitions
                                .type_layout(&storage_type)
                                .unwrap_or_else(|err| {
                                    panic!("Failed to calculate bitfield layout: {}", err.message())
                                });
                        let storage_bits = storage_layout.size * 8;
                        let storage_align = storage_layout.alignment;

                        if *width == 0 {
                            active_storage = None;
                            offset = offset.div_ceil(storage_align) * storage_align;
                            continue;
                        }

                        let (storage_offset, bit_offset) = match active_storage.take() {
                            Some((active_type_id, storage_offset, _storage_size, used_bits))
                                if active_type_id == *integer_type_id
                                    && used_bits + *width <= storage_bits =>
                            {
                                (storage_offset, used_bits)
                            }
                            Some((_, storage_offset, storage_size, _)) => {
                                offset = storage_offset + storage_size;
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

                        active_storage = Some((
                            *integer_type_id,
                            storage_offset,
                            storage_layout.size,
                            bit_offset + *width,
                        ));
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

fn lower_region_duplicate(
    builder: &mut LMIRBuilder,
    source: &MIRExpression,
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    if let MIRExpressionKind::MemberAccess {
        base,
        member_index,
        aggregate_type,
    } = &source.kind
    {
        if let Some(result) = lower_bitfield_read(builder, base, aggregate_type, *member_index)? {
            return Ok(result);
        }
    }

    let lmir_type = builder.convert_cx_type(result_type);
    let layout = builder.type_layout(result_type);
    let source_value = lower_expression(builder, source)?;

    if lmir_type.is_void() {
        Ok(LMIRValue::NULL)
    } else if lmir_type.is_memory_resident() {
        let new_region = builder.add_new_instruction(
            LMIRInstructionKind::Allocate {
                alignment: layout.alignment as u8,
                _type: lmir_type.clone(),
            },
            LMIRType::default_pointer(),
            true,
        )?;
        let literal = builder.int_const(layout.size as i32, LMIRIntegerType::I64);

        builder.add_new_instruction(
            LMIRInstructionKind::Memcpy {
                dest: new_region.clone(),
                src: source_value,
                size: literal,
                alignment: layout.alignment as u8,
            },
            LMIRType::unit(),
            false,
        )?;

        Ok(new_region)
    } else {
        builder.add_new_instruction(
            LMIRInstructionKind::Load {
                memory: source_value,
                _type: lmir_type.clone(),
            },
            lmir_type,
            true,
        )
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
    } = aggregate_member_layout(aggregate_type, &builder.registry, member_index)
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
    } = aggregate_member_layout(aggregate_type, &builder.registry, *member_index)
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
    let storage_lmir_type = builder.convert_cx_type(storage_type);
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

        MIRExpressionKind::IntLiteral(val) => {
            let bc_type = builder.convert_cx_type(&expr._type);
            let LMIRTypeKind::Integer(i_type) = bc_type.kind else {
                unreachable!("Integer literal with non-integer type");
            };

            Ok(LMIRValue::IntImmediate {
                val: *val,
                _type: i_type,
            })
        }

        MIRExpressionKind::FloatLiteral(val) => {
            let bc_type = builder.convert_cx_type(&expr._type);
            let LMIRTypeKind::Float(f_type) = bc_type.kind else {
                unreachable!("Float literal with non-float type");
            };

            Ok(LMIRValue::FloatImmediate {
                val: *val,
                _type: f_type,
            })
        }

        MIRExpressionKind::Unit => Ok(LMIRValue::NULL),

        MIRExpressionKind::Variable { name, location } => {
            match location {
                SymbolValueOrigin::Local => {
                    if let Some(local_value) = builder.get_symbol(name) {
                        return Ok(local_value);
                    }
                }

                SymbolValueOrigin::Global => {
                    if let Some(global_value) = builder.get_global_symbol(name.as_str()) {
                        return Ok(global_value);
                    }
                }
            }

            unreachable!("Variable '{}' not found in symbol table", name);
        }

        MIRExpressionKind::ContractVariable { name, force_param } => {
            if *force_param {
                let idx = builder
                    .current_prototype()
                    .signature()
                    .params
                    .iter()
                    .position(|param| {
                        param.name.as_ref().map(CXIdent::as_str) == Some(name.as_str())
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
            _type,
            initial_value,
        } => {
            let bc_type = builder.convert_cx_type(_type);
            let layout = builder.type_layout(_type);

            let result = if let Some(initial_value) = initial_value {
                if bc_type.is_memory_resident() {
                    // OPTIMIZATION: The initial value expression returns a pointer to its buffer.
                    // Use that buffer directly as this anonymous region.
                    lower_expression(builder, initial_value)?
                } else {
                    // Primitive type - allocate and store the value
                    let alloc = builder.add_new_instruction(
                        LMIRInstructionKind::Allocate {
                            alignment: layout.alignment as u8,
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
                        alignment: layout.alignment as u8,
                        _type: bc_type,
                    },
                    LMIRType::default_pointer(),
                    true,
                )?
            };

            Ok(result)
        }

        MIRExpressionKind::BindRegion {
            name,
            _type,
            initial_region,
            adopting,
        } => {
            let initial_value = lower_expression(builder, initial_region)?;
            let bc_type = builder.convert_cx_type(_type);
            let layout = builder.type_layout(_type);
            let region = if *adopting
                || bc_type.is_memory_resident()
                || initial_region._type.is_memory_reference()
            {
                initial_value
            } else {
                let alloc = builder.add_new_instruction(
                    LMIRInstructionKind::Allocate {
                        alignment: layout.alignment as u8,
                        _type: bc_type.clone(),
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;
                builder.add_new_instruction(
                    LMIRInstructionKind::Store {
                        memory: alloc.clone(),
                        value: initial_value,
                        _type: bc_type,
                    },
                    LMIRType::unit(),
                    false,
                )?;
                alloc
            };
            builder.insert_symbol(name.clone(), region.clone());
            Ok(region)
        }

        MIRExpressionKind::RegionWrite { target, value } => {
            if let Some(result) = lower_bitfield_write(builder, target, value)? {
                return Ok(result);
            }

            let bc_target = lower_expression(builder, target)?;
            let bc_value = lower_expression(builder, value)?;
            let mir_value_type = &value._type;
            let bc_type = builder.convert_cx_type(mir_value_type);
            let layout = builder.type_layout(mir_value_type);

            if bc_type.is_memory_resident() {
                builder.add_new_instruction(
                    LMIRInstructionKind::Memcpy {
                        dest: bc_target.clone(),
                        src: bc_value,
                        size: LMIRValue::IntImmediate {
                            val: layout.size as i64,
                            _type: LMIRIntegerType::I64,
                        },
                        alignment: layout.alignment as u8,
                    },
                    LMIRType::unit(),
                    false,
                )?;
            } else {
                builder.add_new_instruction(
                    LMIRInstructionKind::Store {
                        memory: bc_target.clone(),
                        value: bc_value,
                        _type: bc_type,
                    },
                    LMIRType::unit(),
                    false,
                )?;
            }

            Ok(bc_target)
        }

        MIRExpressionKind::RegionMove { source } => {
            let lmir_type = builder.convert_cx_type(&source._type);

            if lmir_type.is_memory_resident() {
                lower_expression(builder, source.as_ref())
            } else {
                lower_region_duplicate(builder, source, &expr._type)
            }
        }

        MIRExpressionKind::RegionDuplicate { source } => {
            lower_region_duplicate(builder, source, &expr._type)
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

            if let Some(postcondition) = postcondition {
                builder.push_scope(None, None);
                if let Some(binding) = &postcondition.binding {
                    if let Some(val) = val.clone() {
                        builder.insert_symbol(binding.clone(), val);
                    }
                }

                let result = lower_return(builder, val, Some(postcondition));
                builder.pop_scope()?;
                result
            } else {
                lower_return(builder, val, None)
            }
        }

        MIRExpressionKind::Yield { value, .. } => {
            let Some(target) = builder.current_yield_target() else {
                panic!("Yield expression lowered outside of an active yield target");
            };
            let target_block = target.target_block.clone();
            let value = value
                .as_ref()
                .map(|value| lower_expression(builder, value))
                .transpose()?
                .unwrap_or(LMIRValue::NULL);

            builder.record_yield(value);
            builder.add_new_instruction(
                LMIRInstructionKind::Jump {
                    target: target_block,
                },
                LMIRType::unit(),
                false,
            )
        }

        MIRExpressionKind::Emit(_) => {
            panic!("emit expression reached runtime lowering");
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
            } = aggregate_member_layout(aggregate_type, &builder.registry, *member_index)
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
            let element_size = builder.type_layout(element_type).size.into();

            builder.add_new_instruction(
                LMIRInstructionKind::PointerBinOp {
                    op: LMIRPtrBinOp::ADD,
                    ptr_type: bc_element_type,
                    type_size: element_size,
                    left: bc_array,
                    right: bc_index,
                },
                LMIRType::default_pointer(),
                true,
            )
        }

        MIRExpressionKind::PatternIs { lhs, pattern } => {
            let bc_lhs = lower_expression(builder, lhs)?;

            let MIRPattern::TaggedUnionVariant {
                sum_type,
                variant_index,
                inner_name,
            } = pattern
            else {
                unreachable!("'is' patterns are only emitted for tagged union variants");
            };

            if let Some(inner_name) = inner_name {
                let alias = builder.add_new_instruction(
                    LMIRInstructionKind::Alias {
                        value: bc_lhs.clone(),
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;

                builder.insert_symbol(inner_name.clone(), alias);
            }
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

        MIRExpressionKind::TaggedUnionGet {
            value,
            variant_type,
        } => lower_tagged_union_get(builder, value, variant_type, &expr._type),

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
            subject_name,
            arms,
            default,
            exhaustive,
        } => lower_match(
            builder,
            condition,
            subject_name.as_ref(),
            arms,
            default.as_deref(),
            *exhaustive,
            &expr._type,
        ),
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
    let return_layout = builder.type_layout(result_type);

    let fn_val = lower_expression(builder, function)?;
    let signature = match &function._type.kind {
        MIRTypeKind::Function { signature } => signature.as_ref().clone(),
        MIRTypeKind::PointerTo { inner_type, .. } => {
            let inner_type = builder.registry.resolve_type_id(*inner_type);
            if let MIRTypeKind::Function { signature } = &inner_type.kind {
                signature.as_ref().clone()
            } else {
                unreachable!("Call expression function pointer must point to function type")
            }
        }

        _ => unreachable!("Call expression function must have function type"),
    };

    let call_signature = lower_call_signature(builder, &fn_val, &signature);
    let returns_indirectly = call_signature.return_abi.has_indirect_return_param();
    let mut args = Vec::new();
    for (i, arg) in arguments.iter().enumerate() {
        if let (Some(abi_param), Some(semantic_param)) =
            (call_signature.params.get(i), signature.params.get(i))
        {
            args.extend(lower_call_argument(
                builder,
                arg,
                &abi_param.abi,
                &semantic_param._type,
            )?);
        } else {
            args.push(lower_expression(builder, arg)?);
        }
    }

    if let Some(precondition) = &contract.precondition {
        builder.push_scope(None, None);

        for (arg_expr, param) in args.iter().cloned().zip(signature.params.iter()) {
            if let Some(name) = &param.name {
                builder.insert_symbol((*name).clone(), arg_expr);
            }
        }

        let assertion_prototype = contract
            .assertion_prototype
            .as_ref()
            .expect("precondition requires assertion prototype");
        lower_contract_assertion(
            builder,
            precondition,
            "precondition violation",
            assertion_prototype,
        )?;

        builder.pop_scope()?;
    }

    // Capture args for postcondition binding before adding return buffer
    let args_cloned = if contract.postcondition.is_some() {
        args.clone()
    } else {
        vec![]
    };

    let return_buffer = if returns_indirectly {
        let buffer = builder.add_new_instruction(
            LMIRInstructionKind::Allocate {
                alignment: return_layout.alignment as u8,
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

    let instruction_return_type = match &call_signature.return_abi {
        cx_lmir::LMIRReturnABI::Void => LMIRType::unit(),
        cx_lmir::LMIRReturnABI::Direct { .. } => return_type.clone(),
        cx_lmir::LMIRReturnABI::IndirectSret { .. } => LMIRType::unit(),
    };

    let value = if let LMIRValue::FunctionRef(func) = &fn_val {
        builder.add_new_instruction(
            LMIRInstructionKind::DirectCall {
                func: func.clone(),
                args,
                method_sig: call_signature.clone(),
            },
            instruction_return_type.clone(),
            true,
        )?
    } else {
        builder.add_new_instruction(
            LMIRInstructionKind::IndirectCall {
                method_sig: call_signature,
                func_ptr: fn_val,
                args,
            },
            instruction_return_type.clone(),
            true,
        )?
    };

    let value = if let Some(buffer) = return_buffer {
        buffer
    } else if return_type.is_memory_resident() && !returns_indirectly {
        let buffer = builder.add_new_instruction(
            LMIRInstructionKind::Allocate {
                alignment: return_layout.alignment as u8,
                _type: return_type.clone(),
            },
            LMIRType::default_pointer(),
            true,
        )?;
        builder.add_new_instruction(
            LMIRInstructionKind::Store {
                memory: buffer.clone(),
                value,
                _type: return_type.clone(),
            },
            LMIRType::unit(),
            false,
        )?;
        buffer
    } else {
        value
    };

    if let Some(postcondition) = &contract.postcondition {
        builder.push_scope(None, None);

        for (arg_expr, param) in args_cloned.into_iter().zip(signature.params.iter()) {
            if let Some(name) = &param.name {
                builder.insert_symbol((*name).clone(), arg_expr);
            }
        }

        if let Some(ret_name) = &postcondition.binding {
            builder.insert_symbol((*ret_name).clone(), value.clone());
        }

        let assumption = lower_expression(builder, &postcondition.condition)?;
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

fn lower_call_signature(
    builder: &LMIRBuilder,
    fn_val: &LMIRValue,
    semantic_signature: &MIRFunctionSignature,
) -> LMIRFunctionSignature {
    if let LMIRValue::FunctionRef(func) = fn_val {
        if let Some(prototype) = builder.get_prototype(func.as_str()) {
            return prototype.signature.clone();
        }
    }

    classify_signature(
        &semantic_signature.return_type,
        &semantic_signature.params,
        semantic_signature.var_args,
        &builder.registry,
    )
}

fn lower_call_argument(
    builder: &mut LMIRBuilder,
    arg: &MIRExpression,
    abi_arg: &LMIRParameterABI,
    semantic_param_type: &MIRType,
) -> CXResult<Vec<LMIRValue>> {
    let semantic_param_lmir_type = builder.convert_cx_type(semantic_param_type);

    match abi_arg {
        LMIRParameterABI::Direct { slots } => {
            if semantic_param_lmir_type.is_memory_resident() {
                let source = call_argument_region_source(arg);
                let source_value = lower_expression(builder, source)?;
                slots
                    .iter()
                    .map(|slot| lower_abi_slot_load(builder, source_value.clone(), slot))
                    .collect()
            } else {
                Ok(vec![lower_expression(builder, arg)?])
            }
        }
        LMIRParameterABI::Indirect { alignment } => {
            if let MIRExpressionKind::RegionDuplicate { source } = &arg.kind {
                return Ok(vec![lower_byval_copy_argument(
                    builder,
                    source,
                    semantic_param_type,
                    *alignment,
                )?]);
            }

            Ok(vec![lower_expression(builder, arg)?])
        }
    }
}

fn call_argument_region_source(arg: &MIRExpression) -> &MIRExpression {
    match &arg.kind {
        MIRExpressionKind::RegionDuplicate { source }
        | MIRExpressionKind::RegionMove { source } => source,
        _ => arg,
    }
}

fn lower_abi_slot_load(
    builder: &mut LMIRBuilder,
    source: LMIRValue,
    slot: &LMIRABISlot,
) -> CXResult<LMIRValue> {
    let memory = if slot.offset == 0 {
        source
    } else {
        builder.add_new_instruction(
            LMIRInstructionKind::PointerBinOp {
                op: LMIRPtrBinOp::ADD,
                ptr_type: slot._type.clone(),
                type_size: TypeSize::from(1),
                left: source,
                right: builder.int_const(slot.offset as i32, LMIRIntegerType::I64),
            },
            LMIRType::default_pointer(),
            true,
        )?
    };

    builder.add_new_instruction(
        LMIRInstructionKind::Load {
            memory,
            _type: slot._type.clone(),
        },
        slot._type.clone(),
        true,
    )
}

fn lower_byval_copy_argument(
    builder: &mut LMIRBuilder,
    source: &MIRExpression,
    pointee: &MIRType,
    alignment: u8,
) -> CXResult<LMIRValue> {
    let source_value = lower_expression(builder, source)?;
    let pointee_layout = builder.type_layout(pointee);
    let pointee_type = builder.convert_cx_type(pointee);
    let new_region = builder.add_new_instruction(
        LMIRInstructionKind::Allocate {
            alignment,
            _type: pointee_type,
        },
        LMIRType::default_pointer(),
        true,
    )?;
    let size = builder.int_const(pointee_layout.size as i32, LMIRIntegerType::I64);

    builder.add_new_instruction(
        LMIRInstructionKind::Memcpy {
            dest: new_region.clone(),
            src: source_value,
            size,
            alignment,
        },
        LMIRType::unit(),
        false,
    )?;

    Ok(new_region)
}

pub(crate) fn lower_contract_assertion(
    builder: &mut LMIRBuilder,
    condition: &MIRExpression,
    message: &str,
    assertion_prototype: &MIRFunctionPrototype,
) -> CXResult<()> {
    let condition_val = lower_expression(builder, condition)?;
    let message_global = builder.create_static_string(message.to_string());

    let mut assert_prototype = builder.convert_cx_prototype(assertion_prototype);
    assert_prototype.linkage = LinkageType::External;
    let assert_name = assert_prototype.name.clone();

    if builder.get_prototype(assert_name.as_str()).is_none() {
        builder.insert_fn_prototype(assert_prototype.clone());
    }

    builder.add_new_instruction(
        LMIRInstructionKind::DirectCall {
            func: assert_name,
            args: vec![condition_val, message_global],
            method_sig: assert_prototype.signature().clone(),
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
    let element_layout = builder.type_layout(element_type);
    let element_size = element_layout.size.into();

    let array_type = LMIRType::from(LMIRTypeKind::Array {
        element: Box::new(bc_element_type.clone()),
        size: elements.len(),
    });

    let allocation = builder.add_new_instruction(
        LMIRInstructionKind::Allocate {
            alignment: element_layout.alignment as u8,
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
                type_size: element_size,
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
        let bc_element_type = builder.convert_cx_type(element_type);
        if bc_element_type.is_memory_resident() {
            builder.add_new_instruction(
                LMIRInstructionKind::Memcpy {
                    dest: elem_addr,
                    src: bc_elem,
                    size: LMIRValue::IntImmediate {
                        val: element_layout.size as i64,
                        _type: LMIRIntegerType::I64,
                    },
                    alignment: element_layout.alignment as u8,
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
    let struct_layout = builder.type_layout(struct_type);

    let allocation = builder.add_new_instruction(
        LMIRInstructionKind::Allocate {
            alignment: struct_layout.alignment as u8,
            _type: bc_struct_type.clone(),
        },
        LMIRType::default_pointer(),
        true,
    )?;

    for initialization in initializations {
        let layout =
            aggregate_member_layout(struct_type, &builder.registry, initialization.field_index);

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
        let field_layout = builder.type_layout(mir_field_type);

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

        if bc_field_type.is_memory_resident() {
            builder.add_new_instruction(
                LMIRInstructionKind::Memcpy {
                    dest: field_addr,
                    src: bc_value,
                    size: LMIRValue::IntImmediate {
                        val: field_layout.size as i64,
                        _type: LMIRIntegerType::I64,
                    },
                    alignment: field_layout.alignment as u8,
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
    let bc_proto = builder.convert_cx_prototype(&mir_fn.prototype);

    builder.new_function(mir_fn.prototype.clone());
    assert_eq!(builder.scope_depth(), 1);

    let entry_block = builder.create_block(Some("entry"));
    builder.set_current_block(entry_block);

    let has_return_buffer = bc_proto.signature.return_abi.has_indirect_return_param();
    let mut lowered_param_index = if has_return_buffer { 1 } else { 0 };

    for (i, param) in mir_fn.prototype.signature().params.iter().enumerate() {
        let abi_param = bc_proto
            .signature
            .params
            .get(i)
            .expect("function ABI param missing for MIR parameter");
        let lowered_param_count = abi_param.abi.slot_count();

        if let Some(name) = &param.name {
            let param_type = builder.convert_cx_parameter_type(&param._type);
            let raw_param_type = builder.convert_cx_type(&param._type);
            let param_layout = builder.type_layout(&param._type);

            if raw_param_type.is_memory_resident()
                && matches!(abi_param.abi, LMIRParameterABI::Direct { .. })
            {
                let alloc = builder.add_new_instruction(
                    LMIRInstructionKind::Allocate {
                        alignment: param_layout.alignment as u8,
                        _type: raw_param_type.clone(),
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;
                if let LMIRParameterABI::Direct { slots } = &abi_param.abi {
                    for (slot_i, slot) in slots.iter().enumerate() {
                        let memory = if slot.offset == 0 {
                            alloc.clone()
                        } else {
                            builder.add_new_instruction(
                                LMIRInstructionKind::PointerBinOp {
                                    op: LMIRPtrBinOp::ADD,
                                    ptr_type: slot._type.clone(),
                                    type_size: TypeSize::from(1),
                                    left: alloc.clone(),
                                    right: builder
                                        .int_const(slot.offset as i32, LMIRIntegerType::I64),
                                },
                                LMIRType::default_pointer(),
                                true,
                            )?
                        };
                        builder.add_new_instruction(
                            LMIRInstructionKind::Store {
                                memory,
                                value: LMIRValue::ParameterRef(
                                    (lowered_param_index + slot_i) as u32,
                                ),
                                _type: slot._type.clone(),
                            },
                            LMIRType::unit(),
                            false,
                        )?;
                    }
                    lowered_param_index += slots.len();
                }
                builder.insert_symbol(name.clone(), alloc);
            } else if raw_param_type.is_memory_resident() {
                builder.insert_symbol(
                    name.clone(),
                    LMIRValue::ParameterRef(lowered_param_index as u32),
                );
                lowered_param_index += 1;
            } else {
                let alloc = builder.add_new_instruction(
                    LMIRInstructionKind::Allocate {
                        alignment: param_layout.alignment as u8,
                        _type: param_type.clone(),
                    },
                    LMIRType::default_pointer(),
                    true,
                )?;
                builder.add_new_instruction(
                    LMIRInstructionKind::Store {
                        memory: alloc.clone(),
                        value: LMIRValue::ParameterRef(lowered_param_index as u32),
                        _type: param_type,
                    },
                    LMIRType::unit(),
                    false,
                )?;
                builder.insert_symbol(name.clone(), alloc);
                lowered_param_index += 1;
            }
        } else {
            lowered_param_index += lowered_param_count;
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
