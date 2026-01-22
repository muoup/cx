//! Tagged union (sum type) lowering

use std::alloc::alloc;

use cx_bytecode_data::{
    types::{BCIntegerType, BCType, BCTypeKind},
    BCInstructionKind, BCValue,
};
use cx_typechecker_data::mir::{
    expression::MIRExpression,
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::BCBuilder;
use super::expressions::lower_expression;

/// Lower getting the tag from a tagged union
pub fn get_tagged_union_tag(
    builder: &mut BCBuilder,
    value: BCValue,
    sum_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_sum_type = builder.convert_cx_type(sum_type);

    let BCTypeKind::Struct { fields, .. } = &bc_sum_type.kind else {
        unreachable!("TaggedUnion must lower to struct type");
    };

    let tag_offset = fields[0].1.size();

    builder.add_new_instruction(
        BCInstructionKind::StructAccess {
            struct_: value,
            struct_type: bc_sum_type,
            field_index: 1,
            field_offset: tag_offset,
        },
        BCType::default_pointer(),
        true,
    )
}

/// Lower getting a variant value from a tagged union
pub fn lower_tagged_union_get(
    builder: &mut BCBuilder,
    value: &MIRExpression,
) -> CXResult<BCValue> {
    lower_expression(builder, value)
}

/// Lower setting a variant in a tagged union
pub fn lower_tagged_union_set(
    builder: &mut BCBuilder,
    target: &MIRExpression,
    variant_index: usize,
    inner_value: &MIRExpression,
    sum_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_target = lower_expression(builder, target)?;
    let bc_sum_type = builder.convert_cx_type(sum_type);

    let BCTypeKind::Struct { fields, .. } = &bc_sum_type.kind else {
        unreachable!("TaggedUnion must lower to struct type");
    };

    let tag_addr = get_tagged_union_tag(builder, bc_target.clone(), sum_type)?;
    builder.add_new_instruction(
        BCInstructionKind::Store {
            memory: tag_addr,
            value: BCValue::IntImmediate {
                val: variant_index as i64,
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

/// Lower constructing a new tagged union value
pub fn lower_construct_tagged_union(
    builder: &mut BCBuilder,
    variant_index: usize,
    value: &MIRExpression,
    sum_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_sum_type = builder.convert_cx_type(sum_type);

    let allocation = builder.add_new_instruction(
        BCInstructionKind::Allocate {
            alignment: bc_sum_type.alignment(),
            _type: bc_sum_type.clone(),
        },
        BCType::default_pointer(),
        true,
    )?;

    let tag_addr = get_tagged_union_tag(builder, allocation.clone(), sum_type)?;
    builder.add_new_instruction(
        BCInstructionKind::Store {
            memory: tag_addr,
            value: BCValue::IntImmediate {
                val: variant_index as i64,
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
