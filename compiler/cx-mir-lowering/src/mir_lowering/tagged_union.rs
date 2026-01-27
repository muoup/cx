use cx_lmir::{
    types::{LMIRIntegerType, LMIRType, LMIRTypeKind},
    LMIRInstructionKind, LMIRValue,
};
use cx_mir::mir::{
    expression::MIRExpression,
    types::{MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::builder::LMIRBuilder;
use super::expressions::lower_expression;

/// Lower getting the tag from a tagged union
pub fn get_tagged_union_tag(
    builder: &mut LMIRBuilder,
    value: LMIRValue,
    sum_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_sum_type = builder.convert_cx_type(sum_type);

    let LMIRTypeKind::Struct { fields, .. } = &bc_sum_type.kind else {
        unreachable!("TaggedUnion must lower to struct type");
    };

    let tag_offset = fields[0].1.size();

    builder.add_new_instruction(
        LMIRInstructionKind::StructAccess {
            struct_: value,
            struct_type: bc_sum_type,
            field_index: 1,
            field_offset: tag_offset,
        },
        LMIRType::default_pointer(),
        true,
    )
}

/// Lower getting a variant value from a tagged union
pub fn lower_tagged_union_get(
    builder: &mut LMIRBuilder,
    value: &MIRExpression,
) -> CXResult<LMIRValue> {
    lower_expression(builder, value)
}

/// Lower setting a variant in a tagged union
pub fn lower_tagged_union_set(
    builder: &mut LMIRBuilder,
    target: &MIRExpression,
    variant_index: usize,
    inner_value: &MIRExpression,
    sum_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_target = lower_expression(builder, target)?;

    let tag_addr = get_tagged_union_tag(builder, bc_target.clone(), sum_type)?;
    builder.add_new_instruction(
        LMIRInstructionKind::Store {
            memory: tag_addr,
            value: LMIRValue::IntImmediate {
                val: variant_index as i64,
                _type: LMIRIntegerType::I8,
            },
            _type: LMIRType::from(LMIRTypeKind::Integer(LMIRIntegerType::I8)),
        },
        LMIRType::unit(),
        false,
    )?;

    let bc_inner = lower_expression(builder, inner_value)?;
    let mir_inner_type = &inner_value._type;
    let inner_bc_type = builder.convert_cx_type(mir_inner_type);

    if mir_inner_type.is_structure() {
        builder.add_new_instruction(
            LMIRInstructionKind::Memcpy {
                dest: bc_target.clone(),
                src: bc_inner,
                size: LMIRValue::IntImmediate {
                    val: inner_bc_type.size() as i64,
                    _type: LMIRIntegerType::I64,
                },
                alignment: inner_bc_type.alignment(),
            },
            LMIRType::unit(),
            false,
        )?;
    } else if !matches!(mir_inner_type.kind, MIRTypeKind::Unit) {
        builder.add_new_instruction(
            LMIRInstructionKind::Store {
                memory: bc_target.clone(),
                value: bc_inner,
                _type: inner_bc_type,
            },
            LMIRType::unit(),
            false,
        )?;
    }

    Ok(bc_target)
}

/// Lower constructing a new tagged union value
pub fn lower_construct_tagged_union(
    builder: &mut LMIRBuilder,
    variant_index: usize,
    value: &MIRExpression,
    sum_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_sum_type = builder.convert_cx_type(sum_type);

    let allocation = builder.add_new_instruction(
        LMIRInstructionKind::Allocate {
            alignment: bc_sum_type.alignment(),
            _type: bc_sum_type.clone(),
        },
        LMIRType::default_pointer(),
        true,
    )?;

    let tag_addr = get_tagged_union_tag(builder, allocation.clone(), sum_type)?;
    builder.add_new_instruction(
        LMIRInstructionKind::Store {
            memory: tag_addr,
            value: LMIRValue::IntImmediate {
                val: variant_index as i64,
                _type: LMIRIntegerType::I8,
            },
            _type: LMIRType::from(LMIRTypeKind::Integer(LMIRIntegerType::I8)),
        },
        LMIRType::unit(),
        false,
    )?;

    let bc_inner = lower_expression(builder, value)?;
    let inner_type = builder.convert_cx_type(&value._type);

    if inner_type.is_structure() {
        builder.add_new_instruction(
            LMIRInstructionKind::Memcpy {
                dest: allocation.clone(),
                src: bc_inner,
                size: LMIRValue::IntImmediate {
                    val: inner_type.size() as i64,
                    _type: LMIRIntegerType::I64,
                },
                alignment: inner_type.alignment(),
            },
            LMIRType::unit(),
            false,
        )?;
    } else if !inner_type.is_void() {
        builder.add_new_instruction(
            LMIRInstructionKind::Store {
                memory: allocation.clone(),
                value: bc_inner,
                _type: inner_type,
            },
            LMIRType::unit(),
            false,
        )?;
    }

    Ok(allocation)
}
