use cx_bytecode_data::types::{BCIntegerType, BCType, BCTypeKind};
use cx_bytecode_data::*;
use cx_typechecker_data::mir::name_mangling::base_mangle_deconstructor;
use cx_typechecker_data::mir::types::{MIRFunctionPrototype, MIRParameter, MIRType, MIRTypeKind};
use cx_util::CXResult;

use crate::builder::{BCBuilder, LivenessEntry};
use crate::mir_lowering::tagged_union::get_tagged_union_tag;

pub fn generate_deconstructor(builder: &mut BCBuilder, mir_type: &MIRType) -> CXResult<()> {
    let prototype = create_deconstructor_prototype(mir_type);
    builder.new_function(prototype.clone(), None);

    let entry_block = builder.create_block(Some("entry"));
    builder.set_current_block(entry_block);
    let self_value = BCValue::ParameterRef(0);

    builder.insert_symbol("this".into(), self_value.clone());
    generate_deconstructor_body(builder, &self_value, mir_type)?;

    builder.add_new_instruction(
        BCInstructionKind::Return { value: None },
        BCType::unit(),
        false,
    )?;

    builder.finish_function()?;
    Ok(())
}

pub fn create_deconstructor_prototype(mir_type: &MIRType) -> MIRFunctionPrototype {
    let ptr_type = MIRType::pointer_to(mir_type.clone());
    MIRFunctionPrototype {
        name: base_mangle_deconstructor(mir_type).into(),
        return_type: MIRType::unit(),
        params: vec![MIRParameter {
            name: Some("self".into()),
            _type: ptr_type,
        }],
        var_args: false,
        contract: Default::default(),
    }
}

fn generate_deconstructor_body(
    builder: &mut BCBuilder,
    self_ptr: &BCValue,
    mir_type: &MIRType,
) -> CXResult<()> {
    match &mir_type.kind {
        // Struct: deconstruct each field, then call destructor if exists
        MIRTypeKind::Structured { fields, .. } => {
            for (i, (_field_name, field_type)) in fields.iter().enumerate() {
                // Get field pointer
                let field_offset = calculate_field_offset(builder, mir_type, i);
                let field_ptr = builder.add_new_instruction(
                    BCInstructionKind::PointerBinOp {
                        op: BCPtrBinOp::ADD,
                        ptr_type: BCType::default_pointer(),
                        type_padded_size: field_offset as u64,
                        left: self_ptr.clone(),
                        right: BCValue::IntImmediate {
                            val: field_offset as i64,
                            _type: BCIntegerType::I64,
                        },
                    },
                    BCType::default_pointer(),
                    true,
                )?;

                if needs_deconstruction(builder, field_type) {
                    invoke_deconstruction(builder, &field_ptr, field_type)?;
                }
            }

            // Call user destructor if exists
            if let Some(destructor_proto) = builder.get_destructor(mir_type) {
                let destructor_proto = destructor_proto.clone();
                builder.add_new_instruction(
                    BCInstructionKind::DirectCall {
                        args: vec![self_ptr.clone()],
                        method_sig: destructor_proto,
                    },
                    BCType::unit(),
                    false,
                )?;
            }
        }

        // Tagged Union: deconstruct active variant based on tag
        MIRTypeKind::TaggedUnion { variants, .. } => {
            // For tagged unions, we need to:
            // 1. Read the tag
            // 2. Switch on tag value
            // 3. Deconstruct the active variant

            // Get tag pointer
            let tag_ptr = get_tagged_union_tag(builder, self_ptr.clone(), mir_type)?;
            let tag = builder.add_new_instruction(
                BCInstructionKind::Load {
                    memory: tag_ptr,
                    _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                },
                BCType::from(BCTypeKind::Integer(BCIntegerType::I8)),
                true,
            )?;

            // Create blocks for each variant and the end block
            let mut variant_blocks = Vec::new();
            for (i, _) in variants.iter().enumerate() {
                variant_blocks.push(builder.create_block(Some(&format!("variant_{}", i))));
            }
            let end_block = builder.create_block(Some("union_end"));

            // Create jump table for tag dispatch
            let mut targets = Vec::new();
            for (i, block) in variant_blocks.iter().enumerate() {
                targets.push((i as u64, block.clone()));
            }

            builder.add_new_instruction(
                BCInstructionKind::JumpTable {
                    value: tag,
                    targets,
                    default: end_block.clone(),
                },
                BCType::unit(),
                false,
            )?;

            // Process each variant
            for (i, (_, variant_type)) in variants.iter().enumerate() {
                builder.set_current_block(variant_blocks[i].clone());

                if needs_deconstruction(builder, variant_type) {
                    // The variant is stored starting at byte 0 of the tagged union
                    invoke_deconstruction(builder, &self_ptr, variant_type)?;
                }

                builder.add_new_instruction(
                    BCInstructionKind::Jump {
                        target: end_block.clone(),
                    },
                    BCType::unit(),
                    false,
                )?;
            }

            builder.set_current_block(end_block);
        }

        // Array: deconstruct each element
        MIRTypeKind::Array { size, inner_type } => {
            if needs_deconstruction(builder, inner_type) {
                let element_size = builder.convert_cx_type(inner_type).size();

                // Loop through array and deconstruct each element
                for i in 0..*size {
                    let element_offset = i * element_size;
                    let element_ptr = builder.add_new_instruction(
                        BCInstructionKind::PointerBinOp {
                            op: BCPtrBinOp::ADD,
                            ptr_type: BCType::default_pointer(),
                            type_padded_size: element_offset as u64,
                            left: self_ptr.clone(),
                            right: BCValue::IntImmediate {
                                val: element_offset as i64,
                                _type: BCIntegerType::I64,
                            },
                        },
                        BCType::default_pointer(),
                        true,
                    )?;

                    invoke_deconstruction(builder, &element_ptr, mir_type)?;
                }
            }
        }

        // Basic types: no deconstruction needed
        MIRTypeKind::Integer { .. } | MIRTypeKind::Float { .. } => {}
        MIRTypeKind::Unit | MIRTypeKind::Opaque { .. } => {}
        MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference(_) => {}
        MIRTypeKind::Function { .. } => {}

        MIRTypeKind::Union { .. } => {
            // Cannot deconstruct - no way to know which variant is active
        }
    }

    Ok(())
}

pub fn invoke_conditional_deconstruction(
    builder: &mut BCBuilder,
    value: &BCValue,
    liveness_value: &BCValue,
    mir_type: &MIRType,
) -> CXResult<BCValue> {
    let liveness = builder.add_new_instruction(
        BCInstructionKind::Load {
            memory: liveness_value.clone(),
            _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I1)),
        },
        BCType::from(BCTypeKind::Integer(BCIntegerType::I1)),
        true,
    )?;
    let condition = builder.add_new_instruction(
        BCInstructionKind::IntegerBinOp {
            op: BCIntBinOp::NE,
            left: liveness,
            right: BCValue::IntImmediate {
                val: 0,
                _type: BCIntegerType::I1,
            },
        },
        BCType::from(BCTypeKind::Integer(BCIntegerType::I1)),
        true,
    )?;
    let deconstruct_block = builder.create_block(Some("deconstruct"));
    let continue_block = builder.create_block(Some("continue"));

    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition,
            true_block: deconstruct_block.clone(),
            false_block: continue_block.clone(),
        },
        BCType::unit(),
        false,
    )?;
    
    builder.set_current_block(deconstruct_block);
    invoke_deconstruction(builder, value, mir_type)?;

    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: continue_block.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(continue_block);

    Ok(BCValue::NULL)
}

pub fn invoke_deconstruction(
    builder: &mut BCBuilder,
    value: &BCValue,
    mir_type: &MIRType,
) -> CXResult<BCValue> {
    let proto = builder.get_deconstructor(mir_type).unwrap().clone();

    builder.add_new_instruction(
        BCInstructionKind::DirectCall {
            args: vec![value.clone()],
            method_sig: proto,
        },
        BCType::unit(),
        false,
    )?;

    Ok(BCValue::NULL)
}

pub fn allocate_liveness_variable(builder: &mut BCBuilder) -> CXResult<BCValue> {
    let alloca = builder.add_new_instruction(
        BCInstructionKind::Allocate {
            _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I1)),
            alignment: 1,
        },
        BCType::default_pointer(),
        true,
    )?;

    builder.add_new_instruction(
        BCInstructionKind::Store {
            memory: alloca.clone(),
            value: BCValue::IntImmediate {
                val: 1,
                _type: BCIntegerType::I1,
            },
            _type: BCType::from(BCTypeKind::Integer(BCIntegerType::I1)),
        },
        BCType::unit(),
        false,
    )?;

    Ok(alloca)
}

fn calculate_field_offset(
    builder: &mut BCBuilder,
    struct_type: &MIRType,
    field_index: usize,
) -> usize {
    if let MIRTypeKind::Structured { fields, .. } = &struct_type.kind {
        let mut offset = 0;
        for i in 0..field_index {
            let (_, f_type) = &fields[i];
            offset = align_up(offset, builder.convert_cx_type(f_type).alignment() as usize);
            offset += builder.convert_cx_type(f_type).size();
        }
        // Align for the current field
        let (_, field_type) = &fields[field_index];
        offset = align_up(
            offset,
            builder.convert_cx_type(field_type).alignment() as usize,
        );
        return offset;
    }
    0
}

fn align_up(size: usize, alignment: usize) -> usize {
    if alignment == 0 {
        return size;
    }
    (size + alignment - 1) & !(alignment - 1)
}

pub fn needs_deconstruction(builder: &mut BCBuilder, mir_type: &MIRType) -> bool {
    if builder.get_deconstructor(mir_type).is_some() || builder.is_deconstructor_pending(mir_type) {
        return true;
    }

    let add_to_list = |builder: &mut BCBuilder| {
        builder.add_deconstructor_request(mir_type.clone());
    };

    if builder.get_destructor(mir_type).is_some() {
        add_to_list(builder);
        return true;
    }

    match &mir_type.kind {
        MIRTypeKind::Structured { fields: inner, .. }
        | MIRTypeKind::TaggedUnion {
            variants: inner, ..
        } => {
            for (_name, field_type) in inner.iter().cloned() {
                if needs_deconstruction(builder, &field_type) {
                    add_to_list(builder);
                    return true;
                }
            }
        }

        _ => {}
    }

    false
}

pub fn deconstruct(
    builder: &mut BCBuilder,
    value: &BCValue,
    mir_type: &MIRType,
) -> CXResult<BCValue> {
    if needs_deconstruction(builder, mir_type) {
        invoke_deconstruction(builder, value, mir_type)?;
    }
    Ok(BCValue::NULL)
}
