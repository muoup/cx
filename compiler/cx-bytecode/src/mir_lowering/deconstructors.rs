use std::mem::needs_drop;

use cx_bytecode_data::*;
use cx_bytecode_data::types::{BCIntegerType, BCType, BCTypeKind};
use cx_typechecker_data::mir::name_mangling::base_mangle_deconstructor;
use cx_typechecker_data::mir::types::{MIRType, MIRTypeKind, MIRFunctionPrototype, MIRParameter};
use cx_util::CXResult;

use crate::builder::BCBuilder;
use crate::mir_lowering::tagged_union::get_tagged_union_tag;

pub fn generate_deconstructor(
    builder: &mut BCBuilder,
    mir_type: &MIRType,
) -> CXResult<()> {
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

    builder.finish_function();
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
                    call_deconstructor(builder, &field_ptr, field_type)?;
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
            for (i, (variant_name, variant_type)) in variants.iter().enumerate() {
                builder.set_current_block(variant_blocks[i].clone());
                
                if needs_deconstruction(builder, variant_type) {
                    // The variant is stored starting at byte 0 of the tagged union
                    call_deconstructor(builder, &self_ptr, variant_type)?;
                }
                
                builder.add_new_instruction(
                    BCInstructionKind::Jump { target: end_block.clone() },
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

                    call_deconstructor(builder, &element_ptr, inner_type)?;
                }
            }
        }

        // Basic types: no deconstruction needed
        MIRTypeKind::Integer { .. } | MIRTypeKind::Float { .. } => {}
        MIRTypeKind::Unit | MIRTypeKind::Opaque { .. } => {}
        MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference(_) => {}
        MIRTypeKind::Function { .. } => {}

        // Untagged unions: cannot determine active variant
        MIRTypeKind::Union { .. } => {
            // Cannot deconstruct - no way to know which variant is active
        }
    }

    Ok(())
}

fn call_deconstructor(
    builder: &mut BCBuilder,
    value: &BCValue,
    mir_type: &MIRType,
) -> CXResult<BCValue> {
    let deconstructor_name = base_mangle_deconstructor(mir_type);

    // Get the deconstructor prototype, generate if not exists
    let deconstructor_proto = if let Some(proto) = builder.get_prototype(&deconstructor_name) {
        proto.clone()
    } else {
        // Generate the deconstructor
        generate_deconstructor(builder, mir_type)?;
        builder.get_prototype(&deconstructor_name).unwrap().clone()
    };

    // Call the deconstructor using DirectCall
    builder.add_new_instruction(
        BCInstructionKind::DirectCall {
            args: vec![value.clone()],
            method_sig: deconstructor_proto,
        },
        BCType::unit(),
        false,
    )?;

    Ok(BCValue::NULL)
}

fn calculate_field_offset(builder: &mut BCBuilder, struct_type: &MIRType, field_index: usize) -> usize {
    if let MIRTypeKind::Structured { fields, .. } = &struct_type.kind {
        let mut offset = 0;
        for i in 0..field_index {
            let (_, f_type) = &fields[i];
            offset = align_up(offset, builder.convert_cx_type(f_type).alignment() as usize);
            offset += builder.convert_cx_type(f_type).size();
        }
        // Align for the current field
        let (_, field_type) = &fields[field_index];
        offset = align_up(offset, builder.convert_cx_type(field_type).alignment() as usize);
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
        MIRTypeKind::Structured { fields: inner, .. } | MIRTypeKind::TaggedUnion { variants: inner, .. } => {
            for (_name, field_type) in inner.iter().cloned() {
                if needs_deconstruction(builder, &field_type) {
                    add_to_list(builder);
                    return true;
                }
            }
        }
        
        _ => {},
    }
    
    false
}