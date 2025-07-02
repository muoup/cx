use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::{ElementID, ValueID, VirtualInstruction};
use cx_data_bytecode::types::{BCType, BCTypeKind, BCTypeSize};
use cx_util::bytecode_error_log;
use crate::builder::BytecodeBuilder;

pub(crate) struct StructAccess {
    pub(crate) offset: usize,
    pub(crate) index: usize,
    pub(crate) _type: BCType,
}

pub(crate) fn get_struct_field(
    builder: &BytecodeBuilder,
    _type: &BCType,
    name: &str
) -> Option<StructAccess> {
    let BCTypeKind::Struct { fields, .. } = &_type.kind else {
        bytecode_error_log!(builder, "PANIC: Expected struct type on access {name}, got: {:?}", _type);
    };
    
    let mut offset = 0;
    
    for (index, (field_name, field_type)) in fields.iter().enumerate() {
        if field_name == name {
            return Some(StructAccess {
                offset,
                index,
                _type: field_type.clone()
            });
        }
        
        offset += field_type.fixed_size();
    }
    
    None
}

pub(crate) fn allocate_variable(
    name: &str,
    builder: &mut BytecodeBuilder,
    var_type: &CXType,
) -> Option<ValueID> {
    let bc_type = builder.convert_cx_type(var_type)?;

    let memory = match bc_type.size() {
        BCTypeSize::Fixed(size) => {
            builder.add_instruction_bt(
                VirtualInstruction::Allocate {
                    size,
                    alignment: bc_type.alignment(),
                },
                BCTypeKind::Pointer.into()
            )?
        },
        BCTypeSize::Variable(size_expr) => {
            builder.add_instruction_bt(
                VirtualInstruction::VariableAllocate {
                    size: size_expr,
                    alignment: bc_type.alignment(),
                },
                BCTypeKind::Pointer.into()
            )?
        }
    };

    builder.symbol_table.insert(name.to_owned(), memory);
    
    allocation_side_effects(builder, var_type, name, &mut |_| Some(memory))?;

    Some(memory)
}

fn allocation_side_effects(
    builder: &mut BytecodeBuilder,
    var_type: &CXType,
    name: &str,
    memory: &mut dyn FnMut(&mut BytecodeBuilder) -> Option<ValueID>,
) -> Option<()> {
    match var_type.intrinsic_type(&builder.cx_type_map).cloned()? {
        CXTypeKind::StrongPointer { .. } => {
            let memory = memory(builder).unwrap();
            let previous_block = builder.current_block();
            builder.enter_deferred_logic();

            let true_block = builder.create_named_block(format!("free_{name}").as_str());
            let false_block = builder.create_named_block(format!("free_{name}_merge").as_str());

            let pointer_val = builder.add_instruction_bt(
                VirtualInstruction::Load { value: memory },
                BCTypeKind::Pointer.into()
            )?;
            let has_tag = builder.add_instruction_bt(
                VirtualInstruction::HasPointerTag { value: pointer_val },
                BCTypeKind::Bool.into()
            )?;
            
            builder.add_instruction(
                VirtualInstruction::Branch {
                    condition: has_tag,
                    true_block, false_block
                },
                CXType::unit()
            )?;
            
            builder.set_current_block(true_block);
            
            let stdfree = builder.add_instruction_bt(
                VirtualInstruction::FunctionReference {
                    name: "stdfree".into(),
                },
                BCType::from(BCTypeKind::Pointer)
            )?;
            let method_sig = builder.fn_map.get("stdfree").unwrap().clone();
            
            builder.add_instruction(
                VirtualInstruction::DirectCall {
                    func: stdfree,
                    args: vec![pointer_val],
                    method_sig
                },
                CXType::unit()
            )?;
            
            builder.add_instruction(
                VirtualInstruction::Jump {
                    target: false_block,
                },
                CXType::unit()
            );
            
            builder.set_current_block(false_block);
            
            builder.exit_deferred_logic();
            builder.set_current_block(previous_block);
        },
        
        CXTypeKind::Structured { fields, .. } => {
            let mut self_ref = None;
            
            for (name, field_type) in fields.iter() {
                let mut getter = |inner_builder : &mut BytecodeBuilder| {
                    if self_ref.is_none() {
                        self_ref = Some(memory(inner_builder)?);
                    }
                    
                    let as_bc_type = inner_builder.convert_cx_type(field_type)?;
                    let access = get_struct_field(inner_builder, &as_bc_type, name)?;
                    
                    inner_builder.add_instruction(
                        VirtualInstruction::StructAccess {
                            field_offset: access.offset,
                            field_index: access.index,
                            struct_type: access._type.clone(),
                            struct_: self_ref.unwrap(),
                        },
                        field_type.clone()
                    )
                };
                
                allocation_side_effects(
                    builder,
                    field_type,
                    &format!("{name}"),
                    &mut getter
                )?
            }
        },
        
        _ => {}
    };
    
    Some(())
}