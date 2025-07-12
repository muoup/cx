use cx_data_ast::parse::ast::CXTypeMap;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::{ElementID, ValueID, VirtualInstruction};
use cx_data_bytecode::types::{BCType, BCTypeKind, BCTypeSize};
use cx_util::bytecode_error_log;
use crate::builder::BytecodeBuilder;
use crate::deconstructor::deconstruct_variable;

pub(crate) struct StructAccess {
    pub(crate) offset: usize,
    pub(crate) index: usize,
    pub(crate) _type: BCType,
}

fn align_offset(current_offset: usize, alignment: usize) -> usize {
    if current_offset % alignment != 0 {
        current_offset + (alignment - (current_offset % alignment))
    } else {
        current_offset
    }
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
        offset = align_offset(offset, field_type.alignment() as usize);
        
        if field_name == name {
            return Some(StructAccess {
                offset, index,
                _type: field_type.clone()
            });
        }
        
        offset += field_type.fixed_size();
    }
    
    None
}

pub(crate) fn get_cx_struct_field_by_index(
    builder: &BytecodeBuilder,
    _type: &BCType,
    index: usize
) -> Option<StructAccess> {
    let BCTypeKind::Struct { fields, .. } = &_type.kind else {
        bytecode_error_log!(builder, "PANIC: Expected struct type on access by index {index}, got: {:?}", _type);
    };
    
    if index >= fields.len() {
        bytecode_error_log!(builder, "PANIC: Index out of bounds for struct access by index {index}");
    }
    
    let mut offset = 0;
    
    for (i, (_, t)) in fields.iter().enumerate() {
        offset = align_offset(offset, t.alignment() as usize) + t.fixed_size();
    }
    
    let field_type = fields[index].1.clone();
    offset -= field_type.fixed_size();
    
    Some(StructAccess {
        offset, index,
        _type: field_type
    })
}

fn variable_requires_nulling(
    type_map: &CXTypeMap,
    cx_type: &CXType
) -> Option<bool> {
    match cx_type.intrinsic_type_kind(type_map)? {
        CXTypeKind::StrongPointer { .. } => Some(true),
        
        _ => Some(cx_type.has_destructor(type_map)),
    }
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
                BCType::default_pointer()
            )?
        },
        BCTypeSize::Variable(size_expr) => {
            builder.add_instruction_bt(
                VirtualInstruction::VariableAllocate {
                    size: size_expr,
                    alignment: bc_type.alignment(),
                },
                BCType::default_pointer()
            )?
        }
    };

    builder.symbol_table.insert(name.to_owned(), memory);
    
    if builder.function_defers() {
        let current_block = builder.current_block();
        
        builder.enter_deferred_logic();
        deconstruct_variable(builder, memory, var_type, true)?;
        builder.exit_deferred_logic();
        
        builder.set_current_block(current_block);
    }
    
    if variable_requires_nulling(&builder.cx_type_map, var_type)? {
        builder.add_instruction_bt(
            VirtualInstruction::ZeroMemory {
                memory,
                _type: bc_type.clone(),
            },
            BCType::unit()
        )?;
    }

    Some(memory)
}