use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::{ValueID, VirtualInstruction};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_util::bytecode_error_log;
use crate::builder::{BytecodeBuilder, DeclarationLifetime};
use crate::BytecodeResult;
use crate::deconstructor::deconstruct_variable;

pub(crate) struct CXStructAccess {
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
) -> Option<CXStructAccess> {
    let BCTypeKind::Struct { fields, .. } = &_type.kind else {
        bytecode_error_log!(builder, "PANIC: Expected struct type on access {name}, got: {:?}", _type);
    };
    
    let mut offset = 0;
    
    for (index, (field_name, field_type)) in fields.iter().enumerate() {
        offset = align_offset(offset, field_type.alignment() as usize);
        
        if field_name == name {
            return Some(CXStructAccess {
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
) -> Option<CXStructAccess> {
    let BCTypeKind::Struct { fields, .. } = &_type.kind else {
        bytecode_error_log!(builder, "PANIC: Expected struct type on access by index {index}, got: {:?}", _type);
    };
    
    if index >= fields.len() {
        bytecode_error_log!(builder, "PANIC: Index out of bounds for struct access by index {index}");
    }
    
    let mut offset = 0;
    let mut field_iter = fields.iter().peekable();
    
    for _ in 0..index {
        let (_, field_type) = field_iter.next()?;

        offset += field_type.fixed_size();
        
        let alignment = field_iter.peek()
            .map(|(_, next_type)| next_type.alignment() as usize)
            .expect("get_cx_struct_field_by_index: Out of bounds access");
        offset = align_offset(offset, alignment);
    }
    
    let field_type = fields[index].1.clone();
    
    Some(CXStructAccess {
        offset, index,
        _type: field_type
    })
}

fn variable_requires_nulling(
    builder: &BytecodeBuilder,
    cx_type: &CXType
) -> Option<bool> {
    match cx_type.kind {
        CXTypeKind::StrongPointer { .. } => Some(true),
        
        _ => Some(builder.type_check_data.destructor_exists(cx_type))
    }
}

pub(crate) fn allocate_variable(
    name: &str,
    builder: &mut BytecodeBuilder,
    var_type: &CXType,
) -> Option<ValueID> {
    let bc_type = builder.convert_cx_type(var_type)?;
    let memory = builder.add_instruction(
        VirtualInstruction::Allocate {
            _type: bc_type.clone(),
            alignment: bc_type.alignment(),
        },
        BCType::default_pointer()
    )?;

    builder.insert_symbol(name.to_owned(), memory);
    builder.insert_declaration(
        DeclarationLifetime {
            value_id: memory,
            _type: var_type.clone()
        }
    );
    
    if variable_requires_nulling(builder, var_type)? {
        builder.add_instruction(
            VirtualInstruction::ZeroMemory {
                memory,
                _type: bc_type.clone(),
            },
            BCType::unit()
        )?;
    }

    Some(memory)
}

pub(crate) fn deconstruct_scope(
    builder: &mut BytecodeBuilder,
    scope: &[DeclarationLifetime]
) -> BytecodeResult<()> {
    for lifetime in scope {
        deconstruct_variable(builder, lifetime.value_id.clone(), &lifetime._type, true)?;
    }
    
    Some(())
}