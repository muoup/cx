use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_data_typechecker::cx_types::{CXType, CXTypeKind};
use cx_data_mir::{MIRValue, VirtualInstruction};
use cx_data_mir::types::{MIRType, MIRTypeKind};
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_util::bytecode_error_log;
use cx_util::mangling::{mangle_destructor};
use crate::builder::{MIRBuilder, DeclarationLifetime};
use crate::BytecodeResult;
use crate::deconstructor::deconstruct_variable;
use crate::instruction_gen::generate_instruction;

pub(crate) struct CXStructAccess {
    pub(crate) offset: usize,
    pub(crate) index: usize,
    pub(crate) _type: MIRType,
}

fn align_offset(current_offset: usize, alignment: usize) -> usize {
    if current_offset % alignment != 0 {
        current_offset + (alignment - (current_offset % alignment))
    } else {
        current_offset
    }
}

pub(crate) fn try_access_field(
    builder: &mut MIRBuilder,
    ltype: &MIRType,
    left_id: MIRValue,
    field_name: &str,
) -> Option<MIRValue> {
    match ltype.kind {
        MIRTypeKind::Struct { .. } => {
            let struct_access = get_struct_field(
                builder, &ltype, field_name
            ).unwrap_or_else(|| {
                panic!("PANIC: Attempting to access non-existent field {field_name} in struct {ltype:?}");
            });

            builder.add_instruction(
                VirtualInstruction::StructAccess {
                    struct_: left_id,
                    struct_type: ltype.clone(),
                    field_offset: struct_access.offset,
                    field_index: struct_access.index,
                },
                struct_access._type
            )
        },

        MIRTypeKind::Union { .. } => Some(left_id),

        _ => unreachable!("generate_instruction: Expected structured type for access, found {ltype}")
    }
}

pub(crate) fn get_struct_field(
    builder: &MIRBuilder,
    _type: &MIRType,
    name: &str
) -> Option<CXStructAccess> {
    let MIRTypeKind::Struct { fields, .. } = &_type.kind else {
        bytecode_error_log!(builder, "PANIC: Expected struct type on access {name}, got: {:?}", _type);
    };
    
    let mut offset = 0;
    
    for (index, (field_name, field_type)) in fields.iter().enumerate() {
        offset = align_offset(offset, field_type.alignment() as usize);
        
        if field_name.as_str() == name {
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
    builder: &MIRBuilder,
    _type: &MIRType,
    index: usize
) -> Option<CXStructAccess> {
    let MIRTypeKind::Struct { fields, .. } = &_type.kind else {
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
    builder: &MIRBuilder,
    cx_type: &CXType
) -> bool {
    match cx_type.kind {
        CXTypeKind::StrongPointer { .. } => true,

        _ => builder.get_destructor(cx_type).is_some()
    }
}

pub(crate) fn allocate_variable(
    name: &str,
    builder: &mut MIRBuilder,
    var_type: &CXType,
) -> Option<MIRValue> {
    let bc_type = builder.convert_cx_type(var_type)?;
    let memory = builder.add_instruction(
        VirtualInstruction::Allocate {
            _type: bc_type.clone(),
            alignment: bc_type.alignment(),
        },
        MIRType::default_pointer()
    )?;

    builder.insert_symbol(name.to_owned(), memory.clone());
    builder.insert_declaration(
        DeclarationLifetime {
            value_id: memory.clone(),
            _type: var_type.clone()
        }
    );
    
    if variable_requires_nulling(builder, var_type) {
        builder.add_instruction(
            VirtualInstruction::ZeroMemory {
                memory: memory.clone(),
                _type: bc_type.clone(),
            },
            MIRType::unit()
        )?;
    }

    Some(memory)
}

pub(crate) fn assign_value(builder: &mut BytecodeBuilder, target: MIRValue, source: MIRValue, _type: &CXType, additional_op: Option<&CXBinOp>)
    -> BytecodeResult<MIRValue> {

    if additional_op.is_some() { todo!("compound assignment") }

    let inner_type = builder.convert_fixed_cx_type(&_type)?;

    builder.add_instruction(
        VirtualInstruction::Store {
            memory: target,
            value: source,
            type_: inner_type,
        },
        BCType::unit()
    )
}