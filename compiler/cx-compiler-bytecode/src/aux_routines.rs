use cx_data_ast::parse::ast::CXExpr;
use cx_data_bytecode::types::{BCType, BCTypeKind};
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

pub(crate) fn get_union_field(
    builder: &BytecodeBuilder,
    _type: &BCType,
    name: &str
) -> Option<BCType> {
    let BCTypeKind::Union { fields, .. } = &_type.kind else {
        bytecode_error_log!(builder, "PANIC: Expected union type on access {name}, got: {:?}", _type);
    };
    
    fields.iter()
        .find(|(field_name, _)| field_name == name)
        .map(|(_, field_type)| field_type.clone())
        .or_else(|| {
            bytecode_error_log!(builder, "PANIC: Invalid union accessor {name} for type {:?}", _type);
        })
}