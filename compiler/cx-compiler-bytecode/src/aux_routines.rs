use cx_data_bytecode::types::{BCType, BCTypeKind};

pub(crate) struct StructAccess {
    pub(crate) offset: usize,
    pub(crate) index: usize,
    pub(crate) _type: BCType,
}

pub(crate) fn get_struct_field(
    _type: &BCType,
    name: &str
) -> Option<StructAccess> {
    let BCTypeKind::Struct { fields } = &_type.kind else {
        panic!("PANIC: Expected struct type, got: {:?}", _type);
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
        
        offset += field_type.size();
    }
    
    None
}