use cx_bytecode_data::{
    BCInstructionKind, BCValue, types::{BCType, BCTypeKind}
};
use cx_util::CXResult;

use crate::builder::BCBuilder;

pub fn tagged_union_tag_addr(builder: &mut BCBuilder, value: BCValue, _type: BCType) -> CXResult<BCValue> {
    let BCTypeKind::Struct { fields, .. } = &_type.kind else {
        unreachable!()
    };
    
    let tag_offset = fields[0].1.size();
    
    builder.add_new_instruction(
        BCInstructionKind::StructAccess { 
            struct_: value,
            struct_type: _type.clone(),
            field_index: 1,
            field_offset: tag_offset,
        },
        BCType::default_pointer(),
        true
    )
}
