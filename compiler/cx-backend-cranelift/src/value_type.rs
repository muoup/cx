use cranelift::codegen::ir;
use cx_data_ast::parse::ast::CXTypeMap;
use cx_data_ast::parse::value_type::{CXTypeKind, CXType};
use cx_data_bytecode::types::{BCType, BCTypeKind};

pub(crate) fn get_cranelift_abi_type(val_type: &BCType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_type(val_type: &BCType) -> ir::Type {
    match &val_type.kind {
        BCTypeKind::Signed { bytes} |
        BCTypeKind::Unsigned { bytes } if *bytes == 0 
            => ir::Type::int(8).expect("PANIC: Invalid integer size: 0 bytes"),
        
        BCTypeKind::Signed { bytes } |
        BCTypeKind::Unsigned { bytes }    => ir::Type::int(*bytes as u16 * 8)
            .expect(format!("PANIC: Invalid integer size: {} bytes", *bytes).as_str()),
        
        BCTypeKind::Float { bytes: 2 }         => ir::types::F16,
        BCTypeKind::Float { bytes: 4 }         => ir::types::F32,
        BCTypeKind::Float { bytes: 8 }         => ir::types::F64,
        BCTypeKind::Float { bytes: 16 }        => ir::types::F128,
        
        BCTypeKind::Struct { .. } |
        BCTypeKind::Pointer                    => ir::Type::int(64).unwrap(),
        
        _ => panic!("PANIC: Unsupported type for Cranelift: {:?}", val_type),
    }
}