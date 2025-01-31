use cranelift::codegen::ir;
use cranelift_module::Module;
use crate::codegen::codegen::GlobalState;
use crate::parse::val_type::ValType;

pub(crate) fn get_cranelift_abi_type(global_state: &GlobalState, val_type: &ValType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(global_state, val_type))
}

pub(crate) fn get_cranelift_type(global_state: &GlobalState, val_type: &ValType) -> ir::Type {
    match val_type {
        ValType::Integer { size, .. } => {
            match size {
                1 => ir::types::I8,
                2 => ir::types::I16,
                4 => ir::types::I32,
                8 => ir::types::I64,
                _ => panic!("Invalid integer size")
            }
        },
        ValType::Float { size } => {
            match size {
                4 => ir::types::F32,
                8 => ir::types::F64,
                _ => panic!("Invalid float size")
            }
        },
        ValType::Unit => ir::types::INVALID,
        ValType::Pointer(..) | ValType::Array(..) => {
            global_state.object_module.target_config().pointer_type()
        },
        ValType::Struct { .. } => {
            unimplemented!()
        }
    }
}