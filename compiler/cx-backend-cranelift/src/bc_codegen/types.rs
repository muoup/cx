use crate::GlobalState;
use cranelift::{codegen::ir, prelude::Signature};
use cranelift_module::Module;
use cx_bytecode_data::bc_type::{BCFunctionPrototype, BCType, FloatType, IntegerType};
use cx_mir_data::LinkageType;

pub(crate) fn convert_linkage(linkage: LinkageType) -> cranelift_module::Linkage {
    match linkage {
        LinkageType::External => cranelift_module::Linkage::Import,
        LinkageType::Standard => cranelift_module::Linkage::Local,
        LinkageType::Static => cranelift_module::Linkage::Export,
        LinkageType::ODR => cranelift_module::Linkage::Local,
    }
}

pub(crate) fn get_cranelift_abi_type(val_type: &BCType) -> ir::AbiParam {
    ir::AbiParam::new(get_cranelift_type(val_type))
}

pub(crate) fn get_cranelift_int_type(int_type: &IntegerType) -> ir::Type {
    match int_type {
        IntegerType::I8 => ir::Type::int(8).expect("PANIC: Invalid integer size: 8 bits"),
        IntegerType::I16 => ir::Type::int(16).expect("PANIC: Invalid integer size: 16 bits"),
        IntegerType::I32 => ir::Type::int(32).expect("PANIC: Invalid integer size: 32 bits"),
        IntegerType::I64 => ir::Type::int(64).expect("PANIC: Invalid integer size: 64 bits"),
        IntegerType::I128 => ir::Type::int(128).expect("PANIC: Invalid integer size: 128 bits"),
    }
}

pub(crate) fn get_cranelift_float_type(float_type: &FloatType) -> ir::Type {
    match float_type {
        FloatType::F32 => ir::types::F32,
        FloatType::F64 => ir::types::F64,
    }
}

pub(crate) fn get_cranelift_type(val_type: &BCType) -> ir::Type {
    match &val_type {
        BCType::Integer(i_type) => get_cranelift_int_type(i_type),
        BCType::Float(f_type) => get_cranelift_float_type(f_type),

        BCType::Bool => ir::types::I8,


        BCType::Structured { .. } | BCType::Pointer { .. } | BCType::Array { .. } => {
            ir::Type::int(64).unwrap()
        }

        // Because of the way Cranelift codegen works, there is actually no need for
        // handling arrays, as anywhere where the type is used (i.e. in stack allocations)
        // will implicitly use the size which can be derived from the bc type.
        _ => panic!("PANIC: Unsupported type for Cranelift: {val_type:?}"),
    }
}

pub(crate) fn codegen_fn_prototype(
    global_state: &mut GlobalState,
    prototype: &BCFunctionPrototype,
) -> Option<()> {
    let mut sig = Signature::new(global_state.object_module.target_config().default_call_conv);

    if !prototype.return_type.is_unit() {
        sig.returns
            .push(get_cranelift_abi_type(&prototype.return_type));
    }

    for param in prototype.parameter_types.iter() {
        sig.params.push(get_cranelift_abi_type(param));
    }

    let linkage = convert_linkage(prototype.linkage);

    let id = global_state
        .object_module
        .declare_function(prototype.name.as_str(), linkage, &sig)
        .unwrap();

    global_state
        .function_ids
        .insert(prototype.name.to_owned(), id);
    global_state
        .function_sigs
        .insert(prototype.name.to_owned(), sig);

    Some(())
}
