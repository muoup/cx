use std::sync::Mutex;
use inkwell::AddressSpace;
use crate::GlobalState;
use inkwell::types::{AnyType, AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use cx_data_bytecode::BCFunctionPrototype;
use cx_data_bytecode::types::{BCType, BCTypeKind};

fn anonymous_struct_name() -> String {
    static ANON_COUNTER: Mutex<usize> = Mutex::new(0);

    let mut counter = ANON_COUNTER.lock().unwrap();
    *counter += 1;

    format!("anonymous_struct_{}", *counter)
}

pub(crate) fn any_to_basic_type<'a>(any_type: AnyTypeEnum) -> Option<BasicTypeEnum> {
    match any_type {
        AnyTypeEnum::IntType(int_type) => Some(int_type.into()),
        AnyTypeEnum::FloatType(float_type) => Some(float_type.into()),
        AnyTypeEnum::PointerType(ptr_type) => Some(ptr_type.into()),
        AnyTypeEnum::StructType(struct_type) => Some(struct_type.into()),
        AnyTypeEnum::ArrayType(array_type) => Some(array_type.into()),
        AnyTypeEnum::VectorType(vector_type) => Some(vector_type.into()),

        _ => None
    }
}

pub(crate) fn any_to_basic_val(any_value: AnyValueEnum) -> Option<BasicValueEnum> {
    match any_value {
        AnyValueEnum::IntValue(int_value) => Some(int_value.into()),
        AnyValueEnum::FloatValue(float_value) => Some(float_value.into()),
        AnyValueEnum::PointerValue(ptr_value) => Some(ptr_value.into()),
        AnyValueEnum::StructValue(struct_value) => Some(struct_value.into()),
        AnyValueEnum::ArrayValue(array_value) => Some(array_value.into()),
        AnyValueEnum::VectorValue(vector_value) => Some(vector_value.into()),

        _ => None
    }
}

pub(crate) fn bc_llvm_type<'a>(state: &GlobalState<'a>, _type: &BCType) -> Option<AnyTypeEnum<'a>> {
    Some(
        match &_type.kind {
            BCTypeKind::Unit => state.context.void_type().as_any_type_enum(),
            BCTypeKind::Signed { bytes, .. } |
            BCTypeKind::Unsigned { bytes, .. } => {
                match *bytes {
                    1 => state.context.i8_type().as_any_type_enum(),
                    2 => state.context.i16_type().as_any_type_enum(),
                    4 => state.context.i32_type().as_any_type_enum(),
                    8 => state.context.i64_type().as_any_type_enum(),

                    _ => panic!("Invalid integer size")
                }
            },
            BCTypeKind::Bool => state.context.bool_type().as_any_type_enum(),
            BCTypeKind::Float { bytes: 4 } => state.context.f32_type().as_any_type_enum(),
            BCTypeKind::Float { bytes: 8 } => state.context.f64_type().as_any_type_enum(),

            BCTypeKind::Array { element, size } => {
                let inner_llvm_type = bc_llvm_type(state, element)?;
                let basic_type = any_to_basic_type(inner_llvm_type)?;

                basic_type.array_type(*size as u32).as_any_type_enum()
            },
            BCTypeKind::Pointer { .. } => state.context.ptr_type(AddressSpace::from(0)).as_any_type_enum(),

            BCTypeKind::Struct { name, fields } => {
                if let Some(_type) = state.context.get_struct_type(name.as_str()) {
                    return Some(_type.as_any_type_enum());
                }

                let _types = fields
                    .iter()
                    .map(|(_, field_type)| {
                        let type_ = bc_llvm_type(state, field_type)?;

                        any_to_basic_type(type_)
                    })
                    .collect::<Option<Vec<_>>>()?;

                let struct_type = state.context.struct_type(
                    _types.as_slice(),
                    false
                );

                let struct_name = if name.is_empty() {
                    anonymous_struct_name()
                } else {
                    name.clone()
                };
                
                let struct_def = state.context.opaque_struct_type(struct_name.as_str());
                struct_def.set_body(_types.as_slice(), false);

                return Some(struct_type.as_any_type_enum());
            }
            BCTypeKind::Union { name, .. } =>
                state.context.get_struct_type(name.as_str())
                    .unwrap_or_else(|| state.context.opaque_struct_type(name.as_str()))
                    .as_any_type_enum(),

            _ => panic!("Invalid type: {_type:?}")
        }
    )
}

pub(crate) fn bc_llvm_prototype<'a>(
    state: &GlobalState<'a>,
    prototype: &BCFunctionPrototype
) -> Option<FunctionType<'a>> {
    let return_type = match bc_llvm_type(state, &prototype.return_type)? {
        AnyTypeEnum::StructType(struct_type) =>
            state.context.ptr_type(AddressSpace::from(0))
                .as_any_type_enum(),
        
        any_type => any_type
    };
    
    let args = prototype
        .params
        .iter()
        .map(|arg| {
            let bc_arg = bc_llvm_type(state, &arg._type)?;
            let basic_type = any_to_basic_type(bc_arg)?;
            
            let md_type = 
                unsafe { BasicMetadataTypeEnum::new(basic_type.as_type_ref()) };
            
            Some(md_type)
        })
        .collect::<Option<Vec<_>>>()?;
    
    Some(
        match return_type {
            AnyTypeEnum::IntType(int_type) => int_type.fn_type(args.as_slice(), prototype.var_args),
            AnyTypeEnum::FloatType(float_type) => float_type.fn_type(args.as_slice(), prototype.var_args),
            AnyTypeEnum::PointerType(ptr_type) => ptr_type.fn_type(args.as_slice(), prototype.var_args),
            AnyTypeEnum::StructType(struct_type) => struct_type.fn_type(args.as_slice(), prototype.var_args),
            AnyTypeEnum::VoidType(void_type) => void_type.fn_type(args.as_slice(), prototype.var_args),

            _ => panic!("Invalid return type, found: {return_type:?}")
        }
    )
}