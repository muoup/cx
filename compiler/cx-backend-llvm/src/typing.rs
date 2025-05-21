use inkwell::AddressSpace;
use crate::GlobalState;
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};
use inkwell::types::{AnyType, AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValueEnum, BasicValue, BasicValueEnum};
use cx_data_ast::parse::ast::CXFunctionPrototype;
use cx_data_bytecode::builder::BytecodeFunctionPrototype;

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

pub(crate) fn create_fn_proto<'a>(return_type: AnyTypeEnum<'a>, args: &[AnyTypeEnum<'a>], var_args: bool) -> Option<FunctionType<'a>> {
    let args = args
        .iter()
        .map(|arg| {
            let basic_type = any_to_basic_type(arg.clone())?;
            
            unsafe { Some(BasicMetadataTypeEnum::new(basic_type.as_type_ref())) }
        })
        .collect::<Option<Vec<_>>>()?;
    
    Some(
        match return_type {
            AnyTypeEnum::IntType(int_type) => int_type.fn_type(args.as_slice(), var_args),
            AnyTypeEnum::FloatType(float_type) => float_type.fn_type(args.as_slice(), var_args),
            AnyTypeEnum::PointerType(ptr_type) => ptr_type.fn_type(args.as_slice(), var_args),
            AnyTypeEnum::StructType(struct_type) => struct_type.fn_type(args.as_slice(), var_args),
            AnyTypeEnum::VoidType(void_type) => void_type.fn_type(args.as_slice(), var_args),
            
            _ => panic!("Invalid return type, found: {:?}", return_type)
        }
    )
}

pub(crate) fn cx_llvm_type<'a>(state: &GlobalState<'a>, _type: &CXValType) -> Option<AnyTypeEnum<'a>> {
    Some(
        match &_type.internal_type {
            CXTypeUnion::Unit => state.context.void_type().as_any_type_enum(),
            CXTypeUnion::Integer { bytes, .. } => {
                match *bytes {
                    1 => state.context.i8_type().as_any_type_enum(),
                    2 => state.context.i16_type().as_any_type_enum(),
                    4 => state.context.i32_type().as_any_type_enum(),
                    8 => state.context.i64_type().as_any_type_enum(),

                    _ => panic!("Invalid integer size")
                }
            },
            CXTypeUnion::Float { bytes: 4 } => state.context.f32_type().as_any_type_enum(),
            CXTypeUnion::Float { bytes: 8 } => state.context.f64_type().as_any_type_enum(),
            CXTypeUnion::PointerTo(_)
                => state.context.ptr_type(AddressSpace::from(0)).as_any_type_enum(),

            CXTypeUnion::Identifier(ident) => {
                let ident_type = state.type_map.get(ident.as_str())?;
                let name = ident.as_str();
                
                match ident_type.internal_type {
                    CXTypeUnion::Structured { .. } =>
                        state.module.get_struct_type(name)
                            .unwrap_or(state.context.opaque_struct_type(name))
                            .as_any_type_enum(),

                    _ => return cx_llvm_type(state, ident_type)
                }
            },

            _ => panic!("Invalid type: {:?}", _type)
        }
    )
}

pub(crate) fn cx_llvm_prototype<'a>(
    state: &GlobalState<'a>,
    prototype: &CXFunctionPrototype
) -> Option<FunctionType<'a>> {
    let return_type = cx_llvm_type(state, &prototype.return_type)?;
    let arg_types = prototype
        .parameters
        .iter()
        .map(|arg| cx_llvm_type(state, &arg.type_))
        .collect::<Option<Vec<_>>>()?;

    create_fn_proto(
        return_type,
        &arg_types,
        prototype.var_args
    )
}

pub(crate) fn bc_llvm_prototype<'a>(
    state: &GlobalState<'a>,
    prototype: &BytecodeFunctionPrototype
) -> Option<FunctionType<'a>> {
    let return_type = cx_llvm_type(state, &prototype.return_type)?;
    let arg_types = prototype
        .args
        .iter()
        .map(|arg| cx_llvm_type(state, &arg.type_))
        .collect::<Option<Vec<_>>>()?;

    create_fn_proto(
        return_type,
        &arg_types,
        prototype.var_args
    )
}