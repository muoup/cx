use crate::GlobalState;
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};
use inkwell::types::{AnyType, AnyTypeEnum, BasicTypeEnum};
use inkwell::AddressSpace;

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

pub(crate) fn cx_llvm_type<'a>(state: &GlobalState<'a>, _type: &CXValType) -> Option<AnyTypeEnum<'a>> {
    Some(
        match _type.intrinsic_type(state.type_map)? {
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

                match ident_type.internal_type {
                    CXTypeUnion::Structured { .. } =>
                        state.context.opaque_struct_type(ident.as_str()).as_any_type_enum(),

                    _ => return cx_llvm_type(state, ident_type)
                }
            },

            _ => return None
        }
    )
}