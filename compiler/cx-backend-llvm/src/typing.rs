use crate::GlobalState;
use cx_mir_data::types::{MIRType, MIRTypeKind};
use cx_mir_data::{MIRFunctionPrototype, LinkageType};
use inkwell::types::{AnyType, AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use inkwell::AddressSpace;
use std::sync::Mutex;
use inkwell::context::Context;
use inkwell::module::Linkage;

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

pub(crate) fn bc_llvm_type<'a>(context: &'a Context, _type: &MIRType) -> Option<AnyTypeEnum<'a>> {
    Some(
        match &_type.kind {
            MIRTypeKind::Unit => context.void_type().as_any_type_enum(),
            MIRTypeKind::Signed { bytes, .. } |
            MIRTypeKind::Unsigned { bytes, .. } => {
                match *bytes {
                    1 => context.i8_type().as_any_type_enum(),
                    2 => context.i16_type().as_any_type_enum(),
                    4 => context.i32_type().as_any_type_enum(),
                    8 => context.i64_type().as_any_type_enum(),

                    _ => panic!("Invalid integer size")
                }
            },
            MIRTypeKind::Bool => context.bool_type().as_any_type_enum(),
            MIRTypeKind::Float { bytes: 4 } => context.f32_type().as_any_type_enum(),
            MIRTypeKind::Float { bytes: 8 } => context.f64_type().as_any_type_enum(),

            MIRTypeKind::Array { element, size } => {
                let inner_llvm_type = bc_llvm_type(context, element)?;
                let basic_type = any_to_basic_type(inner_llvm_type)?;

                basic_type.array_type(*size as u32).as_any_type_enum()
            },
            MIRTypeKind::Pointer { .. } => context.ptr_type(AddressSpace::from(0)).as_any_type_enum(),

            MIRTypeKind::Struct { name, fields } => {
                if let Some(_type) = context.get_struct_type(name.as_str()) {
                    return Some(_type.as_any_type_enum());
                }

                let _types = fields
                    .iter()
                    .map(|(_, field_type)| {
                        let type_ = bc_llvm_type(context, field_type)?;

                        any_to_basic_type(type_)
                    })
                    .collect::<Option<Vec<_>>>()?;

                let struct_name = if name.is_empty() {
                    anonymous_struct_name()
                } else {
                    name.clone()
                };
                
                let struct_def = context.opaque_struct_type(struct_name.as_str());
                struct_def.set_body(_types.as_slice(), false);

                return context.get_struct_type(struct_name.as_str())
                    .map(|s| s.as_any_type_enum());
            }

            MIRTypeKind::Union { .. } => {
                let _type_size = _type.fixed_size();
                let array_type = context.i8_type().array_type(_type_size as u32);

                array_type.as_any_type_enum()
            },

            _ => panic!("Invalid type: {_type:?}")
        }
    )
}

pub(crate) fn bc_llvm_prototype<'a>(
    state: &GlobalState<'a>,
    prototype: &MIRFunctionPrototype
) -> Option<FunctionType<'a>> {
    let return_type = match bc_llvm_type(state.context, &prototype.return_type)? {
        AnyTypeEnum::StructType(_) =>
            state.context.ptr_type(AddressSpace::from(0))
                .as_any_type_enum(),
        
        any_type => any_type
    };
    
    let args = prototype
        .params
        .iter()
        .map(|arg| {
            let bc_arg = bc_llvm_type(state.context, &arg._type)?;
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

pub(crate) fn convert_linkage(linkage: LinkageType) -> Linkage {
    match linkage {
        LinkageType::ODR => Linkage::LinkOnceODR,
        LinkageType::Static => Linkage::Internal,
        LinkageType::Standard => Linkage::External,
        LinkageType::External => Linkage::ExternalWeak
    }
}