use crate::GlobalState;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameterABI, LMIRReturnABI, LinkageType,
};
use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::types::{
    AnyType, AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use std::sync::Mutex;

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

        _ => None,
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

        _ => None,
    }
}

pub(crate) fn bc_llvm_type<'a>(context: &'a Context, _type: &LMIRType) -> Option<AnyTypeEnum<'a>> {
    Some(match &_type.kind {
        LMIRTypeKind::Unit => context.void_type().as_any_type_enum(),
        LMIRTypeKind::Integer(_type) => match _type {
            LMIRIntegerType::I1 => context.bool_type().as_any_type_enum(),
            LMIRIntegerType::I8 => context.i8_type().as_any_type_enum(),
            LMIRIntegerType::I16 => context.i16_type().as_any_type_enum(),
            LMIRIntegerType::I32 => context.i32_type().as_any_type_enum(),
            LMIRIntegerType::I64 => context.i64_type().as_any_type_enum(),
            LMIRIntegerType::I128 => context.i128_type().as_any_type_enum(),
        },

        LMIRTypeKind::Float(_type) => match _type {
            LMIRFloatType::F32 => context.f32_type().as_any_type_enum(),
            LMIRFloatType::F64 => context.f64_type().as_any_type_enum(),
        },

        LMIRTypeKind::Array { element, size } => {
            let inner_llvm_type = bc_llvm_type(context, element)?;
            let basic_type = any_to_basic_type(inner_llvm_type)?;

            basic_type.array_type(*size as u32).as_any_type_enum()
        }
        LMIRTypeKind::Pointer { .. } => context.ptr_type(AddressSpace::from(0)).as_any_type_enum(),
        LMIRTypeKind::Vector { element, count } => {
            let element = bc_llvm_type(context, &LMIRTypeKind::Float(*element).into())?;
            match any_to_basic_type(element)? {
                BasicTypeEnum::FloatType(ty) => ty.vec_type(*count as u32).as_any_type_enum(),
                ty => panic!("Unsupported LLVM vector element type: {ty:?}"),
            }
        }

        LMIRTypeKind::Struct { name, fields } => {
            let struct_name = if name.is_empty() {
                anonymous_struct_name()
            } else {
                name.clone()
            };

            let type_s = fields
                .iter()
                .map(|(_, field_type)| {
                    let _type = bc_llvm_type(context, field_type)?;

                    any_to_basic_type(_type)
                })
                .collect::<Option<Vec<_>>>()?;

            if let Some(_type) = context.get_struct_type(struct_name.as_str()) {
                return Some(_type.as_any_type_enum());
            }

            let struct_def = context.opaque_struct_type(struct_name.as_str());
            struct_def.set_body(type_s.as_slice(), false);

            return context
                .get_struct_type(struct_name.as_str())
                .map(|s| s.as_any_type_enum());
        }

        LMIRTypeKind::Opaque { bytes } => context
            .i8_type()
            .array_type(*bytes as u32)
            .as_any_type_enum(),
    })
}

pub(crate) fn bc_llvm_signature<'a>(
    state: &GlobalState<'a>,
    signature: &LMIRFunctionSignature,
) -> Option<FunctionType<'a>> {
    let mut args = Vec::new();

    if signature.return_abi.has_indirect_return_param() {
        args.push(state.context.ptr_type(AddressSpace::from(0)).into());
    }

    for param in &signature.params {
        match &param.abi {
            LMIRParameterABI::Direct { slots } => {
                for slot in slots {
                    let bc_arg = bc_llvm_type(state.context, &slot._type)?;
                    let basic_type = any_to_basic_type(bc_arg)?;
                    let md_type = unsafe { BasicMetadataTypeEnum::new(basic_type.as_type_ref()) };
                    args.push(md_type);
                }
            }
            LMIRParameterABI::Indirect { .. } => {
                args.push(state.context.ptr_type(AddressSpace::from(0)).into());
            }
        }
    }

    let return_type = match &signature.return_abi {
        LMIRReturnABI::Void => state.context.void_type().as_any_type_enum(),
        LMIRReturnABI::Direct { slots } if slots.len() == 1 => {
            bc_llvm_type(state.context, &slots[0]._type)?
        }
        LMIRReturnABI::Direct { slots } => {
            let fields = slots
                .iter()
                .map(|slot| {
                    let field = bc_llvm_type(state.context, &slot._type)?;
                    any_to_basic_type(field)
                })
                .collect::<Option<Vec<_>>>()?;
            state
                .context
                .struct_type(fields.as_slice(), false)
                .as_any_type_enum()
        }
        LMIRReturnABI::IndirectSret { .. } => state.context.void_type().as_any_type_enum(),
    };

    Some(match return_type {
        AnyTypeEnum::IntType(int_type) => int_type.fn_type(args.as_slice(), signature.var_args),
        AnyTypeEnum::FloatType(float_type) => {
            float_type.fn_type(args.as_slice(), signature.var_args)
        }
        AnyTypeEnum::PointerType(ptr_type) => ptr_type.fn_type(args.as_slice(), signature.var_args),
        AnyTypeEnum::StructType(struct_type) => {
            struct_type.fn_type(args.as_slice(), signature.var_args)
        }
        AnyTypeEnum::VectorType(vector_type) => {
            vector_type.fn_type(args.as_slice(), signature.var_args)
        }
        AnyTypeEnum::VoidType(void_type) => void_type.fn_type(args.as_slice(), signature.var_args),

        _ty => panic!("Invalid return type, found: {_ty:?}"),
    })
}

pub(crate) fn bc_llvm_prototype<'a>(
    state: &GlobalState<'a>,
    prototype: &LMIRFunctionPrototype,
) -> Option<FunctionType<'a>> {
    bc_llvm_signature(state, prototype.signature())
}

pub(crate) fn convert_linkage(linkage: LinkageType) -> Linkage {
    match linkage {
        LinkageType::ODR => Linkage::LinkOnceODR,
        LinkageType::Static => Linkage::Internal,
        LinkageType::Standard => Linkage::External,
        LinkageType::External => Linkage::ExternalWeak,
    }
}
