use crate::GlobalState;
use crate::attributes::{attr_alignment, attr_byval, get_type_attributes};
use crate::error::{LLVMError, LLVMResult};
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameterABI, LMIRReturnABI, LinkageType,
};
use cx_target::ArchitectureConfig;
use inkwell::AddressSpace;
use inkwell::attributes::AttributeLoc;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::types::{
    AnyType, AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue};
use std::sync::Mutex;

fn anonymous_struct_name() -> String {
    static ANON_COUNTER: Mutex<usize> = Mutex::new(0);

    let mut counter = ANON_COUNTER.lock().unwrap();
    *counter += 1;

    format!("anonymous_struct_{}", *counter)
}

pub(crate) fn any_to_basic_type(any_type: AnyTypeEnum) -> LLVMResult<BasicTypeEnum> {
    match any_type {
        AnyTypeEnum::IntType(int_type) => Ok(int_type.into()),
        AnyTypeEnum::FloatType(float_type) => Ok(float_type.into()),
        AnyTypeEnum::PointerType(ptr_type) => Ok(ptr_type.into()),
        AnyTypeEnum::StructType(struct_type) => Ok(struct_type.into()),
        AnyTypeEnum::ArrayType(array_type) => Ok(array_type.into()),
        AnyTypeEnum::VectorType(vector_type) => Ok(vector_type.into()),

        any_type => Err(LLVMError::new(format!(
            "Expected a basic LLVM type, found {any_type:?}"
        ))),
    }
}

pub(crate) fn any_to_basic_val(any_value: AnyValueEnum) -> LLVMResult<BasicValueEnum> {
    match any_value {
        AnyValueEnum::IntValue(int_value) => Ok(int_value.into()),
        AnyValueEnum::FloatValue(float_value) => Ok(float_value.into()),
        AnyValueEnum::PointerValue(ptr_value) => Ok(ptr_value.into()),
        AnyValueEnum::StructValue(struct_value) => Ok(struct_value.into()),
        AnyValueEnum::ArrayValue(array_value) => Ok(array_value.into()),
        AnyValueEnum::VectorValue(vector_value) => Ok(vector_value.into()),

        any_value => Err(LLVMError::new(format!(
            "Expected a basic LLVM value, found {any_value:?}"
        ))),
    }
}

pub(crate) fn bc_llvm_type<'a>(
    context: &'a Context,
    _type: &LMIRType,
) -> LLVMResult<AnyTypeEnum<'a>> {
    Ok(match &_type.kind {
        LMIRTypeKind::Void => context.void_type().as_any_type_enum(),
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
            let element = match element {
                LMIRFloatType::F32 => context.f32_type(),
                LMIRFloatType::F64 => context.f64_type(),
            };
            element.vec_type(*count as u32).as_any_type_enum()
        }

        LMIRTypeKind::Struct { name, fields } => {
            let struct_name = if name.is_empty() {
                anonymous_struct_name()
            } else {
                name.clone()
            };

            let type_s = fields
                .iter()
                .map(|(_, field_type)| -> LLVMResult<_> {
                    let _type = bc_llvm_type(context, field_type)?;

                    any_to_basic_type(_type)
                })
                .collect::<LLVMResult<Vec<_>>>()?;

            if let Some(_type) = context.get_struct_type(struct_name.as_str()) {
                return Ok(_type.as_any_type_enum());
            }

            let struct_def = context.opaque_struct_type(struct_name.as_str());
            struct_def.set_body(type_s.as_slice(), false);

            return Ok(struct_def.as_any_type_enum());
        }

        LMIRTypeKind::Opaque { bytes }
            if *bytes == ArchitectureConfig::native().pointer_size()
                && usize::from(_type.alignment)
                    == ArchitectureConfig::native().pointer_alignment() =>
        {
            context.ptr_type(AddressSpace::from(0)).as_any_type_enum()
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
) -> LLVMResult<FunctionType<'a>> {
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
            LMIRParameterABI::Indirect { .. } | LMIRParameterABI::ByValue { .. } => {
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
                .collect::<LLVMResult<Vec<_>>>()?;
            state
                .context
                .struct_type(fields.as_slice(), false)
                .as_any_type_enum()
        }
        LMIRReturnABI::IndirectSret { .. } => state.context.void_type().as_any_type_enum(),
    };

    Ok(match return_type {
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

        ty => {
            return Err(LLVMError::new(format!(
                "Invalid LLVM function return type: {ty:?}"
            )));
        }
    })
}

pub(crate) fn bc_llvm_prototype<'a>(
    state: &GlobalState<'a>,
    prototype: &LMIRFunctionPrototype,
) -> LLVMResult<FunctionType<'a>> {
    bc_llvm_signature(state, prototype.signature())
}

pub(crate) fn apply_llvm_parameter_attributes<'a>(
    context: &'a Context,
    architecture: &ArchitectureConfig,
    function: &FunctionValue<'a>,
    signature: &LMIRFunctionSignature,
) -> LLVMResult<()> {
    let mut index = usize::from(signature.has_indirect_return_param());

    for parameter in &signature.params {
        match &parameter.abi {
            LMIRParameterABI::Direct { slots } => {
                for slot in slots {
                    for attribute in get_type_attributes(context, &slot._type) {
                        function.add_attribute(AttributeLoc::Param(index as u32), attribute);
                    }
                    index += 1;
                }
            }
            LMIRParameterABI::Indirect { .. } => {
                let pointer = LMIRType::default_pointer(architecture);
                for attribute in get_type_attributes(context, &pointer) {
                    function.add_attribute(AttributeLoc::Param(index as u32), attribute);
                }
                index += 1;
            }
            LMIRParameterABI::ByValue { alignment } => {
                let pointer = LMIRType::default_pointer(architecture);
                for attribute in get_type_attributes(context, &pointer) {
                    function.add_attribute(AttributeLoc::Param(index as u32), attribute);
                }
                let pointee = bc_llvm_type(context, &parameter._type)?;
                function.add_attribute(
                    AttributeLoc::Param(index as u32),
                    attr_byval(context, pointee),
                );
                function.add_attribute(
                    AttributeLoc::Param(index as u32),
                    attr_alignment(context, *alignment),
                );
                index += 1;
            }
        }
    }

    Ok(())
}

pub(crate) fn convert_linkage(linkage: LinkageType) -> Linkage {
    match linkage {
        LinkageType::ODR => Linkage::LinkOnceODR,
        LinkageType::Static => Linkage::Internal,
        LinkageType::Standard => Linkage::External,
        LinkageType::External => Linkage::ExternalWeak,
    }
}
