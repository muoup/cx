use cx_log::CXRawResult;
use cx_mir::{
    mir::r#type::{MIRFloatType, MIRIntegerType, MIRType, MIRTypeKind},
    type_context::MIRTypeContext,
};

use crate::environment::TypeEnvironment;

pub fn sizeof_type_size(env: &TypeEnvironment, _ty: &MIRType) -> CXRawResult<usize> {
    match &_ty.kind {
        MIRTypeKind::Integer { _type, .. } => match _type {
            MIRIntegerType::I1 | MIRIntegerType::I8 => Ok(1),
            MIRIntegerType::I16 => Ok(2),
            MIRIntegerType::I32 => Ok(4),
            MIRIntegerType::I64 => Ok(8),
            MIRIntegerType::I128 => Ok(16),
        },

        MIRTypeKind::Float { _type } => match _type {
            MIRFloatType::F32 => Ok(4),
            MIRFloatType::F64 => Ok(8),
        },

        MIRTypeKind::MemoryReference { .. } | MIRTypeKind::PointerTo { .. } => Ok(8),

        MIRTypeKind::Opaque { size } => Ok(*size),

        MIRTypeKind::Structured { fields } => {
            let mut size = 0;

            for field in fields {
                let ty = env.symbols.resolve_type_id(field.ty());
                let ty_size = sizeof_type_size(env, &ty)?;

                size = apply_padding(size, ty_size);
                size += ty_size;
            }

            Ok(apply_padding(size, size.min(8)))
        }

        MIRTypeKind::Union { variants } => {
            let size = variants
                .iter()
                .map(|variant| variant.ty())
                .map(|ty| env.symbols.resolve_type_id(ty))
                .map(|ty| sizeof_type_size(env, &ty))
                .collect::<CXRawResult<Vec<usize>>>()?
                .into_iter()
                .max()
                .unwrap_or(0);

            Ok(apply_padding(size, size.min(8))) // Assuming 8-byte alignment for unions
        }

        MIRTypeKind::Array { length, inner_type } => {
            let inner_ty = env.symbols.resolve_type_id(*inner_type);

            sizeof_type_size(env, &inner_ty).map(|s| s * *length)
        }

        MIRTypeKind::TaggedUnion { variants } => {
            let tag_size = 1; // Assuming a 4-byte tag
            let max_variant_size = variants
                .iter()
                .map(|variant| variant.ty())
                .map(|ty| env.symbols.resolve_type_id(ty))
                .map(|ty| sizeof_type_size(env, &ty))
                .collect::<CXRawResult<Vec<usize>>>()?
                .into_iter()
                .max()
                .unwrap_or(0);

            Ok(apply_padding(
                tag_size + max_variant_size,
                (tag_size + max_variant_size).min(8),
            )) // Align to tag size
        }

        _ => env.log_error_base(format!(
            "Cannot determine size of type {}",
            _ty.display_with(&env.symbols)
        )),
    }
}

fn apply_padding(size: usize, alignment: usize) -> usize {
    if alignment == 0 {
        return size; // Avoid division by zero
    }
    let padding = (alignment - (size % alignment)) % alignment;
    size + padding
}
