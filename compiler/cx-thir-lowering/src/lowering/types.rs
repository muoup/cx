use cx_mir::{MIRFloatType, MIRIntType, MIRType, MIRTypeID, MIRTypeKind};
use cx_thir::{
    thir::r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeID, THIRTypeKind},
    type_context::THIRTypeContext,
};

use crate::{MIRBuilder, lowering::calls::lower_field};

pub fn lower_type(builder: &mut MIRBuilder, ty: &THIRType) -> MIRTypeID {
    if let Some(id) = builder.registry().type_id(ty) {
        return lower_type_id(builder, id);
    }

    let kind = lower_type_kind(builder, &ty.kind);
    let debug_name = builder.registry().type_debug_name(ty);
    let id = builder.types_mut().intern(MIRType {
        kind,
        minimum_alignment: ty.attributes.minimum_alignment,
    });
    if builder.types().debug_name(id).is_none()
        && let Some(debug_name) = debug_name
    {
        builder.types_mut().set_debug_name(id, debug_name);
    }
    id
}

pub fn lower_type_id(builder: &mut MIRBuilder, id: THIRTypeID) -> MIRTypeID {
    let mir_id = MIRTypeID::new(id.index());
    if builder.types().definition(mir_id).is_some() || builder.lowering_types.contains(&id)
    {
        return mir_id;
    }

    builder.lowering_types.insert(id);
    let Some(ty) = builder.registry().try_resolve_type_id(id).cloned() else {
        assert!(
            id.0 < builder.registry().type_id_bound(),
            "THIR type {id} is outside its registry"
        );
        builder
            .types_mut()
            .define(mir_id, MIRType::new(MIRTypeKind::Undefined))
            .expect("reserved THIR type ID must have one MIR definition");
        builder.lowering_types.remove(&id);
        return mir_id;
    };
    let debug_name = builder.registry().type_debug_name(&ty);
    let definition = MIRType {
        kind: lower_type_kind(builder, &ty.kind),
        minimum_alignment: ty.attributes.minimum_alignment,
    };
    builder.types_mut().define(mir_id, definition)
        .expect("THIR type ID must have one MIR definition");
    if let Some(debug_name) = debug_name {
        builder.types_mut().set_debug_name(mir_id, debug_name);
    }
    builder.lowering_types.remove(&id);
    mir_id
}

pub(crate) fn lower_type_kind(builder: &mut MIRBuilder, kind: &THIRTypeKind) -> MIRTypeKind {
    match kind {
        THIRTypeKind::Void => MIRTypeKind::Void,
        THIRTypeKind::Integer { _type, signed } => MIRTypeKind::Integer {
            ty: lower_int_type(*_type),
            signed: *signed,
        },
        THIRTypeKind::Float { _type } => MIRTypeKind::Float {
            ty: match _type {
                cx_thir::thir::r#type::THIRFloatType::F32 => cx_mir::MIRFloatType::F32,
                cx_thir::thir::r#type::THIRFloatType::F64 => cx_mir::MIRFloatType::F64,
            },
        },
        THIRTypeKind::Structured { fields } => MIRTypeKind::Structured {
            fields: fields
                .iter()
                .map(|field| lower_field(builder, field))
                .collect(),
        },
        THIRTypeKind::Union { variants } => MIRTypeKind::Union {
            variants: variants
                .iter()
                .map(|field| lower_field(builder, field))
                .collect(),
        },
        THIRTypeKind::TaggedUnion { variants } => MIRTypeKind::TaggedUnion {
            variants: variants
                .iter()
                .map(|field| lower_field(builder, field))
                .collect(),
        },
        THIRTypeKind::PointerTo { inner_type } => MIRTypeKind::PointerTo {
            inner: lower_type_id(builder, *inner_type),
        },
        THIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => MIRTypeKind::MemoryReference {
            inner: lower_type_id(builder, *inner_type),
            bitfield: bitfield.as_ref().map(|bitfield| cx_mir::MIRBitfieldAccess {
                storage_type: lower_type_id(builder, bitfield.storage_type),
                bit_offset: bitfield.bit_offset,
                bit_width: bitfield.bit_width,
                signed: bitfield.signed,
            }),
        },
        THIRTypeKind::Array { length, inner_type } => MIRTypeKind::Array {
            length: *length,
            inner: lower_type_id(builder, *inner_type),
        },
        THIRTypeKind::Function { signature } => MIRTypeKind::Function {
            signature: cx_mir::MIRFunctionType {
                params: signature
                    .params
                    .iter()
                    .map(|param| lower_type(builder, &param._type))
                    .collect(),
                return_type: lower_type(builder, &signature.return_type),
                variadic: signature.var_args,
            },
        },
        THIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque {
            size: *size,
            alignment: *alignment,
        },
        THIRTypeKind::Undefined => MIRTypeKind::Undefined,
        THIRTypeKind::Str => MIRTypeKind::Str,
    }
}

pub(crate) fn lower_int_type(ty: THIRIntType) -> MIRIntType {
    match ty {
        THIRIntType::I1 => MIRIntType::I1,
        THIRIntType::I8 => MIRIntType::I8,
        THIRIntType::I16 => MIRIntType::I16,
        THIRIntType::I32 => MIRIntType::I32,
        THIRIntType::I64 => MIRIntType::I64,
        THIRIntType::I128 => MIRIntType::I128,
    }
}

pub(crate) fn lower_float_type(ty: THIRFloatType) -> MIRFloatType {
    match ty {
        THIRFloatType::F32 => MIRFloatType::F32,
        THIRFloatType::F64 => MIRFloatType::F64,
    }
}
