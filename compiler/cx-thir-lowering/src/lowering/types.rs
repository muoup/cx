use cx_log::CXResult;
use cx_mir::{
    MIRBitfieldAccess, MIRFloatType, MIRIntType, MIRType, MIRTypeID, MIRTypeKind,
    ty::interface::MTRegistry,
};
use cx_thir::{
    thir::r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeID, THIRTypeKind},
    type_context::THIRTypeContext,
};

use crate::{
    MIRBuilder,
    lowering::{calls::lower_field, comptime::evaluate_integer},
};

pub fn lower_type(builder: &mut MIRBuilder, ty: &THIRType) -> CXResult<MIRTypeID> {
    if let Some(id) = builder.registry().type_id(ty) {
        return lower_type_id(builder, id);
    }

    let kind = lower_type_kind(builder, &ty.kind)?;
    let debug_name = builder.registry().type_debug_name(ty);
    let id = builder.types_mut().intern(MIRType::new(kind, None));
    if builder.types().debug_name(id).is_none()
        && let Some(debug_name) = debug_name
    {
        builder.types_mut().set_debug_name(id, debug_name);
    }
    Ok(id)
}

pub fn lower_type_id(builder: &mut MIRBuilder, id: THIRTypeID) -> CXResult<MIRTypeID> {
    let mir_id = MIRTypeID::new(id.index());

    if builder.types().definition(mir_id).is_some() || builder.types().is_lowering_type(&id) {
        return Ok(mir_id);
    }

    builder.types_mut().insert_lowering_type(id);

    let result = (|| {
        let Some(ty) = builder.registry().try_resolve_type_id(id).cloned() else {
            assert!(
                id.0 < builder.registry().type_id_bound(),
                "THIR type {id} is outside its registry"
            );
            builder
                .types_mut()
                .define(mir_id, MIRType::undefined())
                .expect("reserved THIR type ID must have one MIR definition");
            return Ok(mir_id);
        };
        let debug_name = builder.registry().type_debug_name(&ty);
        let definition = MIRType::new(lower_type_kind(builder, &ty.kind)?, None);
        builder
            .types_mut()
            .define(mir_id, definition)
            .expect("THIR type ID must have one MIR definition");
        if let Some(debug_name) = debug_name {
            builder.types_mut().set_debug_name(mir_id, debug_name);
        }
        Ok(mir_id)
    })();

    builder.types_mut().remove_lowering_type(&id);
    result
}

pub(crate) fn lower_type_kind(
    builder: &mut MIRBuilder,
    kind: &THIRTypeKind,
) -> CXResult<MIRTypeKind> {
    Ok(match kind {
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
                .collect::<CXResult<Vec<_>>>()?,
        },
        THIRTypeKind::Union { variants } => MIRTypeKind::Union {
            variants: variants
                .iter()
                .map(|field| lower_field(builder, field))
                .collect::<CXResult<Vec<_>>>()?,
        },
        THIRTypeKind::TaggedUnion { variants } => MIRTypeKind::TaggedUnion {
            variants: variants
                .iter()
                .map(|field| lower_field(builder, field))
                .collect::<CXResult<Vec<_>>>()?,
        },
        THIRTypeKind::PointerTo { inner_type } => MIRTypeKind::PointerTo {
            inner: lower_type_id(builder, *inner_type)?,
        },
        THIRTypeKind::MemoryReference {
            inner_type,
            bitfield,
        } => MIRTypeKind::MemoryReference {
            inner: lower_type_id(builder, *inner_type)?,
            bitfield: bitfield.as_ref().map(|bitfield| MIRBitfieldAccess {
                bit_offset: bitfield.bit_offset,
                bit_width: bitfield.bit_width,
                signed: bitfield.signed,
            }),
        },
        THIRTypeKind::Array { length, inner_type } => MIRTypeKind::Array {
            length: evaluate_integer(builder, length, "array length")?,
            inner: lower_type_id(builder, *inner_type)?,
        },
        THIRTypeKind::Function { signature } => MIRTypeKind::Function {
            signature: cx_mir::MIRFunctionType {
                params: signature
                    .params
                    .iter()
                    .map(|param| lower_type(builder, &param._type))
                    .collect::<CXResult<Vec<_>>>()?,
                return_type: if signature.return_type.is_unreachable() {
                    builder.types().unit()
                } else {
                    lower_type(builder, &signature.return_type)?
                },
                variadic: signature.var_args,
            },
        },
        THIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque {
            size: *size,
            alignment: *alignment,
        },
        THIRTypeKind::Undefined => MIRTypeKind::Undefined,
        THIRTypeKind::Unreachable => MIRTypeKind::Void,
        THIRTypeKind::Str => MIRTypeKind::Str,
    })
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

pub(crate) fn lower_prototype(
    builder: &mut MIRBuilder,
    prototype: &THIRFnPrototype,
) -> CXResult<MIRFnPrototype> {
    let signature = prototype.signature();

    let return_type = lower_type(builder, &signature.return_type)?;
    let mut params = signature
        .params
        .iter()
        .map(|parameter| {
            let ty = lower_type(builder, &parameter._type)?;

            MIRFnParam::new(parameter.name.clone(), ty, parameter._type.is_nodrop())
        })
        .collect::<Vec<_>>();

    Ok(MIRFnPrototype::new(
        MIRFnSignature::new(
            params,
            return_type,
            signature.var_args,
            signature.contract.safe(),
        ),
        prototype.linkage(),
        CXIdent::from(prototype.symbol_name().to_string()),
        prototype.debug_name().cloned(),
    ))
}

pub(crate) fn lower_comptime_prototype(
    builder: &mut MIRBuilder,
    prototype: &THIRComptimeFnPrototype,
) -> CXResult<MIRComptimeFnPrototype> {
    let mut params = Vec::with_capacity(prototype.params().len());

    for parameter in prototype.params() {
        let ty = lower_type(self, &parameter.value_type._type)?;
        let staged_params = if parameter.value_type.expr {
            Some(
                parameter
                    .value_type
                    .params
                    .iter()
                    .map(|ty| lower_type(self, ty))
                    .collect::<CXResult<Vec<_>>>()?,
            )
        } else {
            None
        };
        let param = match parameter.name.clone() {
            Some(name) => MIRFnParam::named(name, ty),
            None => MIRFnParam::new(ty),
        }
        .with_staged(
            staged_params,
            parameter.value_type.expr && parameter.value_type._type.is_unreachable(),
        );
        params.push(param);
    }

    let return_type = lower_type(self, &prototype.return_type()._type)?;
    let return_staged_params = if prototype.return_type().expr {
        Some(
            prototype
                .return_type()
                .params
                .iter()
                .map(|ty| lower_type(self, ty))
                .collect::<CXResult<Vec<_>>>()?,
        )
    } else {
        None
    };

    Ok(MIRComptimeFnPrototype::new(
        CXIdent::from(prototype.symbol_name().to_string()),
        MIRComptimeFnSignature::new(
            MIRComptimeType::new(return_type, prototype.return_type()._type.is_nodrop()),
            params,
        )
        .with_staged_return(return_staged_params),
    ))
}
