use cx_mir::{MIRConstant, MIRField, MIRTypeID, MIRTypeKind, MIRUnit, ty::interface::MTRegistry};

#[derive(Debug, Clone)]
pub(crate) enum RuntimeValue {
    Constant(MIRConstant),
    Place(cx_mir::MIRPlace),
}

pub(crate) fn zero(unit: &MIRUnit, ty: MIRTypeID) -> Result<MIRConstant, String> {
    let kind = unit
        .types()
        .kind(ty)
        .map_err(|error| format!("cannot create zero value for {ty}: {error}"))?;
    match kind {
        MIRTypeKind::Void => Ok(MIRConstant::Unit),
        MIRTypeKind::Integer { ty, signed } => Ok(MIRConstant::Integer {
            value: 0,
            ty: *ty,
            signed: *signed,
        }),
        MIRTypeKind::Float { ty } => Ok(MIRConstant::Float {
            value: 0.0.into(),
            ty: *ty,
        }),
        MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. }
        | MIRTypeKind::Function { .. } => Ok(MIRConstant::Null { ty }),
        MIRTypeKind::Array { length, inner } => Ok(MIRConstant::Aggregate {
            ty,
            fields: (0..*length)
                .map(|index| Ok((index, zero(unit, *inner)?)))
                .collect::<Result<_, String>>()?,
        }),
        MIRTypeKind::Structured { fields } => aggregate_zero(unit, ty, fields),
        MIRTypeKind::Union { .. } | MIRTypeKind::TaggedUnion { .. } => Ok(MIRConstant::Aggregate {
            ty,
            fields: Vec::new(),
        }),
        MIRTypeKind::Opaque { .. } | MIRTypeKind::Str | MIRTypeKind::Undefined => {
            Err(format!("type {ty} has no comptime zero value"))
        }
    }
}

fn aggregate_zero(
    unit: &MIRUnit,
    ty: MIRTypeID,
    fields: &[MIRField],
) -> Result<MIRConstant, String> {
    Ok(MIRConstant::Aggregate {
        ty,
        fields: fields
            .iter()
            .enumerate()
            .map(|(index, field)| Ok((index, zero(unit, field.ty())?)))
            .collect::<Result<_, String>>()?,
    })
}
