use cx_mir::{MIRConstant, MIRField, MIRTypeID, MIRTypeKind, MIRUnit, ty::interface::MTRegistry};

pub(crate) fn field_type(
    unit: &MIRUnit,
    aggregate: MIRTypeID,
    field: usize,
) -> Result<MIRTypeID, String> {
    let kind = unit
        .types()
        .kind(aggregate)
        .map_err(|error| format!("invalid aggregate type {aggregate}: {error}"))?;
    match kind {
        MIRTypeKind::Array { inner, length } => {
            if field >= *length {
                return Err(format!(
                    "array field {field} is out of bounds for {aggregate}"
                ));
            }
            Ok(*inner)
        }
        MIRTypeKind::Structured { fields } | MIRTypeKind::Union { variants: fields } => fields
            .get(field)
            .map(MIRField::ty)
            .ok_or_else(|| format!("aggregate field {field} is out of bounds for {aggregate}")),
        MIRTypeKind::TaggedUnion { variants } => variants
            .get(field)
            .map(MIRField::ty)
            .ok_or_else(|| format!("variant {field} is out of bounds for {aggregate}")),
        _ => Err(format!("type {aggregate} is not an aggregate")),
    }
}

pub(crate) fn construct(
    unit: &MIRUnit,
    ty: MIRTypeID,
    fields: Vec<(usize, MIRConstant)>,
) -> Result<MIRConstant, String> {
    for (field, _) in &fields {
        field_type(unit, ty, *field)?;
    }
    Ok(MIRConstant::Aggregate { ty, fields })
}

pub(crate) fn string_literal(
    unit: &MIRUnit,
    global: cx_mir::MIRGlobalID,
    target: MIRTypeID,
) -> Result<MIRConstant, String> {
    let value = match unit
        .global(global)
        .ok_or_else(|| format!("string global {global} is not present in the MIR unit"))?
        .kind
        .clone()
    {
        cx_mir::MIRGlobalKind::StringLiteral { value } => value,
        _ => return Err(format!("global {global} is not a string literal")),
    };
    let kind = unit
        .types()
        .kind(target)
        .map_err(|error| format!("invalid string target {target}: {error}"))?;
    let MIRTypeKind::Array { inner, length } = kind else {
        return Err(format!("string literal target {target} is not an array"));
    };
    let MIRTypeKind::Integer { ty, signed } = unit
        .types()
        .kind(*inner)
        .map_err(|error| format!("invalid string element type {inner}: {error}"))?
    else {
        return Err(format!("string array element {inner} is not an integer"));
    };
    Ok(MIRConstant::Aggregate {
        ty: target,
        fields: value
            .bytes()
            .chain(std::iter::once(0))
            .take(*length)
            .enumerate()
            .map(|(index, byte)| {
                (
                    index,
                    MIRConstant::Integer {
                        value: i128::from(byte),
                        ty: *ty,
                        signed: *signed,
                    },
                )
            })
            .collect(),
    })
}
