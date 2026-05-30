use cx_mir::mir::data::{MIRType, MIRSymbolRegistry, MIRTypeKind};

pub struct StructField {
    pub index: usize,
    pub field_type: MIRType,
}

pub fn struct_field(
    struct_type: &MIRType,
    definitions: &MIRSymbolRegistry,
    field_name: &str,
) -> Option<StructField> {
    let struct_type = definitions.memory_resident_type(struct_type);
    if !matches!(
        struct_type.kind,
        MIRTypeKind::Structured { .. } | MIRTypeKind::Union { .. }
    ) {
        return None;
    }
    let Some(fields) = definitions.aggregate_fields(struct_type) else {
        return None;
    };

    fields
        .iter()
        .position(|field| field.name() == Some(field_name))
        .and_then(|index| {
            let field_type = definitions.get(fields[index].type_id())?.clone();
            Some(StructField { index, field_type })
        })
}
