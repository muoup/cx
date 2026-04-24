use cx_mir::mir::data::{MIRType, MIRTypeContext, MIRTypeKind};

pub struct StructField {
    pub index: usize,
    pub offset: usize,
    pub field_type: MIRType,
}

pub fn struct_field_offset(
    struct_type: &MIRType,
    definitions: &MIRTypeContext,
    field_index: usize,
) -> Option<usize> {
    let struct_type = definitions.memory_resident_type(struct_type);
    if !matches!(struct_type.kind, MIRTypeKind::Structured { .. }) {
        return None;
    }
    let Some(fields) = struct_type.aggregate_fields(definitions) else {
        unreachable!(
            "Invalid type for struct_field_offset: {}",
            struct_type.display_with(definitions)
        );
    };

    let mut field_offset = 0;

    for (i, (_, field_type)) in fields.iter().enumerate() {
        let field_alignment = field_type.type_alignment(definitions);

        field_offset = (field_offset + field_alignment - 1) / field_alignment * field_alignment;

        if i == field_index {
            return Some(field_offset);
        }

        field_offset += field_type.type_size(definitions);
    }

    None
}

pub fn struct_field(
    struct_type: &MIRType,
    definitions: &MIRTypeContext,
    field_name: &str,
) -> Option<StructField> {
    let struct_type = definitions.memory_resident_type(struct_type);
    if !matches!(struct_type.kind, MIRTypeKind::Structured { .. }) {
        return None;
    }
    let Some(fields) = struct_type.aggregate_fields(definitions) else {
        return None;
    };

    fields
        .iter()
        .position(|(name, _)| name.as_str() == field_name)
        .and_then(|index| {
            let offset = struct_field_offset(struct_type, definitions, index)?;

            Some(StructField {
                index,
                offset,
                field_type: fields[index].1.clone(),
            })
        })
}
