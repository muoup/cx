use cx_mir::{
    mir::data::{MIRType, MIRTypeKind},
    registry::MIRSymbolRegistry,
};

pub struct StructField {
    pub index: usize,
    pub field_type: MIRType,
}

pub fn struct_field(
    struct_type: &MIRType,
    definitions: &MIRSymbolRegistry,
    field_name: &str,
) -> Option<StructField> {
    let struct_type = struct_type
        .mem_ref_inner()
        .map(|id| definitions.resolve_type_id(id))
        .unwrap_or(struct_type);

    let fields = match &struct_type.kind {
        MIRTypeKind::Structured { fields } => fields,
        MIRTypeKind::Union { variants } => variants,

        _ => return None,
    };

    fields
        .iter()
        .position(|field| field.name() == Some(field_name))
        .and_then(|index| {
            let field_type = definitions.resolve_type_id(fields[index].ty()).clone();
            Some(StructField { index, field_type })
        })
}
