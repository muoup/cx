use cx_mir::{
    mir::data::{MIRType, MIRTypeKind},
    type_context::MIRTypeContext,
};

use crate::symbol::registry::MIRSymbolRegistry;

pub struct StructField {
    pub index: usize,
    pub field_type: MIRType,
}

pub fn struct_field(
    definitions: &MIRSymbolRegistry,
    struct_type: &MIRType,
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
        .map(|index| {
            let field_type = definitions.resolve_type_id(fields[index].ty()).clone();
            StructField { index, field_type }
        })
}
