use cx_hir::ast::types::ANONYMOUS_MEMBER_PREFIX;
use cx_thir::{
    thir::{
        data::{THIRType, THIRTypeKind},
        r#type::THIRField,
    },
    type_context::THIRTypeContext,
};

use crate::symbol::registry::MIRSymbolRegistry;

pub struct StructField {
    pub index: usize,
    pub field_type: THIRType,
    pub is_bitfield: bool,
}

fn aggregate_fields<'a>(
    definitions: &'a MIRSymbolRegistry,
    struct_type: &'a THIRType,
) -> Option<&'a [THIRField]> {
    let struct_type = struct_type
        .mem_ref_inner()
        .map(|id| definitions.resolve_type_id(id))
        .unwrap_or(struct_type);

    match &struct_type.kind {
        THIRTypeKind::Structured { fields } => Some(fields),
        THIRTypeKind::Union { variants } => Some(variants),

        _ => None,
    }
}

pub fn anonymous_member_containing(
    definitions: &MIRSymbolRegistry,
    struct_type: &THIRType,
    field_name: &str,
) -> Option<StructField> {
    aggregate_fields(definitions, struct_type)?
        .iter()
        .enumerate()
        .find_map(|(index, field)| {
            if !field.name()?.starts_with(ANONYMOUS_MEMBER_PREFIX) {
                return None;
            }

            let field_type = definitions.resolve_type_id(field.ty()).clone();
            let contains = struct_field(definitions, &field_type, field_name).is_some()
                || anonymous_member_containing(definitions, &field_type, field_name).is_some();

            contains.then_some(StructField {
                index,
                field_type,
                is_bitfield: false,
            })
        })
}

pub fn struct_field(
    definitions: &MIRSymbolRegistry,
    struct_type: &THIRType,
    field_name: &str,
) -> Option<StructField> {
    let fields = aggregate_fields(definitions, struct_type)?;

    fields
        .iter()
        .position(|field| field.name() == Some(field_name))
        .map(|index| {
            let field_type = definitions.resolve_type_id(fields[index].ty()).clone();
            StructField {
                index,
                field_type,
                is_bitfield: matches!(fields[index], THIRField::Bitfield { .. }),
            }
        })
}
