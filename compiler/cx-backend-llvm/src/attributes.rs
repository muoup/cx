use cx_bytecode_data::types::{BCType, BCTypeKind};
use inkwell::attributes::Attribute;
use inkwell::context::Context;

pub(crate) fn attr_noundef(context: &Context) -> Attribute {
    context.create_enum_attribute(Attribute::get_named_enum_kind_id("noundef"), 1)
}

pub(crate) fn attr_nonnull(context: &Context) -> Attribute {
    context.create_enum_attribute(Attribute::get_named_enum_kind_id("nonnull"), 1)
}

pub(crate) fn attr_dereferenceable(context: &Context, bytes: u64) -> Attribute {
    context.create_enum_attribute(Attribute::get_named_enum_kind_id("dereferenceable"), bytes)
}

pub fn get_type_attributes(context: &Context, _type: &BCType) -> Vec<Attribute> {
    match _type.kind {
        BCTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        } => vec![attr_nonnull(context), attr_noundef(context)],
        BCTypeKind::Pointer {
            nullable: false,
            dereferenceable,
        } => vec![
            attr_nonnull(context),
            attr_noundef(context),
            attr_dereferenceable(context, dereferenceable as u64),
        ],

        _ => vec![attr_noundef(context)],
    }
}
