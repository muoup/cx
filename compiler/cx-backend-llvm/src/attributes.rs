use inkwell::attributes::Attribute;
use inkwell::context::Context;
use cx_data_bytecode::types::{BCType, BCTypeKind};

pub(crate) fn noundef(context: &Context) -> Attribute {
    context.create_enum_attribute(
        Attribute::get_named_enum_kind_id("noundef"), 1
    )
}

pub(crate) fn nonnull(context: &Context) -> Attribute {
    context.create_enum_attribute(
        Attribute::get_named_enum_kind_id("nonnull"), 1
    )
}

pub fn get_type_attributes(context: &Context, _type: &BCType) -> Vec<Attribute> {
    match _type.kind {
        BCTypeKind::Pointer { nullable: false } => vec![nonnull(context), noundef(context)],
        
        _ => vec![noundef(context)],
    }
}