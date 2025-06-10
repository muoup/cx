use inkwell::attributes::Attribute;
use inkwell::context::Context;

pub(crate) fn noundef(context: &Context) -> Attribute {
    context.create_enum_attribute(
        Attribute::get_named_enum_kind_id("noundef"), 1
    )
}
