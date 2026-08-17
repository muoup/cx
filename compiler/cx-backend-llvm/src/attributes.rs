use cx_lmir::types::{LMIRType, LMIRTypeKind};
use inkwell::attributes::Attribute;
use inkwell::context::Context;
use inkwell::types::AnyTypeEnum;

pub(crate) fn attr_nonnull(context: &Context) -> Attribute {
    context.create_enum_attribute(Attribute::get_named_enum_kind_id("nonnull"), 1)
}

pub(crate) fn attr_dereferenceable(context: &Context, bytes: u64) -> Attribute {
    context.create_enum_attribute(Attribute::get_named_enum_kind_id("dereferenceable"), bytes)
}

pub(crate) fn attr_sret(context: &Context, pointee: AnyTypeEnum) -> Attribute {
    context.create_type_attribute(Attribute::get_named_enum_kind_id("sret"), pointee)
}

pub(crate) fn attr_byval(context: &Context, pointee: AnyTypeEnum) -> Attribute {
    context.create_type_attribute(Attribute::get_named_enum_kind_id("byval"), pointee)
}

pub(crate) fn attr_alignment(context: &Context, alignment: u8) -> Attribute {
    context.create_enum_attribute(
        Attribute::get_named_enum_kind_id("align"),
        u64::from(alignment),
    )
}

pub fn get_type_attributes(context: &Context, _type: &LMIRType) -> Vec<Attribute> {
    match _type.kind {
        LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
            ..
        } => vec![attr_nonnull(context)],
        LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable,
            ..
        } => vec![
            attr_nonnull(context),
            attr_dereferenceable(context, dereferenceable as u64),
        ],

        _ => vec![],
    }
}
