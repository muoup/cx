use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRFieldDef,
    context::HMIRTypeContext,
};
use cx_log::{CXResult, catalogue::typecheck};

use crate::{env::HMIREnvironment, eval::types, log::hmir_error};

// An unbounded array may only end a struct with another named member, and a type ending in one may not be held by
// a struct or an array
pub(super) fn validate(env: &HMIREnvironment, ty: &HMIRType) -> CXResult<()> {
    let (fields, position) = match ty.kind() {
        HMIRTypeKind::Struct { fields } => {
            member(env, ty, fields)?;
            (fields, "a struct member")
        }
        HMIRTypeKind::TaggedUnion { fields } => (fields, "an enum union member"),
        HMIRTypeKind::Array { element, .. }
        | HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
            return nested(env, ty, *element, "an array element");
        }
        _ => return Ok(()),
    };
    for field in fields {
        nested(env, ty, *field.ty(), position)?;
    }
    Ok(())
}

fn member(env: &HMIREnvironment, ty: &HMIRType, fields: &[HMIRFieldDef<HMIRTypeID>]) -> CXResult<()> {
    let Some(index) = fields
        .iter()
        .position(|field| env.is_unbounded_array(*field.ty()))
    else {
        return Ok(());
    };
    let field = &fields[index];
    let named = |field: &HMIRFieldDef<HMIRTypeID>| field.name().is_some() || field.bit_width().is_none();
    let rule = if index + 1 != fields.len() {
        "must be the last member of a struct"
    } else if !fields[..index].iter().any(named) {
        "requires another named member"
    } else if ty.semantics().move_semantics.is_nodrop()
        || types::owned_semantics(env, *field.ty()).move_semantics.is_nodrop()
    {
        "cannot belong to a @nodrop type"
    } else {
        return Ok(());
    };
    let name = field.name().map(|name| name.as_str()).unwrap_or("<anonymous>");
    let subject = (name.into(), rule.into());
    Err(hmir_error(ty.decl_span(), &typecheck::FLEXIBLE_ARRAY_MEMBER, subject))
}

fn nested(env: &HMIREnvironment, owner: &HMIRType, ty: HMIRTypeID, position: &str) -> CXResult<()> {
    if !has_flexible_member(env, ty) {
        return Ok(());
    }
    let subject = (types::name(env, ty), position.into());
    Err(hmir_error(owner.decl_span(), &typecheck::FLEXIBLE_ARRAY_NESTING, subject))
}

// C extends the restriction to a union holding such a struct, however deeply
fn has_flexible_member(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } => fields
            .last()
            .is_some_and(|field| env.is_unbounded_array(*field.ty())),
        HMIRTypeKind::Union { fields } => fields
            .iter()
            .any(|field| has_flexible_member(env, *field.ty())),
        _ => false,
    }
}
