use cx_hmir::ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{env::HMIREnvironment, eval::types, log::hmir_error};

pub fn declaration(env: &HMIREnvironment, ty: &HMIRType) -> CXResult<()> {
    let fields = match ty.kind() {
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => fields,
        HMIRTypeKind::PointerTo(inner)
            if matches!(env.resolve_kind(*inner), HMIRTypeKind::Unreachable) =>
        {
            return Err(hmir_error(
                ty.decl_span(),
                &typecheck::TYPE_REQUIREMENT,
                ("pointer target".into(), "a reachable type".into(), None),
            ));
        }
        _ => return Ok(()),
    };
    let semantics = ty.semantics();
    for field in fields {
        let owned = types::owned_semantics(env, *field.ty());
        let required = if owned.move_semantics.is_nodrop() && !semantics.move_semantics.is_nodrop()
        {
            Some("@nodrop")
        } else if owned.move_semantics.is_nocopy() && !semantics.move_semantics.is_nocopy() {
            Some("@nocopy")
        } else if owned.unsafe_move && !semantics.unsafe_move {
            Some("@unsafe_move")
        } else {
            None
        };
        if let Some(required) = required {
            let name = field
                .name()
                .map(|name| name.as_str())
                .unwrap_or("<anonymous>");
            return Err(hmir_error(
                ty.decl_span(),
                &typecheck::FIELD_TRAIT,
                (name.into(), required.into(), required.into()),
            ));
        }
    }
    Ok(())
}

pub fn object(env: &HMIREnvironment, ty: HMIRTypeID, span: &TokenRange) -> CXResult<()> {
    if matches!(env.resolve_kind(ty), HMIRTypeKind::Function(_)) {
        return Err(hmir_error(
            span,
            &typecheck::TYPE_REQUIREMENT,
            (
                "local variable".into(),
                "an object type".into(),
                Some(types::name(env, ty)),
            ),
        ));
    }
    Ok(())
}
