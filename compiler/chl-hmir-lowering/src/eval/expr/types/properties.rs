use chl_hmir::{
    HMIRFunctionStage,
    ty::{HMIRTypeID, HMIRTypeKind, HMIRUndefined, context::HMIRTypeContext},
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_tokens::TokenRange;

use super::name;
use crate::{env::HMIREnvironment, log::hmir_error};

pub(crate) fn opaque_slot(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    matches!(
        env.resolve_kind(ty),
        HMIRTypeKind::Type
            | HMIRTypeKind::Staged(_)
            | HMIRTypeKind::Dependent
            | HMIRTypeKind::Function { .. }
    )
}

pub(crate) fn comptime_only(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    env.semantics(ty).comptime
        || opaque_slot(env, ty)
        || match env.resolve_kind(ty) {
            HMIRTypeKind::Array { element, .. }
            | HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
                comptime_only(env, *element)
            }
            HMIRTypeKind::Struct { fields }
            | HMIRTypeKind::Union { fields }
            | HMIRTypeKind::TaggedUnion { fields } => {
                fields.iter().any(|field| comptime_only(env, *field.ty()))
            }
            _ => false,
        }
}

pub(crate) fn runtime_function(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    let HMIRTypeKind::Function { signature, .. } = env.resolve_kind(ty) else {
        return false;
    };
    signature.stage() != HMIRFunctionStage::Comptime
        && !signature
            .params()
            .iter()
            .any(|param| param.comptime() || comptime_only(env, *param.ty()))
        && !comptime_only(env, *signature.return_type())
}

pub(crate) fn require_runtime(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<()> {
    if comptime_only(env, ty) {
        return Err(hmir_error(
            span,
            &typecheck::COMPTIME_ONLY_TYPE,
            name(env, ty),
        ));
    }
    Ok(())
}
