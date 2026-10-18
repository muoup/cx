use cx_hmir::{
    ty::{
        HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{coerce::pending::Coercion, op::specify, types},
};

pub(crate) fn decays(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    matches!(
        env.resolve_kind(ty),
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_)
    )
}

// C11 6.3.2.1: an array, string or function used as a value is a pointer to its first element, or to itself
pub(crate) fn try_decay(env: &HMIREnvironment, value: &HMIRValue) -> Coercion {
    match env.resolve_kind(value.ty()) {
        HMIRTypeKind::Array { element, .. } => {
            let element = *element;
            let constant = env.resolve_type_id(value.ty()).semantics().constant;

            Coercion::apply(move |context, value, span| {
                let element = match constant {
                    true => {
                        let specifiers = HMIRTypeSemantics {
                            constant: true,
                            ..HMIRTypeSemantics::default()
                        };
                        specify(context, element, specifiers, span)
                    }
                    false => element,
                };

                let value = emit::spill(context, value, span)?;
                decay(context, value, element, span)
            })
        }
        HMIRTypeKind::Str => Coercion::apply(|context, value, span| {
            let element = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
            decay(context, value, element, span)
        }),
        HMIRTypeKind::Function(_) => Coercion::apply(|context, value, span| {
            let function = value.ty();
            decay(context, value, function, span)
        }),
        _ => Coercion::unapplied(),
    }
}

// A constant string or function stands for its own address; anything else is addressed through its storage
pub(crate) fn decay(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    pointee: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = types::pointer(context.env_mut(), pointee, span);

    match value.kind() {
        HMIRValueKind::Constant(_) => Ok(value.with_ty(pointer)),
        _ => emit::address(context, &value, pointer, span),
    }
}
