use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    intrinsic::HMIRIntWidth,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{coerce::result::Coercion, op::specify, storage, types},
    value::{HMIRValue, HMIRValueKind},
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let reference = context.env().reference_inner(value.ty());
    let subject = reference.unwrap_or(value.ty());

    match context.env().resolve_kind(subject).clone() {
        HMIRTypeKind::Array { element, .. } => {
            let mut storage = match reference {
                Some(_) => HMIRValue::indirect(subject, value),
                None => value,
            };
            if matches!(storage.kind(), HMIRValueKind::Constant(_)) {
                storage = storage::temporary(context, storage, span)?;
            }

            let element = match context.env().semantics(subject).constant {
                true => specify(context, element, constant(), span),
                false => element,
            };
            let pointer = pointer_to(context, element, span);
            Ok(Coercion::Applied(emit::address(
                context, &storage, pointer, span,
            )?))
        }

        HMIRTypeKind::Str | HMIRTypeKind::Function(_) => {
            if reference.is_some() || !matches!(value.kind(), HMIRValueKind::Constant(_)) {
                todo!("decay of a string or function that is not a constant")
            }

            let pointee = match context.env().resolve_kind(subject) {
                HMIRTypeKind::Str => types::int(context.env_mut(), HMIRIntWidth::I8, false, span),
                _ => subject,
            };
            let pointer = pointer_to(context, pointee, span);
            Ok(Coercion::Applied(value.with_ty(pointer)))
        }

        _ => Ok(Coercion::Unapplied(value)),
    }
}

fn constant() -> HMIRTypeSemantics {
    HMIRTypeSemantics {
        constant: true,
        ..Default::default()
    }
}

fn pointer_to(
    context: &mut LocalLoweringContext,
    pointee: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::PointerTo(pointee),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ))
}
