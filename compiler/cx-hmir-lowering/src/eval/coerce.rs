use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRCoerceMode,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, aggregate::eval_initializer, constant, eval, eval_type, place::dereference,
        types,
    },
    log::hmir_error,
};

pub(crate) fn eval_coerce(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if mode == HMIRCoerceMode::Truthy {
        let value = eval(context, body, value)?;
        return Ok(truthy(context, value, span)?);
    }

    let target = eval_type(context, body, target)?;
    let value = eval_initializer(context, body, value, target)?;
    Ok(convert(context, value, target, span)?)
}

pub(crate) fn truthy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = emit::referent(context, value, span)?;
    let value = decay(context, value, span)?;
    let ty = value.ty();

    let zero = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => return Ok(value),
        HMIRTypeKind::Int(_) => HMIRConstant::Int { value: 0, ty },
        HMIRTypeKind::Float(_) => HMIRConstant::Float {
            value: FloatWrapper::from(0.0),
            ty,
        },
        HMIRTypeKind::PointerTo(_) => HMIRConstant::Null(ty),
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::NO_TRUTH_VALUE,
                types::name(context.env(), ty),
            ));
        }
    };

    let zero = HMIRValue::constant(ty, zero);
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    emit::binary(context, HMIRBinaryOp::Neq, &value, &zero, bool_ty, span)
}

pub(crate) fn decay(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointee = match context.env().resolve_kind(value.ty()) {
        HMIRTypeKind::Str => types::int(context.env_mut(), HMIRIntWidth::I8, false, span),
        HMIRTypeKind::Function(_) => value.ty(),
        HMIRTypeKind::Array { .. } => todo!("decay of an array to a pointer"),
        _ => return Ok(value),
    };
    if !matches!(value.kind(), HMIRValueKind::Constant(_)) {
        todo!("decay of a string or function that is not a constant")
    }

    let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::PointerTo(pointee),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    Ok(value.with_ty(ty))
}

fn bind_reference(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointee = context.env().pointer_inner(value.ty());
    let value = match pointee {
        Some(pointee) if context.env().same_type(pointee, inner) => {
            dereference(context, value, span)?
        }
        _ => value,
    };

    if !context.env().same_type(value.ty(), inner) {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (types::name(context.env(), value.ty()), types::name(context.env(), inner)),
        ));
    }

    let value = emit::spill(context, value, span)?;
    Ok(match value.kind() {
        HMIRValueKind::Place(place) => HMIRValue::constant(
            target,
            HMIRConstant::Address {
                place: *place,
                path: Vec::new(),
                ty: target,
            },
        ),
        HMIRValueKind::Indirect(reference) => reference.as_ref().clone().with_ty(target),
        HMIRValueKind::Constant(_) => unreachable!("a spilled value is addressable"),
    })
}

pub(crate) fn convert(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    if context.env().same_type(source, target) {
        return Ok(value.with_ty(target));
    }

    let kinds = (
        context.env().resolve_kind(source).clone(),
        context.env().resolve_kind(target).clone(),
    );

    match &kinds {
        (_, HMIRTypeKind::Void) => return Ok(constant::unit(context, span)),

        (
            HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_),
            HMIRTypeKind::Int(to),
        ) if to.width() == HMIRIntWidth::I1 => {
            return Ok(truthy(context, value, span)?.with_ty(target));
        }

        (HMIRTypeKind::ReferenceTo(inner), _) => {
            if context
                .env()
                .resolve_type_id(*inner)
                .semantics()
                .move_semantics
                .is_nocopy()
            {
                return Err(hmir_error(
                    span,
                    &typecheck::INVALID_CONVERSION,
                    (
                        types::name(context.env(), source),
                        types::name(context.env(), target),
                    ),
                ));
            }
            let value = emit::referent(context, value, span)?;
            let value = emit::read(context, value, span)?;
            return convert(context, value, target, span);
        }
        (_, HMIRTypeKind::ReferenceTo(inner)) => {
            return bind_reference(context, value, *inner, target, span);
        }
        (HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_), _) => {
            let value = decay(context, value, span)?;
            return convert(context, value, target, span);
        }
        (_, HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_)) => {
            return Err(hmir_error(
                span,
                &typecheck::INVALID_CONVERSION,
                (types::name(context.env(), source), types::name(context.env(), target)),
            ));
        }

        _ => {}
    }

    let source_mir = types::lower_type(context.env_mut(), source, span)?;
    let target_mir = types::lower_type(context.env_mut(), target, span)?;
    if source_mir == target_mir {
        return Ok(emit::read(context, value, span)?.with_ty(target));
    }

    match kinds {
        (
            HMIRTypeKind::Int(_),
            HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_),
        )
        | (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_) | HMIRTypeKind::PointerTo(_)) => {
            emit::cast(context, &value, target, span)
        }
        _ => Err(hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (types::name(context.env(), source), types::name(context.env(), target)),
        )),
    }
}

pub(crate) fn value(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.env().reference_inner(value.ty()) {
        Some(inner) => convert(context, value, inner, span),
        None => Ok(value),
    }
}
