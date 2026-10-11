use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRBindable, MIRConstant, MIRInstructionKind, MIRInternalIntrinsic, MIRPtrIntrinsic, MIRTarget,
    MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    env::lowering::FnLoweringContext,
    log::hmir_error,
    lower::{
        coerce::convert,
        constant,
        dispatch::lower_expression,
        emit,
        op::{lower_binary_operands, lower_operand, promote_operand},
        types,
    },
};

pub(crate) fn lower_assign(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    target: HMIRExprID,
    op: Option<HMIRBinaryOp>,
    value: HMIRExprID,
) -> CXResult<HMIRValue> {
    let target = lower_expression(context, body, body.expr(target))?;
    if emit::terminated(context.builder()) {
        return Ok(target);
    }

    let target = emit::referent(context, target, span)?;
    let Some(destination) = emit::target(context, &target) else {
        return Err(hmir_error(
            span,
            &typecheck::NOT_ADDRESSABLE,
            "assign to".into(),
        ));
    };
    let ty = target.ty();

    let value = match op {
        Some(op) => {
            let current = promote_operand(context, target.clone(), span)?;
            let Some(operand) = lower_operand(context, body, span, value)? else {
                return Ok(constant::unit(context, span));
            };

            lower_binary_operands(context, span, op, current, operand)?
        }
        None => {
            let value = lower_expression(context, body, body.expr(value))?;
            if emit::terminated(context.builder()) {
                return Ok(value);
            }

            value
        }
    };

    let value = convert(context, value, ty, span)?;
    let value = emit::read(context, value, span)?;
    let value = emit::materialize(context, &value, span)?;
    let mir = types::lower_type(context.env_mut(), ty, span)?;

    if let MIRTarget::Place(place) = destination {
        let place = MIRBindable::Place(place);
        context.builder_mut().emit(
            MIRInstructionKind::Invalidate {
                place: place.clone(),
                kind: MIRInvalidationKind::Drop,
            },
            span.clone(),
        );
        context
            .builder_mut()
            .emit(MIRInstructionKind::Initialize { place }, span.clone());
    }
    emit::store(context.builder_mut(), destination, value, mir, span.clone());

    Ok(target)
}

pub(crate) fn lower_address_of(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> CXResult<HMIRValue> {
    let value = lower_expression(context, body, body.expr(operand))?;
    if emit::terminated(context.builder()) {
        return Ok(value);
    }

    let value = emit::referent(context, value, span)?;
    if matches!(
        context.env().resolve_type_id(value.ty()).kind(),
        HMIRTypeKind::Str | HMIRTypeKind::Function(_)
    ) {
        todo!("address of a string or function")
    }

    let value = emit::spill(context, value, span)?;
    let ty = context
        .env_mut()
        .resolver_mut()
        .realize_type(HMIRTypeKind::PointerTo(value.ty()), span);

    match value.kind() {
        HMIRValueKind::Place(place) => Ok(HMIRValue::constant(
            ty,
            HMIRConstant::Address {
                place: *place,
                path: Vec::new(),
                ty,
            },
        )),
        HMIRValueKind::Indirect(reference) => {
            let reference = emit::materialize(context, reference, span)?;
            let out = emit::register(context, ty, span)?;
            let intrinsic = MIRInternalIntrinsic::ReferenceAddress {
                out: MIRTarget::Register(out),
                reference,
            };
            emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

            Ok(context.bind(ty, MIRBindable::Register(out)))
        }
        HMIRValueKind::Constant(_) => unreachable!("a spilled value is addressable"),
    }
}

pub(crate) fn lower_dereference(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    pointer: HMIRExprID,
) -> CXResult<HMIRValue> {
    let value = lower_expression(context, body, body.expr(pointer))?;
    if emit::terminated(context.builder()) {
        return Ok(value);
    }

    let value = emit::referent(context, value, span)?;
    dereference(context, value, span)
}

pub(crate) fn dereference(
    context: &mut FnLoweringContext,
    pointer: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let inner = match context.env().resolve_type_id(pointer.ty()).kind() {
        HMIRTypeKind::PointerTo(inner) => *inner,
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_) => {
            todo!("array, string or function operand decay")
        }
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::UNEXPECTED_KIND,
                (todo!("type display"), "a pointer".into()),
            ));
        }
    };
    if matches!(
        context.env().resolve_type_id(inner).kind(),
        HMIRTypeKind::Function(_)
    ) {
        todo!("dereference of a function pointer")
    }

    if matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        return Ok(HMIRValue::indirect(inner, pointer));
    }

    let pointer = emit::read(context, pointer, span)?;
    let value = emit::materialize(context, &pointer, span)?;

    let null = MIRConstant::Nullptr {
        ty: types::lower_type(context.env_mut(), pointer.ty(), span)?,
    };
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let condition = emit::register(context, bool_ty, span)?;
    let nonnull = MIRPtrIntrinsic::Neq {
        out: MIRTarget::Register(condition),
        lhs: value.clone(),
        rhs: MIRValue::Constant(null),
    };
    emit::intrinsic(context.builder_mut(), nonnull, span.clone());
    let assert = MIRInternalIntrinsic::Assert {
        condition: MIRValue::Register(condition),
        message: Some("dereferenced a null pointer".into()),
    };
    emit::intrinsic(context.builder_mut(), assert, span.clone());

    let reference_ty = context
        .env_mut()
        .resolver_mut()
        .realize_type(HMIRTypeKind::ReferenceTo(inner), span);
    let target_ty = types::lower_type(context.env_mut(), reference_ty, span)?;
    let out = emit::register(context, reference_ty, span)?;
    let bitcast = MIRInternalIntrinsic::Bitcast {
        out: MIRTarget::Register(out),
        value,
        target_ty,
    };
    emit::intrinsic(context.builder_mut(), bitcast, span.clone());

    let reference = context.bind(reference_ty, MIRBindable::Register(out));
    Ok(HMIRValue::indirect(inner, reference))
}
