use cx_hmir::{
    HMIRBody,
    expr::op::{HMIRMemberStep, HMIRTypeOp},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
    error::CXError,
};
use cx_mir::ty::layout::{MIRTypeLayout, calculate_field_layout, calculate_type_layout};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, aggregate, deduce, eval_comptime, flow::Evaluation, types},
    log::hmir_error,
    lower::types::lower_type,
    value::{HMIRValue, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_type_op(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<Evaluation<HMIRValue>> {
    let (HMIRTypeOp::SizeOf(subject) | HMIRTypeOp::AlignOf(subject) | HMIRTypeOp::OffsetOf { ty: subject, .. }) =
        op
    else {
        todo!("HMIRTypeOp: {}", op.path())
    };
    let subject = deduce::subject(context, body, *subject)?;
    let (span, op) = (span.clone(), op.clone());
    subject.and_then(context, body, None, move |context, body, ty, _| {
        let width = HMIRIntWidth::pointer_width(context.env().architecture());
        let size_type = types::int(context.env_mut(), width, false, &span);
        let value = match &op {
            HMIRTypeOp::SizeOf(_) => layout(context, ty, &span)?.size() as i128,
            HMIRTypeOp::AlignOf(_) => layout(context, ty, &span)?.alignment() as i128,
            HMIRTypeOp::OffsetOf { member, .. } => {
                offset(context, body, ty, member, size_type, &span)?
            }
            _ => unreachable!("only layout queries are evaluated here"),
        };
        Ok(Evaluation::Ready(HMIRValue::constant(size_type, HMIRConstant::Int(value))))
    })
}

fn layout(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> CXResult<MIRTypeLayout> {
    if !context.complete_object(ty) {
        return Err(incomplete_type(context, ty, span));
    }
    let ty = lower_type(context.env_mut(), ty, span)?;
    Ok(calculate_type_layout(context.env().module(), ty))
}

fn offset(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    mut current: HMIRTypeID,
    member: &[HMIRMemberStep],
    size_type: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<i128> {
    if !context.complete_object(current) {
        return Err(incomplete_type(context, current, span));
    }
    let mut offset = 0i128;
    for step in member {
        match step {
            HMIRMemberStep::Field(name) => {
                let Some(path) = aggregate::member_path(context.env(), current, name) else {
                    let member = (types::name(context.env(), current), name.to_string());
                    return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
                };
                for index in path {
                    let (field_ty, field_offset) = field(context, current, index, span)?;
                    current = field_ty;
                    offset = offset.wrapping_add(field_offset as i128);
                }
            }
            HMIRMemberStep::Index(index) => {
                let index_span = body.expr(*index).span();
                let Some(element) = context.env().array_inner(current) else {
                    let mismatch = (
                        "offsetof subscript".into(),
                        "an array member".into(),
                        types::name(context.env(), current),
                    );
                    return Err(hmir_error(index_span, &typecheck::TYPE_MISMATCH, mismatch).into());
                };
                let element_size = layout(context, element, span)?.size() as i128;
                let value = eval_comptime(context, body, *index, Some(size_type))?;
                let (HMIRTypeKind::Int(int_type), Some(value)) =
                    (context.env().resolve_kind(value.ty()), value.as_int())
                else {
                    let subject = ("offsetof subscript".into(), "integer".into());
                    return Err(hmir_error(index_span, &mir::EXPECTED_CONSTANT, subject).into());
                };

                let index = int::normalize(value, *int_type);
                offset = offset.wrapping_add(element_size.wrapping_mul(index));
                current = element;
            }
        }
    }
    Ok(offset)
}

fn field(
    context: &mut LocalLoweringContext,
    owner: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CXResult<(HMIRTypeID, usize)> {
    let missing = || hmir_error(span, &mir::MISSING_ENTITY, (format!("field {index}"), "aggregate layout".into()));
    let field = aggregate::member_fields(context.env(), owner).get(index).ok_or_else(missing)?;
    if field.bit_width().is_some() {
        let form = ("offsetof".into(), "bit-field member".into());
        return Err(hmir_error(span, &typecheck::INVALID_FORM, form).into());
    }

    let field_ty = *field.ty();
    let owner = lower_type(context.env_mut(), owner, span)?;
    let layout = calculate_field_layout(context.env().module(), owner, index).ok_or_else(missing)?;
    Ok((field_ty, layout.offset()))
}

fn incomplete_type(context: &LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> CXError {
    let subject = format!("operand of type '{}'", types::name(context.env(), ty));
    hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject)
}
