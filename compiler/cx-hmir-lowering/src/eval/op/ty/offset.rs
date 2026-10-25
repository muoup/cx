use cx_hmir::{
    HMIRBody,
    expr::op::{HMIRMemberStep, HMIRTypeOp},
    ty::{
        HMIRTypeID, HMIRTypeKind, aggregate::HMIRFieldDef, context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
    error::CXError,
};
use cx_mir::ty::layout::{calculate_field_layout, calculate_type_layout};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{EvalResult, eval_comptime, flow::Evaluation, types, unevaluated},
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_offsetof(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<HMIRValue> {
    let HMIRTypeOp::OffsetOf { ty, member } = op else {
        unreachable!()
    };
    let subject = unevaluated::subject(context, body, *ty)?;
    let span = span.clone();
    let member = member.clone();

    subject.and_then(context, body, None, move |context, body, ty, _| {
        if !context.complete_object(ty) {
            return Err(incomplete_type(context, ty, &span));
        }

        let size_width = HMIRIntWidth::pointer_width(context.env().architecture());
        let size_type = types::int(context.env_mut(), size_width, false, &span);
        let mut current = ty;
        let mut offset = 0i128;
        for step in &member {
            match step {
                HMIRMemberStep::Field(name) => {
                    let Some(path) = member_path(context.env(), current, name) else {
                        return Err(hmir_error(
                            &span,
                            &typecheck::UNKNOWN_MEMBER,
                            (types::name(context.env(), current), name.to_string()),
                        )
                        .into());
                    };
                    for index in path {
                        let (field_ty, field_offset) = field(context, current, index, &span)?;
                        current = field_ty;
                        offset = offset.wrapping_add(field_offset as i128);
                    }
                }
                HMIRMemberStep::Index(index) => {
                    let Some(element) = context.env().array_inner(current) else {
                        let index_span = body.expr(*index).span();
                        return Err(hmir_error(
                            index_span,
                            &typecheck::TYPE_MISMATCH,
                            (
                                "offsetof subscript".into(),
                                "an array member".into(),
                                types::name(context.env(), current),
                            ),
                        )
                        .into());
                    };
                    if !context.complete_object(element) {
                        return Err(incomplete_type(context, element, &span));
                    }

                    let element_mir = types::lower_type(context.env_mut(), element, &span)?;
                    let element_size =
                        calculate_type_layout(context.env().module(), element_mir).size();
                    let value = eval_comptime(context, body, *index, Some(size_type))?;
                    let (
                        HMIRTypeKind::Int(int_type),
                        HMIRValueKind::Constant(HMIRConstant::Int(value)),
                    ) = (context.env().resolve_kind(value.ty()), value.kind())
                    else {
                        let index_span = body.expr(*index).span();
                        return Err(hmir_error(
                            index_span,
                            &mir::EXPECTED_CONSTANT,
                            ("offsetof subscript".into(), "integer".into()),
                        )
                        .into());
                    };

                    let index = int::normalize(*value, *int_type);
                    offset = offset.wrapping_add((element_size as i128).wrapping_mul(index));
                    current = element;
                }
            }
        }

        Ok(Evaluation::Ready(HMIRValue::constant(
            size_type,
            HMIRConstant::Int(offset),
        )))
    })
}

fn field(
    context: &mut LocalLoweringContext,
    owner: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CXResult<(HMIRTypeID, usize)> {
    let Some(field) = member_fields(context.env(), owner).and_then(|fields| fields.get(index))
    else {
        return Err(hmir_error(
            span,
            &mir::MISSING_ENTITY,
            (format!("field {index}"), "aggregate layout".into()),
        )
        .into());
    };
    if field.bit_width().is_some() {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_FORM,
            ("offsetof".into(), "bit-field member".into()),
        )
        .into());
    }

    let field_ty = *field.ty();
    let owner_mir = types::lower_type(context.env_mut(), owner, span)?;
    let layout =
        calculate_field_layout(context.env().module(), owner_mir, index).ok_or_else(|| {
            hmir_error(
                span,
                &mir::MISSING_ENTITY,
                (format!("field {index}"), "aggregate layout".into()),
            )
        })?;
    Ok((field_ty, layout.offset()))
}

fn member_path(
    env: &HMIREnvironment<'_, '_>,
    ty: HMIRTypeID,
    name: &CXIdent,
) -> Option<Vec<usize>> {
    let fields = member_fields(env, ty)?;
    if let Some(index) = fields.iter().position(|field| field.name() == Some(name)) {
        return Some(vec![index]);
    }

    fields
        .iter()
        .enumerate()
        .filter(|(_, field)| field.name().is_none())
        .find_map(|(index, field)| {
            let mut path = member_path(env, *field.ty(), name)?;
            path.insert(0, index);
            Some(path)
        })
}

fn member_fields<'env>(
    env: &'env HMIREnvironment<'_, '_>,
    ty: HMIRTypeID,
) -> Option<&'env [HMIRFieldDef<HMIRTypeID>]> {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => Some(fields),
        _ => None,
    }
}

fn incomplete_type(context: &LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> CXError {
    hmir_error(
        span,
        &typecheck::INCOMPLETE_TYPE,
        format!("operand of type '{}'", types::name(context.env(), ty)),
    )
}
