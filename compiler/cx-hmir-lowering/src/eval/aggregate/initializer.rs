use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{EvalResult, coerce, complete::elaborate, types},
    log::hmir_error,
    value::HMIRValue,
};

pub fn initialize(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    ty: HMIRTypeID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<HMIRValue> {
    let declared = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => fields.clone(),
        HMIRTypeKind::Array { element, length } => {
            return array(context, body, span, ty, *element, *length, fields);
        }
        HMIRTypeKind::TaggedUnion { .. } => todo!("tagged union initializers"),
        _ => return initialize_scalar(context, body, span, ty, fields),
    };
    let mut values = Vec::with_capacity(fields.len());
    let mut next = 0;
    for (name, value) in fields {
        let index = match name {
            Some(name) => {
                let field = declared.iter().position(|field| field.name() == Some(name));
                let Some(index) = field else {
                    return Err(hmir_error(
                        span,
                        &typecheck::UNKNOWN_MEMBER,
                        (types::name(context.env(), ty), name.to_string()),
                    )
                    .into());
                };

                index
            }
            None => next,
        };
        let Some(field) = declared.get(index) else {
            let limit = ("aggregate".into(), Some(declared.len()));
            return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit).into());
        };
        next = index + 1;

        values.push((index, field_value(context, body, *value, *field.ty())?));
    }

    Ok(emit::aggregate(context, ty, &values, span)?)
}

fn array(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    ty: HMIRTypeID,
    element: HMIRTypeID,
    length: Option<u64>,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<HMIRValue> {
    if let Some(length) = length
        && fields.len() as u64 > length
    {
        let limit = ("array".into(), usize::try_from(length).ok());
        return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit).into());
    }

    let mut values = Vec::with_capacity(fields.len());
    for (index, (name, value)) in fields.iter().enumerate() {
        if let Some(name) = name {
            let member = (types::name(context.env(), ty), name.to_string());
            return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
        }
        let value_span = body.expr(*value).span();
        let value = elaborate(context, body, *value)?.complete(context, body, Some(element))?;
        if fields.len() == 1
            && matches!(context.env().resolve_kind(element), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8)
            && matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Str)
        {
            return Ok(coerce::implicit(context, value, ty, span)?);
        }
        values.push((
            index,
            coerce::implicit(context, value, element, value_span)?,
        ));
    }
    let ty = match length {
        Some(_) => ty,
        None => types::array(context.env_mut(), ty, fields.len() as u64, span),
    };

    Ok(emit::aggregate(context, ty, &values, span)?)
}

fn initialize_scalar(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    ty: HMIRTypeID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<HMIRValue> {
    match fields {
        [(None, value)] => field_value(context, body, *value, ty),
        [(Some(name), _)] => Err(hmir_error(
            span,
            &typecheck::UNKNOWN_MEMBER,
            (types::name(context.env(), ty), name.to_string()),
        )
        .into()),
        _ => Err(hmir_error(
            span,
            &typecheck::LIST_INITIALIZATION,
            (types::name(context.env(), ty)).into(),
        )
        .into()),
    }
}

fn field_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: HMIRExprID,
    ty: HMIRTypeID,
) -> EvalResult<HMIRValue> {
    let span = body.expr(value).span();
    let value = elaborate(context, body, value)?.complete(context, body, Some(ty))?;

    Ok(coerce::implicit(context, value, ty, span)?)
}
