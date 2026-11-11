use cx_hmir::{
    HMIRBodyContext,
    expr::HMIRExprID,
    ty::{
        HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRFieldDef,
        context::HMIRTypeContext, intrinsic::HMIRIntWidth,
    },
};
use cx_intrinsics::AggregateIntrinsic;
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::variant;
use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{EvalResult, coerce, eval, eval_type, flow::Evaluation, types},
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval_initializer(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    ty: HMIRExprID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let ty = eval_type(context, body, ty)?.guide();
    let (span, fields) = (span.clone(), fields.to_vec());
    ty.and_then(context, body, move |context, body, ty| {
        let (span, fields) = (&span, fields.as_slice());
        match context.env().resolve_kind(ty) {
            HMIRTypeKind::Struct { fields: declared } | HMIRTypeKind::Union { fields: declared } => {
                let declared = declared.clone();
                structure(context, body, span, ty, &declared, fields)
            }
            HMIRTypeKind::Array { element, length } => {
                array(context, body, span, ty, *element, Some(*length), fields)
            }
            HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
                array(context, body, span, ty, *element, None, fields)
            }
            HMIRTypeKind::TaggedUnion { .. } => tagged(context, body, span, ty, fields),
            _ => scalar(context, body, span, ty, fields),
        }
    })
}

pub(crate) fn initialize(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let init = AggregateIntrinsic::Init { ty, fields: fields.to_vec() };
    emit::intrinsics::aggregate(context, &init, ty, span)
}

fn structure(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    ty: HMIRTypeID,
    declared: &[HMIRFieldDef<HMIRTypeID>],
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let mut values = Vec::with_capacity(fields.len());
    let mut next = 0;
    for (name, value) in fields {
        let index = match name {
            Some(name) => match declared.iter().position(|field| field.name() == Some(name)) {
                Some(index) => index,
                None => {
                    let member = (types::name(context.env(), ty), name.to_string());
                    return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
                }
            },
            None => next,
        };
        let Some(field) = declared.get(index) else {
            let limit = ("aggregate".into(), Some(declared.len()));
            return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit).into());
        };
        next = index + 1;
        values.push((index, (field_value(context, body, *value, *field.ty()))?.ready()?));
    }
    Ok(Evaluation::Ready(initialize(context, ty, &values, span)?))
}

fn array(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    ty: HMIRTypeID,
    element: HMIRTypeID,
    length: Option<u64>,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
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
        let value = eval(context, body, *value)?.complete(context, body, Some(element))?;
        if fields.len() == 1
            && matches!(context.env().resolve_kind(element), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8)
            && matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Str)
        {
            return Ok(Evaluation::Ready(coerce::implicit::standard(context, value, ty, span)?));
        }
        values.push((index, coerce::implicit::standard(context, value, element, value_span)?));
    }
    let ty = match length {
        Some(_) => ty,
        None => types::array(context.env_mut(), ty, fields.len() as u64, span),
    };
    Ok(Evaluation::Ready(initialize(context, ty, &values, span)?))
}

fn tagged(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    ty: HMIRTypeID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let [(Some(name), value)] = fields else {
        let subject = types::name(context.env(), ty);
        return Err(hmir_error(span, &typecheck::LIST_INITIALIZATION, subject).into());
    };
    let Some(found) = variant::lookup(context.env(), ty, name) else {
        let member = (types::name(context.env(), ty), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
    };
    let payload = (field_value(context, body, *value, found.payload))?.ready()?;
    Ok(Evaluation::Ready(variant::construct(context, ty, found.index, Some(payload), span)?))
}

fn scalar(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    ty: HMIRTypeID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    match fields {
        [(None, value)] => field_value(context, body, *value, ty),
        [(Some(name), _)] => {
            let member = (types::name(context.env(), ty), name.to_string());
            Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into())
        }
        _ => Err(hmir_error(span, &typecheck::LIST_INITIALIZATION, types::name(context.env(), ty)).into()),
    }
}

fn field_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    value: HMIRExprID,
    ty: HMIRTypeID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = body.expr(value).span();
    let value = eval(context, body, value)?.complete(context, body, Some(ty))?;
    Ok(Evaluation::Ready(coerce::implicit::standard(context, value, ty, span)?))
}
