use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBody, HMIROp,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
    value::HMIRValue,
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext},
    },
    eval::{
        EvalResult,
        coerce::convert,
        eval, eval_type,
        op::{binary_operands, eval_operand, specify},
        place::dereference,
    },
    log::hmir_error,
};

pub(crate) fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRAggregateOp,
) -> EvalResult<HMIRValue> {
    match op {
        HMIRAggregateOp::Member { base, name } => eval_member(context, body, span, *base, name),
        HMIRAggregateOp::Initialize { ty, fields } => {
            if matches!(body.expr(*ty).kind(), HMIRExprKind::Hole(_)) {
                todo!("initializer list without a declared type");
            }

            let ty = eval_type(context, body, *ty)?;
            initialize(context, body, span, ty, fields)
        }
        HMIRAggregateOp::Index { base, index } => {
            let base = eval_operand(context, body, span, *base)?;
            let index = eval_operand(context, body, span, *index)?;
            let element = binary_operands(context, span, HMIRBinaryOp::Add, base, index)?;

            let element = dereference(context, element, span)?;
            Ok(emit::reference(context, &element, span)?)
        }
        _ => todo!("HMIROp::AggregateOp({})", op.path()),
    }
}

fn eval_member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    base: HMIRExprID,
    name: &CXIdent,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, base)?;
    let mut value = emit::referent(context, value, span)?;
    if context.env().pointer_inner(&value.ty()).is_some() {
        value = dereference(context, value, span)?;
    }
    if matches!(context.context(), FnContext::Comptime(_)) {
        todo!("comptime member access");
    }

    let Some(path) = member_path(context.env(), value.ty(), name) else {
        return Err(hmir_error(
            span,
            &typecheck::UNKNOWN_MEMBER,
            (todo!("type display"), name.to_string()),
        )
        .into());
    };

    let mut value = emit::spill(context, value, span)?;
    for index in path {
        let field = &member_fields(context.env(), value.ty())[index];
        if field.bit_width().is_some() {
            todo!("bitfield member access");
        }

        let mut ty = *field.ty();
        if context
            .env()
            .resolve_type_id(value.ty())
            .semantics()
            .constant
        {
            ty = specify(
                context,
                ty,
                HMIRTypeSemantics {
                    constant: true,
                    ..Default::default()
                },
                span,
            );
        }
        value = emit::field(context, &value, index, ty, span)?;
    }

    Ok(emit::reference(context, &value, span)?)
}

fn member_fields<'a>(env: &'a HMIREnvironment, ty: HMIRTypeID) -> &'a [HMIRFieldDef<HMIRTypeID>] {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => fields,
        _ => &[],
    }
}

// The field indices that lead to a member, which may sit inside of anonymous members
fn member_path(env: &HMIREnvironment, ty: HMIRTypeID, name: &CXIdent) -> Option<Vec<usize>> {
    let fields = member_fields(env, ty);

    fields
        .iter()
        .enumerate()
        .find_map(|(index, field)| match field.name() {
            Some(field_name) if field_name == name => Some(vec![index]),
            Some(_) => None,
            None => {
                let mut path = member_path(env, *field.ty(), name)?;
                path.insert(0, index);
                Some(path)
            }
        })
}

fn initialize(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    ty: HMIRTypeID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
) -> EvalResult<HMIRValue> {
    let declared = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => fields.clone(),
        HMIRTypeKind::Array { .. } => todo!("array initializers"),
        HMIRTypeKind::TaggedUnion { .. } => todo!("tagged union initializers"),
        _ => return initialize_scalar(context, body, span, ty, fields),
    };
    if matches!(context.context(), FnContext::Comptime(_)) {
        todo!("comptime aggregate initialization");
    }

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
                        (todo!("type display"), name.to_string()),
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
            (todo!("type display"), name.to_string()),
        )
        .into()),
        _ => Err(hmir_error(span, &typecheck::LIST_INITIALIZATION, todo!("type display")).into()),
    }
}

fn field_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: HMIRExprID,
    ty: HMIRTypeID,
) -> EvalResult<HMIRValue> {
    let span = body.expr(value).span();
    let value = eval_initializer(context, body, value, ty)?;

    Ok(convert(context, value, ty, span)?)
}

// A list without a declared type takes the type of the field or cast that it is the value of
pub(crate) fn eval_initializer(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: HMIRExprID,
    ty: HMIRTypeID,
) -> EvalResult<HMIRValue> {
    let expr = body.expr(value);

    match expr.kind() {
        HMIRExprKind::Op(HMIROp::AggregateOp(HMIRAggregateOp::Initialize {
            ty: declared,
            fields,
        })) if matches!(body.expr(*declared).kind(), HMIRExprKind::Hole(_)) => {
            initialize(context, body, expr.span(), ty, fields)
        }
        _ => eval(context, body, value),
    }
}
