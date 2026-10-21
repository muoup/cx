use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
};
use cx_log::catalogue::{mir, typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, eval_comptime, eval_type, types},
    log::hmir_error,
    value::{HMIRValueKind, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_type_op(
    _context: &mut LocalLoweringContext,
    _body: &HMIRBody,
    _span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<HMIRTypeID> {
    todo!("HMIRTypeOp: {}", op.path())
}

pub(crate) fn eval_declared_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declaration: HMIRExprID,
    ty: &HMIRExprType,
) -> EvalResult<HMIRTypeID> {
    match ty.kind() {
        HMIRExprTypeKind::Nominal(inner) => {
            let inner = eval_type(context, body, *inner)?;
            Ok(specify(context, inner, *ty.semantics(), ty.decl_span()))
        }
        HMIRExprTypeKind::Struct { .. }
        | HMIRExprTypeKind::Union { .. }
        | HMIRExprTypeKind::TaggedUnion { .. } => eval_aggregate(context, body, declaration, ty),
        _ => {
            let ty = evaluate(context, body, ty)?;
            Ok(context.env_mut().resolver_mut().realize_type(ty))
        }
    }
}

// The fields of an aggregate may refer back to it, which finds it incomplete until they are all evaluated
fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declaration: HMIRExprID,
    ty: &HMIRExprType,
) -> EvalResult<HMIRTypeID> {
    if let Some(id) = context.env().resolver().declared_type(declaration) {
        return Ok(id);
    }

    let incomplete = HMIRType::new(
        HMIRTypeKind::Opaque {
            size: 0,
            alignment: 1,
        },
        *ty.semantics(),
        ty.decl_span().clone(),
        ty.debug_name().map(str::to_string),
    );
    let id = context
        .env_mut()
        .resolver_mut()
        .declare_type(declaration, incomplete);
    let complete = evaluate(context, body, ty)?;
    context.env_mut().resolver_mut().complete_type(id, complete);

    Ok(id)
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
) -> EvalResult<HMIRType> {
    let inline = match ty.kind() {
        HMIRExprTypeKind::Array { .. } => Some("array element"),
        HMIRExprTypeKind::Struct { .. }
        | HMIRExprTypeKind::Union { .. }
        | HMIRExprTypeKind::TaggedUnion { .. } => Some("member"),
        _ => None,
    };

    ty.map(
        context,
        |context, inner| member(context, body, *inner, inline),
        |context, length| array_length(context, body, *length),
    )
}

fn member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    inline: Option<&str>,
) -> EvalResult<HMIRTypeID> {
    let ty = eval_type(context, body, expr)?;

    match inline {
        Some(subject) if !types::is_complete(context.env(), ty) => {
            let subject = format!("{subject} of type '{}'", types::name(context.env(), ty));
            Err(hmir_error(body.expr(expr).span(), &typecheck::INCOMPLETE_TYPE, subject).into())
        }
        _ => Ok(ty),
    }
}

// Specifiers only accumulate, so a type that already carries them stands for itself
pub(crate) fn specify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    specifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env_mut();
    let current = env.semantics(ty);
    let semantics = current.apply(specifiers);
    if semantics == current {
        return ty;
    }

    let kind = HMIRTypeKind::Nominal(env.underlying(ty));
    env.resolver_mut().realize_type(HMIRType::new(
        kind,
        semantics,
        span.clone(),
        None
    ))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    length: HMIRExprID,
) -> EvalResult<u64> {
    let span = body.expr(length).span();
    let value = eval_comptime(context, body, length, None)?;
    let (HMIRTypeKind::Int(ty), HMIRValueKind::Constant(HMIRConstant::Int { value, .. })) =
        (context.env().resolve_kind(value.ty()), value.kind())
    else {
        let subject = ("array length".into(), "integer".into());
        return Err(hmir_error(span, &mir::EXPECTED_CONSTANT, subject).into());
    };

    let value = int::normalize(*value, *ty);
    let (negative, found) = match int::signed(*ty) {
        true => (value < 0, value.to_string()),
        false => (false, (value as u128).to_string()),
    };
    let requirement = match negative {
        true => "a non-negative value".to_string(),
        false => match u64::try_from(value as u128) {
            Ok(length) => return Ok(length),
            Err(_) => format!("a value of at most {}", u64::MAX),
        },
    };

    let subject = ("array length".into(), requirement, Some(found));
    Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, subject).into())
}
