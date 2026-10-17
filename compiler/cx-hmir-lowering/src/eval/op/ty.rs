use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, constant::HMIRConstant, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, eval_comptime, eval_type},
    log::hmir_error,
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
    if let Some(field) = incomplete_field(context, &complete) {
        let subject = match field {
            Some(name) => format!("field '{name}'"),
            None => "anonymous field".into(),
        };
        return Err(hmir_error(ty.decl_span(), &typecheck::INCOMPLETE_TYPE, subject).into());
    }
    context.env_mut().resolver_mut().complete_type(id, complete);

    Ok(id)
}

// A field is stored inline, so its type cannot be one still being declared (this one included) or only forward declared
fn incomplete_field(context: &LocalLoweringContext, ty: &HMIRType) -> Option<Option<CXIdent>> {
    let fields = match ty.kind() {
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => fields,
        _ => return None,
    };

    fields
        .iter()
        .find(|field| is_incomplete(context, *field.ty()))
        .map(|field| field.name().cloned())
}

fn is_incomplete(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Opaque { size: 0, .. } => true,
        HMIRTypeKind::Array { element, .. } => is_incomplete(context, *element),
        _ => false,
    }
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
) -> EvalResult<HMIRType> {
    ty.map(
        context,
        |context, inner| eval_type(context, body, *inner),
        |context, length| array_length(context, body, *length),
    )
}

// Specifiers only accumulate, so a type that already carries them stands for itself
pub(crate) fn specify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    specifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env_mut();
    let current = *env.resolve_type_id(ty).semantics();
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
    match eval_comptime(context, body, length)? {
        HMIRConstant::Int { value, .. } if value >= 0 => Ok(value as u64),
        _ => {
            let span = body.expr(length).span();
            let requirement = (
                "array length".into(),
                "a non-negative integer constant".into(),
                None,
            );
            Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, requirement).into())
        }
    }
}
