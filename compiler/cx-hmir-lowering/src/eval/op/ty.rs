use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRMoveSemantics, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{eval_comptime, eval_type, flow, types},
    log::hmir_error,
    value::{HMIRValueKind, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_type_op(
    _context: &mut LocalLoweringContext,
    _body: &HMIRBody,
    _span: &TokenRange,
    op: &HMIRTypeOp,
) -> CXResult<HMIRTypeID> {
    todo!("HMIRTypeOp: {}", op.path())
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
    move_semantics: HMIRMoveSemantics,
) -> CXResult<HMIRType> {
    let inline = match ty.kind() {
        HMIRExprTypeKind::Array { .. } => Some(Inline::Element),
        HMIRExprTypeKind::Struct { .. } | HMIRExprTypeKind::Union { .. } => Some(Inline::Member),
        HMIRExprTypeKind::TaggedUnion { .. } => Some(Inline::Variant),
        _ => None,
    };

    ty.map(
        context,
        |context, inner| member(context, body, *inner, inline),
        |context, length| array_length(context, body, *length),
        |_, _| Ok(move_semantics),
    )
}

fn declared_move(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declared: &HMIRDeclaredMove,
) -> CXResult<HMIRMoveSemantics> {
    match declared {
        HMIRDeclaredMove::Explicit(semantics) => Ok(*semantics),
        HMIRDeclaredMove::CopyTraits(source) => {
            let source = flow::top(eval_type(context, body, *source)?.description())?;
            Ok(types::owned_semantics(context.env(), source).move_semantics)
        }
    }
}

#[derive(Clone, Copy)]
enum Inline {
    Element,
    Member,
    Variant,
}

impl Inline {
    fn subject(self) -> &'static str {
        match self {
            Inline::Element => "array element",
            Inline::Member | Inline::Variant => "member",
        }
    }

    fn admits(self, env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
        let void = matches!(env.resolve_kind(env.underlying(ty)), HMIRTypeKind::Void);
        (void && matches!(self, Inline::Variant)) || types::is_complete(env, ty)
    }
}

fn member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    inline: Option<Inline>,
) -> CXResult<HMIRTypeID> {
    let ty = flow::top(eval_type(context, body, expr)?.description())?;

    match inline {
        Some(inline) if !inline.admits(context.env(), ty) => {
            let subject = inline.subject();
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
    env.resolver_mut()
        .realize_type(HMIRType::new(kind, semantics, span.clone(), None))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    length: HMIRExprID,
) -> CXResult<u64> {
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
