mod offset;

use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRMoveSemantics, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
};
use cx_mir::ty::layout::calculate_type_layout;
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, eval_comptime, eval_type, unevaluated,
        flow::{self, Evaluation},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_type_op(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<HMIRValue> {
    if matches!(op, HMIRTypeOp::OffsetOf { .. }) {
        return offset::eval_offsetof(context, body, span, op);
    }

    let (HMIRTypeOp::SizeOf(operand) | HMIRTypeOp::AlignOf(operand)) = op else {
        todo!("HMIRTypeOp: {}", op.path())
    };
    let ty = unevaluated::subject(context, body, *operand)?;
    let span = span.clone();
    let size = matches!(op, HMIRTypeOp::SizeOf(_));
    ty.and_then(context, body, None, move |context, _, ty, _| {
        if !context.complete_object(ty) {
            return Err(hmir_error(
                &span,
                &typecheck::INCOMPLETE_TYPE,
                format!("operand of type '{}'", types::name(context.env(), ty)),
            ));
        }
        let mir = types::lower_type(context.env_mut(), ty, &span)?;
        let layout = calculate_type_layout(context.env().module(), mir);
        let value = if size {
            layout.size()
        } else {
            layout.alignment()
        };
        let width = HMIRIntWidth::pointer_width(context.env().architecture());
        let ty = types::int(context.env_mut(), width, false, &span);
        Ok(Evaluation::Ready(HMIRValue::constant(
            ty,
            HMIRConstant::Int {
                value: value as i128,
                ty,
            },
        )))
    })
}

pub(crate) fn eval_declared_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> CXResult<HMIRTypeID> {
    let ty = context.deferred_type(expr);
    context.force_type(ty, body.expr(expr).span())?;
    Ok(ty)
}

pub(crate) fn construct_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
    memo: crate::env::memo::MemoID,
) -> CXResult<HMIRTypeID> {
    let id = crate::env::resolver::HMIRResolver::type_id(memo);
    let forward = context.env().resolver().is_incomplete(id)
        && matches!(ty.kind(), HMIRExprTypeKind::Opaque { .. });
    let semantics = ty.semantics().map(|declared| {
        CXResult::Ok(match declared {
            HMIRDeclaredMove::Explicit(semantics) => *semantics,
            HMIRDeclaredMove::CopyTraits(_) => HMIRMoveSemantics::default(),
        })
    })?;
    let aggregate = matches!(
        ty.kind(),
        HMIRExprTypeKind::Struct { .. }
            | HMIRExprTypeKind::Union { .. }
            | HMIRExprTypeKind::TaggedUnion { .. }
    );
    if aggregate {
        let name = ty.debug_name().map(str::to_owned);
        context
            .env_mut()
            .resolver_mut()
            .forward_type(id, semantics, ty.decl_span(), name);
        let value = crate::eval::type_value(context, id, ty.decl_span())?;
        context.env_mut().resolver_mut().publish(memo, value);
    }
    let move_semantics = declared_move(context, body, &ty.semantics().move_semantics)?;
    let complete = match ty.kind() {
        HMIRExprTypeKind::PointerTo(_)
        | HMIRExprTypeKind::ReferenceTo(_)
        | HMIRExprTypeKind::Function(_) => {
            let mut complete = ty.map(
                context,
                |context, expr| CXResult::Ok(context.deferred_type(*expr)),
                |_, _| unreachable!(),
                |_, _| Ok(move_semantics),
            )?;
            if let (HMIRExprTypeKind::Function(source), HMIRTypeKind::Function(signature)) =
                (ty.kind(), complete.kind())
            {
                let signature = types::adjust_signature(context, body, source, signature)?;
                complete = HMIRType::new(
                    HMIRTypeKind::Function(signature),
                    *complete.semantics(),
                    complete.decl_span().clone(),
                    complete.debug_name().map(str::to_owned),
                );
            }
            context.env_mut().resolver_mut().complete_type(id, complete);
            return Ok(id);
        }
        HMIRExprTypeKind::Nominal(inner) => {
            let inner = flow::top(eval_type(context, body, *inner)?.description())?;
            let current = context.env().semantics(inner);
            let semantics = current.apply(semantics);
            HMIRType::new(
                HMIRTypeKind::Nominal(inner),
                semantics,
                ty.decl_span().clone(),
                None,
            )
        }
        _ => evaluate(context, body, ty, move_semantics)?,
    };
    context.env_mut().resolver_mut().complete_type(id, complete);
    if forward {
        context.env_mut().resolver_mut().mark_incomplete(id);
    }
    types::validate::declaration(context.env(), context.env().resolve_type_id(id))?;
    Ok(id)
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
        Some(inline) if !inline.admits(context.env(), ty) || !context.type_visible(ty) => {
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

pub(crate) fn unspecify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    qualifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env();
    let current = env.semantics(ty);
    let semantics = HMIRTypeSemantics {
        constant: current.constant && !qualifiers.constant,
        volatile: current.volatile && !qualifiers.volatile,
        ..current
    };
    if semantics == current {
        return ty;
    }

    let underlying = env.underlying(ty);
    let resolved = env.resolve_type_id(underlying);
    let base = resolved.semantics();
    let kind = match resolved.kind() {
        _ if (!base.constant || semantics.constant) && (!base.volatile || semantics.volatile) => {
            HMIRTypeKind::Nominal(underlying)
        }
        kind @ (HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_)) => {
            kind.clone()
        }
        _ => return ty,
    };
    context.env_mut().resolver_mut().realize_type(HMIRType::new(
        kind,
        semantics,
        span.clone(),
        None,
    ))
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
