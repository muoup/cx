use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRExprKind, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRMoveSemantics, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, comptime, eval, eval_comptime, eval_type,
        flow::{self, Evaluation},
        name, types,
    },
    log::hmir_error,
    symbol,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_type_op(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<HMIRValue> {
    let (HMIRTypeOp::SizeOf(operand) | HMIRTypeOp::AlignOf(operand)) = op else {
        todo!("HMIRTypeOp: {}", op.path())
    };
    let (size, alignment) = match operand_type(context, body, *operand)? {
        Err(length) => (length, 1),
        Ok(ty) => {
            if !context.complete_object(ty) {
                return Err(hmir_error(
                    body.expr(*operand).span(),
                    &typecheck::INCOMPLETE_TYPE,
                    format!("operand of type '{}'", types::name(context.env(), ty)),
                ));
            }
            let mir = types::lower_type(context.env_mut(), ty, span)?;
            let layout = cx_mir::ty::layout::calculate_type_layout(context.env().module(), mir);
            (layout.size(), layout.alignment())
        }
    };
    let width = HMIRIntWidth::pointer_width(context.env().architecture());
    let ty = types::int(context.env_mut(), width, false, span);
    let value = if matches!(op, HMIRTypeOp::SizeOf(_)) {
        size
    } else {
        alignment
    };
    Ok(Evaluation::Ready(HMIRValue::constant(
        ty,
        HMIRConstant::Int {
            value: value as i128,
            ty,
        },
    )))
}

fn operand_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    operand: HMIRExprID,
) -> CXResult<Result<HMIRTypeID, usize>> {
    let span = body.expr(operand).span();
    if let HMIRExprKind::Local(local) = body.expr(operand).kind()
        && let Some(value) = context.get_local(local)
    {
        let ty = value.ty();
        return Ok(Ok(context.env().reference_inner(ty).unwrap_or(ty)));
    }
    let context = &mut comptime::frozen_child(context);
    let def = match body.expr(operand).kind() {
        HMIRExprKind::Def(def) => Some(*def),
        HMIRExprKind::Name { name, tag } => name::resolve(context, span, name, *tag)?,
        HMIRExprKind::Literal(_) => {
            let value = flow::top(eval(context, body, operand))?;
            return Ok(match value.kind() {
                HMIRValueKind::Constant(HMIRConstant::Str(string)) => Err(string.len() + 1),
                _ => Ok(value.ty()),
            });
        }
        HMIRExprKind::Type(_) => None,
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::UNSUPPORTED_FEATURE,
                "the size or alignment of a computed expression".into(),
            ));
        }
    };
    if let Some(def) = def {
        let point = context.visible_through();
        let value = symbol::binding(context.env_mut(), def, point)?;
        if !matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Type(_))) {
            return Ok(Ok(context
                .env()
                .reference_inner(value.ty())
                .unwrap_or(value.ty())));
        }
    }
    flow::top(eval_type(context, body, operand)).map(Ok)
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
            let complete = ty.map(
                context,
                |context, expr| CXResult::Ok(context.deferred_type(*expr)),
                |_, _| unreachable!(),
                |_, _| Ok(move_semantics),
            )?;
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
