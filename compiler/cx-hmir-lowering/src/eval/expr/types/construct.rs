use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRMoveSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};

use crate::{
    env::{function::LocalLoweringContext, memo::MemoID, resolver::HMIRResolver},
    eval::{eval_comptime, eval_type, flow, name::type_value, types},
    log::hmir_error,
    value::scalar::int,
};

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
    memo: MemoID,
) -> CXResult<HMIRTypeID> {
    let id = HMIRResolver::type_id(memo);
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
        let value = type_value(context, id, ty.decl_span())?;
        context.env_mut().resolver_mut().publish(memo, value);
    }
    let move_semantics = declared_move(context, body, &ty.semantics().move_semantics)?;
    let complete = match ty.kind() {
        HMIRExprTypeKind::PointerTo(_)
        | HMIRExprTypeKind::ReferenceTo { .. }
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
            let inner = flow::top(eval_type(context, body, *inner).and_then(|ty| ty.description()))?;
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
    types::validate(context.env(), context.env().resolve_type_id(id))?;
    Ok(id)
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
    move_semantics: HMIRMoveSemantics,
) -> CXResult<HMIRType> {
    ty.map(
        context,
        |context, inner| member(context, body, *inner, ty.kind()),
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
            let source = flow::top(eval_type(context, body, *source).and_then(|ty| ty.description()))?;
            Ok(types::owned_semantics(context.env(), source).move_semantics)
        }
    }
}

fn member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    owner: &HMIRExprTypeKind,
) -> CXResult<HMIRTypeID> {
    let ty = flow::top(eval_type(context, body, expr).and_then(|ty| ty.description()))?;
    let (subject, admits_void) = match owner {
        HMIRExprTypeKind::Array { .. } => ("array element", false),
        HMIRExprTypeKind::Struct { .. } | HMIRExprTypeKind::Union { .. } => ("member", false),
        HMIRExprTypeKind::TaggedUnion { .. } => ("member", true),
        _ => return Ok(ty),
    };
    let env = context.env();
    let admitted = (admits_void && env.is_void(ty)) || types::is_complete(env, ty);
    if admitted && context.type_visible(ty) {
        return Ok(ty);
    }
    let subject = format!("{subject} of type '{}'", types::name(context.env(), ty));
    Err(hmir_error(body.expr(expr).span(), &typecheck::INCOMPLETE_TYPE, subject).into())
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    length: HMIRExprID,
) -> CXResult<u64> {
    let span = body.expr(length).span();
    let value = eval_comptime(context, body, length, None)?;
    let (HMIRTypeKind::Int(ty), Some(value)) = (context.env().resolve_kind(value.ty()), value.as_int())
    else {
        let subject = ("array length".into(), "integer".into());
        return Err(hmir_error(span, &mir::EXPECTED_CONSTANT, subject).into());
    };

    let value = int::normalize(value, *ty);
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
