use cx_hir::ast::types::HIRTagKind;
use cx_hmir::{
    HMIRBodyContext,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::{HMIRDeclaredMove, HMIRMoveSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};

use crate::{
    emit::comptime::int, env::{function::LocalLoweringContext, memo::MemoID, resolver::HMIRResolver}, eval::{eval_comptime, eval_type, flow, name::type_value, types}, log::hmir_error,
};

pub(crate) fn eval_declared_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    expr: HMIRExprID,
) -> CXResult<HMIRTypeID> {
    let ty = context.deferred_type(expr);
    context.force_type(ty, body.expr(expr).span())?;
    Ok(ty)
}

pub(crate) fn construct_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: &HMIRExprType,
    memo: MemoID,
) -> CXResult<HMIRTypeID> {
    let id = HMIRResolver::type_id(memo);
    let semantics = ty.semantics().map(|declared| {
        CXResult::Ok(match declared {
            HMIRDeclaredMove::Explicit(semantics) => *semantics,
            HMIRDeclaredMove::CopyTraits(_) => HMIRMoveSemantics::default(),
        })
    })?;
    // A tagged union shares the union tag namespace
    let aggregate = match ty.kind() {
        HMIRExprTypeKind::Struct { .. } => Some(HIRTagKind::Struct),
        HMIRExprTypeKind::Union { .. } | HMIRExprTypeKind::TaggedUnion { .. } => {
            Some(HIRTagKind::Union)
        }
        _ => None,
    };
    if let Some(tag) = aggregate {
        let name = ty.debug_name().map(str::to_owned);
        context
            .env_mut()
            .resolver_mut()
            .forward_type(id, tag, semantics, ty.decl_span(), name);
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
        HMIRExprTypeKind::Array { element, length } => {
            let element = member(context, body, *element, ty.kind())?;
            let kind = match array_length(context, body, *length)? {
                Some(length) => HMIRTypeKind::Array { element, length },
                None => HMIRTypeKind::Undefined(HMIRUndefined::Array { element }),
            };
            array_type(ty, kind, move_semantics)?
        }
        _ => evaluate(context, body, ty, move_semantics)?,
    };
    context.env_mut().resolver_mut().complete_type(id, complete);
    types::validate(context.env(), context.env().resolve_type_id(id))?;
    Ok(id)
}

// An array bounded by '_' has no type of its own, only one an expected array type completes
pub(crate) fn hole_bounded(body: &HMIRBodyContext, ty: &HMIRExprType) -> Option<HMIRExprID> {
    match ty.kind() {
        HMIRExprTypeKind::Array { element, length } if matches!(body.expr(*length).kind(), HMIRExprKind::Hole(_)) => {
            Some(*element)
        }
        _ => None,
    }
}

// The undefined array a hole-bounded array is completed from, apart from the type its expression names
pub(crate) fn undefined_array(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: &HMIRExprType,
    element: HMIRExprID,
) -> CXResult<HMIRTypeID> {
    let move_semantics = declared_move(context, body, &ty.semantics().move_semantics)?;
    let element = member(context, body, element, ty.kind())?;
    let kind = HMIRTypeKind::Undefined(HMIRUndefined::Array { element });
    let undefined = array_type(ty, kind, move_semantics)?;
    types::validate(context.env(), &undefined)?;
    Ok(context.env_mut().resolver_mut().realize_type(undefined))
}

fn array_type(ty: &HMIRExprType, kind: HMIRTypeKind, move_semantics: HMIRMoveSemantics) -> CXResult<HMIRType> {
    Ok(HMIRType::new(
        kind,
        ty.semantics().map(|_| CXResult::Ok(move_semantics))?,
        ty.decl_span().clone(),
        ty.debug_name().map(str::to_owned),
    ))
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: &HMIRExprType,
    move_semantics: HMIRMoveSemantics,
) -> CXResult<HMIRType> {
    ty.map(
        context,
        |context, inner| member(context, body, *inner, ty.kind()),
        |_, _| unreachable!("an array is constructed from its bound"),
        |_, _| Ok(move_semantics),
    )
}

fn declared_move(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
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
    body: &HMIRBodyContext,
    expr: HMIRExprID,
    owner: &HMIRExprTypeKind,
) -> CXResult<HMIRTypeID> {
    let ty = flow::top(eval_type(context, body, expr).and_then(|ty| ty.description()))?;
    let subject = match owner {
        HMIRExprTypeKind::Array { .. } => "array element",
        HMIRExprTypeKind::Struct { .. } | HMIRExprTypeKind::Union { .. } | HMIRExprTypeKind::TaggedUnion { .. } => {
            "member"
        }
        _ => return Ok(ty),
    };
    if types::is_complete(context.env(), ty) && context.type_visible(ty) {
        return Ok(ty);
    }
    let subject = format!("{subject} of type '{}'", types::name(context.env(), ty));
    Err(hmir_error(body.expr(expr).span(), &typecheck::INCOMPLETE_TYPE, subject))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    length: HMIRExprID,
) -> CXResult<Option<u64>> {
    let span = body.expr(length).span();
    if matches!(body.expr(length).kind(), HMIRExprKind::Hole(_)) {
        return Err(hmir_error(span, &typecheck::CANNOT_INFER, "the array bound".into()));
    }
    let value = eval_comptime(context, body, length, None)?;
    if matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void) {
        return Ok(None);
    }
    let (HMIRTypeKind::Int(ty), Some(value)) = (context.env().resolve_kind(value.ty()), value.as_int())
    else {
        let subject = ("array length".into(), "integer".into());
        return Err(hmir_error(span, &mir::EXPECTED_CONSTANT, subject));
    };

    let value = int::normalize(value, *ty);
    let (negative, found) = match int::signed(*ty) {
        true => (value < 0, value.to_string()),
        false => (false, (value as u128).to_string()),
    };
    let requirement = match negative {
        true => "a non-negative value".to_string(),
        false => match u64::try_from(value as u128) {
            Ok(length) => return Ok(Some(length)),
            Err(_) => format!("a value of at most {}", u64::MAX),
        },
    };

    let subject = ("array length".into(), requirement, Some(found));
    Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, subject))
}
