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
    emit::comptime::int,
    env::{function::LocalLoweringContext, resolver::demand},
    eval::{eval_comptime, eval_type, flow, types},
    log::hmir_error,
    value,
};

// A named aggregate is reserved before its members are evaluated, so that they may refer to it; 'reserved' is
// the type already reserved for it
pub(crate) fn construct_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: &HMIRExprType,
    reserved: Option<HMIRTypeID>,
) -> CXResult<HMIRTypeID> {
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
    let named = aggregate
        .filter(|_| reserved.is_some() || ty.debug_name().is_some())
        .map(|tag| {
            let kind = HMIRTypeKind::Undefined(HMIRUndefined::Forward(Some(tag)));
            let name = ty.debug_name().map(str::to_owned);
            let placeholder = HMIRType::new(kind, semantics, ty.decl_span().clone(), name);
            let resolver = context.env_mut().resolver_mut();
            match reserved {
                Some(id) => {
                    resolver.complete_type(id, placeholder);
                    id
                }
                None => resolver.reserve_type(placeholder),
            }
        });
    let move_semantics = declared_move(context, body, &ty.semantics().move_semantics)?;
    let complete = match ty.kind() {
        HMIRExprTypeKind::Function(source) => {
            let complete = evaluate(context, body, ty, move_semantics)?;
            let HMIRTypeKind::Function(signature) = complete.kind() else {
                unreachable!("a function type evaluates to one")
            };
            let signature = types::adjust_signature(context, body, source, signature)?;
            HMIRType::new(
                HMIRTypeKind::Function(signature),
                *complete.semantics(),
                complete.decl_span().clone(),
                complete.debug_name().map(str::to_owned),
            )
        }
        HMIRExprTypeKind::Nominal(inner) => {
            let inner =
                flow::top(eval_type(context, body, *inner).and_then(|ty| ty.description()))?;
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
    types::validate(context.env(), &complete)?;
    let resolver = context.env_mut().resolver_mut();
    Ok(match named {
        Some(id) => {
            resolver.complete_type(id, complete);
            id
        }
        None => resolver.realize_type(complete),
    })
}

// The aggregate 'expr' declares, when it is one
pub(crate) fn aggregate(body: &HMIRBodyContext, expr: HMIRExprID) -> Option<&HMIRExprType> {
    let HMIRExprKind::Type(declared) = body.expr(expr).kind() else {
        return None;
    };
    matches!(
        declared.kind(),
        HMIRExprTypeKind::Struct { .. }
            | HMIRExprTypeKind::Union { .. }
            | HMIRExprTypeKind::TaggedUnion { .. }
            | HMIRExprTypeKind::Undefined(HMIRUndefined::Forward(_))
    )
    .then_some(declared)
}

// An array bounded by '_' has no type of its own, only one an expected array type completes
pub(crate) fn hole_bounded(body: &HMIRBodyContext, ty: &HMIRExprType) -> Option<HMIRExprID> {
    match ty.kind() {
        HMIRExprTypeKind::Array { element, length }
            if matches!(body.expr(*length).kind(), HMIRExprKind::Hole(_)) =>
        {
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

fn array_type(
    ty: &HMIRExprType,
    kind: HMIRTypeKind,
    move_semantics: HMIRMoveSemantics,
) -> CXResult<HMIRType> {
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
            let source =
                flow::top(eval_type(context, body, *source).and_then(|ty| ty.description()))?;
            demand::force(context.env_mut(), source)?;
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
        HMIRExprTypeKind::Struct { .. }
        | HMIRExprTypeKind::Union { .. }
        | HMIRExprTypeKind::TaggedUnion { .. } => "member",
        _ => return Ok(ty),
    };
    if demand::complete(context.env_mut(), ty)? {
        return Ok(ty);
    }
    let subject = format!("{subject} of type '{}'", types::name(context.env(), ty));
    Err(hmir_error(
        body.expr(expr).span(),
        &typecheck::INCOMPLETE_TYPE,
        subject,
    ))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    length: HMIRExprID,
) -> CXResult<Option<u64>> {
    let span = body.expr(length).span();
    if matches!(body.expr(length).kind(), HMIRExprKind::Hole(_)) {
        return Err(hmir_error(
            span,
            &typecheck::CANNOT_INFER,
            "the array bound".into(),
        ));
    }
    let value = eval_comptime(context, body, length, None)?;
    if matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void) {
        return Ok(None);
    }
    let (HMIRTypeKind::Int(ty), Some(value)) =
        (context.env().resolve_kind(value.ty()), value::as_int(&value))
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
