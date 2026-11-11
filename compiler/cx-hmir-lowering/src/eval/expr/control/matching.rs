use rustc_hash::FxHashSet;

use cx_hmir::{
    HMIRBodyContext, HMIRPattern,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use super::{closing, merge};
use crate::{
    emit::{self, comptime::int},
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        aggregate::{
            pattern,
            variant::{self, Variant},
        },
        coerce, eval, eval_comptime,
        flow::{self, Evaluation},
        types,
    },
    log::hmir_error,
    value::{self, HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

struct Match {
    subject: HMIRValue,
    arms: Vec<(HMIRPattern, HMIRExprID)>,
    blocks: Vec<MIRBasicBlockID>,
    exits: Vec<(MIRBasicBlockID, HMIRValue)>,
    span: TokenRange,
}

pub(crate) fn eval_match(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    scrutinee: HMIRExprID,
    local: HMIRLocalID,
    arms: &[(HMIRPattern, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let subject = eval(context, body, scrutinee)?.ready()?;
    let subject = coerce::collapse(context, subject, span)?;
    let owned = !context.env().is_reference(subject.ty());
    let ty = context.env().referent(subject.ty());
    if context.env().pointer_inner(ty).is_some() {
        return Err(hmir_error(span, &typecheck::POINTER_PATTERN, ()).into());
    }
    let variants = match context.env().resolve_kind(ty) {
        HMIRTypeKind::TaggedUnion { fields } => Some(fields.len()),
        HMIRTypeKind::Int(_) => None,
        _ => {
            let requirement = (
                "match".into(),
                "an integer or tagged union".into(),
                Some(types::name(context.env(), ty)),
            );
            return Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, requirement).into());
        }
    };

    emit::open_scope(context);
    let (subject, dispatch) = if owned {
        let name = body.local(local).name().cloned();
        let storage = emit::storage::allocate(context, ty, name, span)?;
        emit::storage::initialize(context, &storage, Some(&subject), span)?;
        emit::storage::bind_local(context, local, storage.clone());
        (storage.with_ty(ty), subject)
    } else {
        emit::storage::bind_local(context, local, subject.clone());
        (subject.clone(), subject)
    };
    let dispatch = match variants {
        Some(_) => variant::tag(context, &dispatch, span)?,
        None => dispatch,
    };
    let dispatch = coerce::arithmetic(context, dispatch, span)?;
    let cases = cases(context, body, ty, dispatch.ty(), variants, arms, span)?;

    let result = if context.is_runtime() {
        let blocks = arms
            .iter()
            .map(|_| emit::runtime::flow::block(context, "match.arm"))
            .collect::<Vec<_>>();
        let catchall = cases.iter().position(Option::is_none);
        let default = match catchall {
            Some(index) => blocks[index],
            None => emit::runtime::flow::block(context, "match.unreachable"),
        };
        let signed = matches!(context.env().resolve_kind(dispatch.ty()), HMIRTypeKind::Int(ty) if ty.signed());
        let branch = MIRInstructionKind::CaseBranch {
            value: emit::runtime::value(context, &dispatch, span)?,
            signed,
            cases: cases
                .iter()
                .zip(&blocks)
                .filter_map(|(case, block)| case.map(|value| (value, MIRBlockTarget::new(*block))))
                .collect(),
            default: Some(MIRBlockTarget::new(default)),
        };
        context.runtime_mut().emit(branch, span.clone());
        if catchall.is_none() {
            emit::runtime::flow::switch_to(context, default);
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
        }
        let state = Match {
            subject,
            arms: arms.to_vec(),
            blocks,
            exits: Vec::new(),
            span: span.clone(),
        };
        lower_arms(context, body, state, 0, None)
    } else {
        let value = value::as_int(&dispatch).expect("comptime dispatch is constant");
        let selected = cases
            .iter()
            .position(|case| *case == Some(value))
            .or_else(|| cases.iter().position(Option::is_none))
            .expect("match coverage was checked");
        let (pattern, arm) = &arms[selected];
        bind_arm(context, body, &subject, pattern, span)?;
        arm_value(context, body, *arm, span, None)
    };
    closing(context, body, result, span)
}

fn cases(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: HMIRTypeID,
    dispatch_ty: HMIRTypeID,
    variants: Option<usize>,
    arms: &[(HMIRPattern, HMIRExprID)],
    span: &TokenRange,
) -> CXResult<Vec<Option<i128>>> {
    let HMIRTypeKind::Int(integer) = *context.env().resolve_kind(dispatch_ty) else {
        unreachable!("a match dispatches on an integer")
    };
    let mut covered = FxHashSet::default();
    let mut catchall = false;
    let mut cases = Vec::new();
    for (pattern, arm) in arms {
        let arm_span = body.expr(*arm).span();
        if catchall || variants.is_some_and(|count| covered.len() == count) {
            return Err(hmir_error(arm_span, &typecheck::UNREACHABLE_MATCH_ARM, ()));
        }
        let value = match pattern {
            HMIRPattern::Binding(_) | HMIRPattern::Wildcard => {
                catchall = true;
                None
            }
            HMIRPattern::Variant { .. } if variants.is_some() => {
                Some(pattern::variant(context, body, ty, pattern, arm_span)?.index as i128)
            }
            HMIRPattern::Integer(value) if variants.is_none() => Some(*value as i128),
            HMIRPattern::Value(expr) if variants.is_none() => {
                let value = eval_comptime(context, body, *expr, Some(dispatch_ty))?;
                match value.kind() {
                    HMIRValueKind::Immediate(HMIRImmediate::Int(value)) => Some(*value),
                    _ => return Err(hmir_error(arm_span, &typecheck::VALUE_PATTERN_CONSTANT, ())),
                }
            }
            HMIRPattern::Float(_) => {
                return Err(hmir_error(arm_span, &typecheck::FLOATING_CASE, ()));
            }
            _ => return Err(hmir_error(arm_span, &typecheck::INVALID_PATTERN, ())),
        };
        let value = value.map(|value| int::normalize(value, integer));
        if let Some(value) = value
            && !covered.insert(value)
        {
            return Err(hmir_error(arm_span, &typecheck::UNREACHABLE_MATCH_ARM, ()));
        }
        cases.push(value);
    }
    if !catchall && variants.is_none_or(|count| covered.len() != count) {
        let missing = match context.env().resolve_kind(ty) {
            HMIRTypeKind::TaggedUnion { fields } => Some(
                fields
                    .iter()
                    .enumerate()
                    .filter(|(index, _)| !covered.contains(&(*index as i128)))
                    .filter_map(|(_, field)| field.name())
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            _ => None,
        };
        return Err(hmir_error(span, &typecheck::NONEXHAUSTIVE_MATCH, missing));
    }
    Ok(cases)
}

fn bind_arm(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    subject: &HMIRValue,
    pattern: &HMIRPattern,
    span: &TokenRange,
) -> CXResult<()> {
    let owned = !context.env().is_reference(subject.ty());
    let ty = context.env().referent(subject.ty());
    let source = match owned {
        true => subject.clone().with_ty(types::reference(context.env_mut(), ty, None, span)),
        false => subject.clone(),
    };
    let consumes = match pattern {
        HMIRPattern::Binding(local) => pattern::bind(context, body, *local, source, owned, span)?,
        HMIRPattern::Variant { inner, .. } => {
            let Variant { index, payload } = pattern::variant(context, body, ty, pattern, span)?;
            match inner.as_deref() {
                Some(HMIRPattern::Binding(local)) => {
                    let reference = pattern::payload(context, &source, index, payload, span)?;
                    pattern::bind(context, body, *local, reference, owned, span)?
                }
                _ => !context.env().is_nodrop(payload),
            }
        }
        _ => false,
    };
    if owned && consumes {
        emit::storage::invalidate(context, subject, MIRInvalidationKind::Move, span);
    }
    Ok(())
}

fn lower_arms(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    mut state: Match,
    index: usize,
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let Some((pattern, arm)) = state.arms.get(index).cloned() else {
        let env = context.env();
        let valued = state
            .exits
            .iter()
            .find(|(_, value)| !env.is_void(value.ty()));
        if let Some((_, found)) = valued
            && state.exits.iter().any(|(_, value)| env.is_void(value.ty()))
        {
            let mismatch = (
                "match arms".into(),
                "'void'".into(),
                format!("'{}'", types::name(env, found.ty())),
            );
            return Err(hmir_error(&state.span, &typecheck::TYPE_MISMATCH, mismatch).into());
        }
        return merge(context, state.exits, true, &state.span);
    };

    emit::runtime::flow::switch_to(context, state.blocks[index]);
    let declared = context
        .env()
        .is_reference(state.subject.ty())
        .then(|| context.runtime().declared_places());
    emit::open_scope(context);
    bind_arm(context, body, &state.subject, &pattern, &state.span)?;
    let result = arm_value(context, body, arm, &state.span, declared);
    let result = closing(context, body, result, &state.span);
    let source = (!context.env().is_reference(state.subject.ty())).then(|| state.subject.clone());
    let span = state.span.clone();
    let result = flow::finally(context, body, result, move |context, _, _| {
        if let Some(source) = source {
            emit::storage::invalidate(context, &source, MIRInvalidationKind::Drop, &span);
        }
        Ok(())
    });
    let result = flow::catch_diverged(result)?.try_complete(context, body, expected)?;
    result.chain(
        context,
        body,
        None,
        None,
        move |context, body, value, expected| {
            if let Some(value) = value {
                state.exits.push((emit::runtime::flow::current_block(context), value));
            }
            lower_arms(context, body, state, index + 1, expected)
        },
    )
}

// An arm stays a place when it designates one that outlives the match: the subject is borrowed, and the arm
// declared nothing since 'declared' places
fn arm_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    arm: HMIRExprID,
    span: &TokenRange,
    declared: Option<usize>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = span.clone();
    eval(context, body, arm)?.map(context, move |context, value| {
        let outlives = declared
            .is_some_and(|declared| context.runtime().declared_places() == declared);
        if outlives && context.env().is_reference(value.ty()) {
            return Ok(value);
        }
        coerce::promotion::std_rvalue(context, value, &span)
    })
}
