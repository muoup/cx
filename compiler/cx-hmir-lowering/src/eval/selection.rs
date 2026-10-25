use std::collections::HashSet;

use cx_hmir::{
    HMIRBody, HMIRPattern,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        aggregate::pattern,
        coerce, control, defer, eval, eval_comptime,
        flow::{self, ComptimeStop, Evaluation, StopReason, ready},
        literal, storage, types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant, scalar::int},
};

struct Match {
    scope: usize,
    subject: HMIRValue,
    arms: Vec<(HMIRPattern, HMIRExprID)>,
    blocks: Vec<MIRBasicBlockID>,
    exits: Vec<(MIRBasicBlockID, HMIRValue)>,
    span: TokenRange,
}

pub(crate) fn eval_match(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    scrutinee: HMIRExprID,
    local: HMIRLocalID,
    arms: &[(HMIRPattern, HMIRExprID)],
) -> EvalResult<HMIRValue> {
    let subject = ready!(eval(context, body, scrutinee));
    let owned = !context.env().is_reference(subject.ty());
    let ty = context
        .env()
        .reference_inner(subject.ty())
        .unwrap_or(subject.ty());
    if context.env().pointer_inner(ty).is_some() {
        return Err(hmir_error(span, &typecheck::POINTER_PATTERN, ()));
    }
    let variants = match context.env().resolve_kind(ty) {
        HMIRTypeKind::TaggedUnion { fields } => Some(fields.len()),
        HMIRTypeKind::Int(_) => None,
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::TYPE_REQUIREMENT,
                (
                    "match".into(),
                    "an integer or tagged union".into(),
                    Some(types::name(context.env(), ty)),
                ),
            ));
        }
    };
    let scope = emit::open_scope(context, span.clone());
    let (subject, dispatch) = if owned {
        let storage = emit::allocate(context, ty, body.local(local).name().cloned(), span)?;
        storage::initialize(context, &storage, &subject, span)?;
        let reference = types::reference(context.env_mut(), ty, span);
        emit::storage::bind_local(context, local, storage.clone().with_ty(reference));
        (storage, subject)
    } else {
        emit::storage::bind_local(context, local, subject.clone());
        (subject.clone(), subject)
    };
    let dispatch = if variants.is_some() {
        emit::tag(context, &subject, span)?
    } else {
        dispatch
    };
    let dispatch = coerce::arithmetic(context, dispatch, span)?;
    let cases = cases(context, body, ty, dispatch.ty(), variants, arms, span)?;
    let result = match context.context() {
        FnContext::Comptime(_) => {
            let HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) = dispatch.kind() else {
                unreachable!("comptime dispatch is constant")
            };
            let selected = cases
                .iter()
                .position(|case| *case == Some(*value))
                .or_else(|| cases.iter().position(Option::is_none))
                .expect("match coverage was checked");
            context
                .comptime_mut()
                .scopes_mut()
                .control_mut(scope)
                .begin_yield();
            let result = bind_arm(context, body, &subject, &arms[selected].0, span)
                .map_err(Into::into)
                .and_then(|()| eval(context, body, arms[selected].1))
                .and_then(|result| {
                    let span = span.clone();
                    result
                        .and_then(context, body, None, move |context, _, _, _| {
                            Ok(Evaluation::Ready(literal::unit(context, &span)))
                        })?
                        .catch(|stop| match stop {
                            StopReason::Comptime(ComptimeStop::Yield(value)) => {
                                Ok(Evaluation::Ready(value))
                            }
                            stop => Ok(Evaluation::Stop(stop)),
                        })
                });
            result
        }
        FnContext::Runtime(_) => {
            let blocks = arms
                .iter()
                .map(|_| runtime::block(context, "match.arm"))
                .collect::<Vec<_>>();
            let default = match cases.iter().position(Option::is_none) {
                Some(index) => blocks[index],
                None => runtime::block(context, "match.unreachable"),
            };
            let signed = matches!(context.env().resolve_kind(dispatch.ty()), HMIRTypeKind::Int(ty) if ty.signed());
            let dispatch = runtime::lower_value(context, &dispatch, span)?;
            context.runtime_mut().emit(
                MIRInstructionKind::CaseBranch {
                    value: dispatch,
                    signed,
                    cases: cases
                        .iter()
                        .zip(&blocks)
                        .filter_map(|(case, block)| {
                            case.map(|value| (value, MIRBlockTarget::new(*block)))
                        })
                        .collect(),
                    default: Some(MIRBlockTarget::new(default)),
                },
                span.clone(),
            );
            if !cases.iter().any(Option::is_none) {
                runtime::switch_to(context, default);
                context
                    .runtime_mut()
                    .emit(MIRInstructionKind::Unreachable, span.clone());
            }
            context
                .runtime_mut()
                .scopes_mut()
                .control_mut(scope)
                .begin_yield();
            lower_arms(
                context,
                body,
                Match {
                    scope,
                    subject,
                    arms: arms.to_vec(),
                    blocks,
                    exits: Vec::new(),
                    span: span.clone(),
                },
                0,
                None,
            )
        }
    };
    let span = span.clone();
    flow::finally(context, body, result, move |context, _, completed| {
        defer::close(context, completed, &span)
    })
}

fn cases(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: HMIRTypeID,
    dispatch_ty: HMIRTypeID,
    variants: Option<usize>,
    arms: &[(HMIRPattern, HMIRExprID)],
    span: &TokenRange,
) -> CXResult<Vec<Option<i128>>> {
    let mut covered = HashSet::new();
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
                Some(pattern::variant(context, body, ty, pattern, arm_span)?.0 as i128)
            }
            HMIRPattern::Integer(value) if variants.is_none() => Some(*value as i128),
            HMIRPattern::Value(expr) if variants.is_none() => {
                let value = eval_comptime(context, body, *expr, Some(dispatch_ty))?;
                match value.kind() {
                    HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) => Some(*value),
                    _ => return Err(hmir_error(arm_span, &typecheck::VALUE_PATTERN_CONSTANT, ())),
                }
            }
            HMIRPattern::Float(_) => {
                return Err(hmir_error(arm_span, &typecheck::FLOATING_CASE, ()));
            }
            _ => return Err(hmir_error(arm_span, &typecheck::INVALID_PATTERN, ())),
        };
        let value = value.map(|value| match context.env().resolve_kind(dispatch_ty) {
            HMIRTypeKind::Int(ty) => int::normalize(value, *ty),
            _ => unreachable!(),
        });
        if let Some(value) = value {
            if !covered.insert(value) {
                return Err(hmir_error(arm_span, &typecheck::UNREACHABLE_MATCH_ARM, ()));
            }
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
    body: &HMIRBody,
    subject: &HMIRValue,
    pattern: &HMIRPattern,
    span: &TokenRange,
) -> CXResult<()> {
    let owned = !context.env().is_reference(subject.ty());
    let ty = context
        .env()
        .reference_inner(subject.ty())
        .unwrap_or(subject.ty());
    let consumes = match pattern {
        HMIRPattern::Binding(local) => {
            let source = if owned {
                emit::reference(context, subject, span)?
            } else {
                subject.clone()
            };
            pattern::bind(context, body, *local, source, owned, span)?
        }
        HMIRPattern::Variant { inner, .. } => {
            let (index, payload) = pattern::variant(context, body, ty, pattern, span)?;
            match inner.as_deref() {
                Some(HMIRPattern::Binding(local)) => {
                    let reference = pattern::payload(context, subject, index, payload, span)?;
                    pattern::bind(context, body, *local, reference, owned, span)?
                }
                _ => !context.env().is_nodrop(payload),
            }
        }
        _ => false,
    };
    if owned && consumes {
        emit::invalidate(context, subject, MIRInvalidationKind::Move, span);
    }
    Ok(())
}

fn lower_arms(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    mut state: Match,
    index: usize,
    expected: Option<HMIRTypeID>,
) -> EvalResult<HMIRValue> {
    if index == state.arms.len() {
        let yields = context
            .runtime_mut()
            .scopes_mut()
            .control_mut(state.scope)
            .end_yield();
        state.exits.extend(yields);
        let has_void = state
            .exits
            .iter()
            .any(|(_, value)| matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void));
        let has_value = state.exits.iter().any(|(_, value)| {
            !matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void)
        });
        if has_void && has_value {
            return Err(hmir_error(
                &state.span,
                &typecheck::MISSING_YIELD,
                "every match arm".into(),
            ));
        }
        return control::merge(context, state.exits, &state.span);
    }
    runtime::switch_to(context, state.blocks[index]);
    emit::open_scope(context, body.expr(state.arms[index].1).span().clone());
    bind_arm(
        context,
        body,
        &state.subject,
        &state.arms[index].0,
        &state.span,
    )?;
    let result = eval(context, body, state.arms[index].1);
    let span = state.span.clone();
    let source = (!context.env().is_reference(state.subject.ty())).then(|| state.subject.clone());
    let result = flow::finally(context, body, result, move |context, _, completed| {
        defer::close(context, completed, &span)?;
        if let Some(source) = source {
            emit::invalidate(context, &source, MIRInvalidationKind::Drop, &span);
        }
        Ok(())
    });
    let result = flow::catch_diverged(result)?.complete(context, body, expected)?;
    result.and_then(
        context,
        body,
        None,
        move |context, body, value, expected| {
            if value.is_some() {
                state.exits.push((
                    runtime::current_block(context),
                    literal::unit(context, &state.span),
                ));
            }
            lower_arms(context, body, state, index + 1, expected)
        },
    )
}
