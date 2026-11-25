use chl_hmir::{
    HMIRBodyContext,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_mir::{MIRFnPrototype, MIRFnSignature, MIRType, MIRTypeKind};
use chl_tokens::TokenRange;
use chl_util::{identifier::CHLIdent, linkage::LinkageMode};

use crate::{
    emit,
    env::{
        function::{
            FnContext, LocalLoweringContext, RuntimeContext,
            runtime::{Emission, RuntimeScope},
            scope::Bindings,
        },
        resolver::demand,
    },
    log::hmir_error,
    value::{
        self, HMIRValue, HMIRValueKind,
        immediate::HMIRImmValue,
        place::HMIRPlaceBrand,
        staged::Quote,
    },
};

use crate::eval::{
    EvalResult, coerce, comptime, eval,
    flow::{EvalExit, Evaluation, RuntimeStop, StopReason},
    storage, types,
};

// The type of an operand evaluated without emitting it; a diverging operand is typed unreachable
pub(crate) fn subject(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let span = body.expr(operand).span();
    typed(context, span, false, |child| eval(child, body, operand))
}

// The type of what 'evaluate' yields in a detached context that emits nothing; its referent unless taken 'as_is'
pub(crate) fn typed(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    as_is: bool,
    evaluate: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<Evaluation<HMIRValue>>,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let mut child = detached(context, Emission::Unevaluated, false, span)?;
    let result = match evaluate(&mut child) {
        Err(EvalExit::Stop(StopReason::Runtime(RuntimeStop::Diverged))) => {
            return Ok(Evaluation::Ready(types::realize(
                child.env_mut(),
                HMIRTypeKind::Unreachable,
                span,
            )));
        }
        result => result?,
    };
    let span = span.clone();
    let result = result.map(&mut child, move |context, value| {
        let referent = context.env().referent(value.ty());
        let ty = match context.env().resolve_kind(referent) {
            HMIRTypeKind::Type => {
                let value = coerce::promotion::std_rvalue(context, value, &span)?;
                value::as_type(&value).ok_or_else(|| {
                    hmir_error(
                        &span,
                        &typecheck::EXPECTED_TYPE,
                        "an unknown type value".into(),
                    )
                })?
            }
            _ if as_is => value.ty(),
            _ => referent,
        };
        demand::prepare(context.env_mut(), ty)?;
        Ok(ty)
    })?;
    Ok(match result {
        Evaluation::Incomplete(incomplete) => {
            Evaluation::Incomplete(incomplete.in_context(child.into_context()))
        }
        result => result,
    })
}

pub(crate) fn detached<'parent, 'global, 'hmir>(
    context: &'parent mut LocalLoweringContext<'_, 'global, 'hmir>,
    emission: Emission,
    deferred: bool,
    span: &TokenRange,
) -> CHLResult<LocalLoweringContext<'parent, 'global, 'hmir>> {
    let frozen = comptime::inputs(context).into_iter().collect::<Bindings>();
    let locals = context
        .visible_locals()
        .into_iter()
        .map(|(local, value)| {
            let owned = match context.env().reference_inner(value.ty()) {
                Some(ty)
                    if context.is_runtime()
                        && context.env().is_nodrop(ty)
                        && storage::tracked(context, &value, span)?.is_some() =>
                {
                    Some(ty)
                }
                _ => None,
            };
            Ok((local, value, owned))
        })
        .collect::<CHLResult<Vec<_>>>()?;
    let void = context
        .env_mut()
        .module_mut()
        .intern_type(MIRType::new(MIRTypeKind::Void));
    let prototype = MIRFnPrototype::new(
        MIRFnSignature::new(Vec::new(), void, false, false),
        LinkageMode::Static,
        CHLIdent::new("__chl_unevaluated"),
        None,
    );
    let mut runtime = RuntimeContext::new(prototype, HMIRPlaceBrand::DETACHED);
    runtime.set_emission(emission);
    runtime.freeze(frozen.clone());
    let evaluation = context.evaluation();
    let mut child = context.child(None, FnContext::Runtime(runtime), Some(evaluation));
    for (local, value, owned) in locals {
        let value = match (owned, frozen.get(&local)) {
            (Some(inner), _) => {
                emit::storage::allocate(&mut child, inner, None, span)?.with_ty(value.ty())
            }
            (None, Some(known)) if value::as_type(known).is_some() || known == &value => {
                shadow(&mut child, known.clone(), span)?
            }
            _ => placeholder(&mut child, value.ty(), span)?,
        };
        emit::storage::bind_local(&mut child, local, value);
    }
    let scopes = child.runtime_mut().scopes_mut();
    let parent = scopes.top();
    scopes.push(
        RuntimeScope::new(deferred),
        Some(parent),
        Bindings::default(),
    );

    Ok(child)
}

fn shadow(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let ty = value.ty();
    let value = match value::into_application(value) {
        Ok(application) => {
            let application = application.try_map(|operand| shadow(context, operand, span))?;
            return Ok(value::application(ty, application));
        }
        Err(value) => value,
    };
    match value.kind() {
        HMIRValueKind::Quote(node) => {
            let quote = shadowed(context, node.get(), span)?;
            Ok(HMIRValue::new(value.ty(), quote.into_kind()))
        }
        HMIRValueKind::Immediate(_) => Ok(value),
        _ => placeholder(context, value.ty(), span),
    }
}

// 'quote' with what it captures standing in a detached context
pub(crate) fn shadowed(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    span: &TokenRange,
) -> CHLResult<Quote> {
    quote.map_captures(|captured| shadow(context, captured.clone(), span))
}

// Unevaluated storage standing in for a value of type 'ty' whose contents are unknown
pub(crate) fn placeholder(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let inner = context.env().referent(ty);
    if types::comptime_only(context.env(), inner) && !types::runtime_function(context.env(), inner)
    {
        return Ok(HMIRValue::new(ty, HMIRImmValue::Opaque));
    }
    match context.env().reference_inner(ty) {
        Some(ty) => emit::storage::scratch(context, ty, span),
        None => Ok(HMIRValue::new(
            ty,
            emit::runtime::register(context, ty, span)?,
        )),
    }
}
