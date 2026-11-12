use cx_hmir::{
    HMIRBodyContext,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRFnPrototype, MIRFnSignature, MIRType, MIRTypeKind};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

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
        immediate::HMIRImmediate,
        place::HMIRPlaceBrand,
        staged::{Quote, QuoteRef},
    },
};

use crate::eval::{
    EvalResult, coerce, comptime, eval,
    flow::{EvalExit, Evaluation, RuntimeStop, StopReason},
    types,
};

// The type of an operand evaluated without emitting it; a diverging operand is typed unreachable
pub(crate) fn subject(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let span = body.expr(operand).span();
    typed(context, span, |child| eval(child, body, operand))
}

// The type of what 'evaluate' yields in a detached context that emits nothing
pub(crate) fn typed(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
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
        let ty = match (context.env().resolve_kind(referent), value.kind()) {
            (HMIRTypeKind::Type, _) => {
                let value = coerce::promotion::std_rvalue(context, value, &span)?;
                value::as_type(&value).ok_or_else(|| {
                    hmir_error(
                        &span,
                        &typecheck::EXPECTED_TYPE,
                        "an unknown type value".into(),
                    )
                })?
            }
            (_, HMIRValueKind::Immediate(HMIRImmediate::Address(_)))
                if let Some(string) = value::as_literal(&value) =>
            {
                let element = types::int(context.env_mut(), HMIRIntWidth::I8, false, &span);
                let length = (string.len() + 1) as u64;
                types::realize(
                    context.env_mut(),
                    HMIRTypeKind::Array { element, length },
                    &span,
                )
            }
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
) -> CXResult<LocalLoweringContext<'parent, 'global, 'hmir>> {
    let locals = context.visible_locals();
    let frozen = comptime::inputs(context).into_iter().collect::<Bindings>();
    let void = context
        .env_mut()
        .module_mut()
        .intern_type(MIRType::new(MIRTypeKind::Void));
    let prototype = MIRFnPrototype::new(
        MIRFnSignature::new(Vec::new(), void, false, false),
        LinkageMode::Static,
        CXIdent::new("__cx_unevaluated"),
        None,
    );
    let mut runtime = RuntimeContext::new(prototype, HMIRPlaceBrand::DETACHED);
    runtime.set_emission(emission);
    runtime.freeze(frozen.clone());
    let evaluation = context.evaluation();
    let mut child = context.child(None, FnContext::Runtime(runtime), Some(evaluation));
    for (local, value) in locals {
        let value = match frozen.get(&local) {
            Some(known) if value::as_type(known).is_some() || known == &value => {
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
) -> CXResult<HMIRValue> {
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Quote(node)) => {
            let code = shadowed(context, node.get(), span)?;
            Ok(HMIRValue::new(value.ty(), HMIRImmediate::Quote(QuoteRef::new(code))))
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
) -> CXResult<Quote> {
    let mut captures = Bindings::default();
    for (local, captured) in quote.captures() {
        captures.insert(*local, shadow(context, captured.clone(), span)?);
    }
    Ok(Quote::new(quote.source(), quote.root(), quote.params().to_vec(), captures))
}

// Unevaluated storage standing in for a value of type 'ty' whose contents are unknown
pub(crate) fn placeholder(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if types::comptime_only(context.env(), ty) {
        return Ok(HMIRValue::new(ty, HMIRImmediate::Opaque));
    }
    match context.env().reference_inner(ty) {
        Some(ty) => emit::storage::scratch(context, ty, span),
        None => Ok(HMIRValue::new(ty, emit::runtime::register(context, ty, span)?)),
    }
}
