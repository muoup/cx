mod inference;
mod matching;

pub(crate) use inference::infer;

use std::collections::BTreeMap;

use chl_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRFunction,
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use chl_log::{CHLResult, catalogue::typecheck, error::CHLError};
use chl_tokens::TokenRange;

use crate::{
    env::function::{ComptimeContext, LocalLoweringContext},
    eval::{
        EvalResult,
        call::dependent::Frame,
        coerce, comptime,
        flow::{Evaluation, Incomplete, IncompleteReason},
        staged, symbol,
        types::{self, alike},
    },
    log::hmir_error,
    lower::declaration,
    value::{self, HMIRValue},
};

use super::pattern::Form;

pub(crate) struct Deduction<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    slots: BTreeMap<usize, Slot>,
}

struct Slot {
    pending: Option<Pending>,
    evidence: Option<HMIRValue>,
}

pub(crate) enum Pending {
    Value,
    Argument(Incomplete<HMIRValue>),
    Default(Incomplete<HMIRValue>),
}

impl<'a> Deduction<'a> {
    pub(crate) fn new(function: &'a HMIRFunction, def: HMIRDefID) -> Self {
        Self {
            function,
            def,
            slots: BTreeMap::new(),
        }
    }

    pub(crate) fn defer(&mut self, index: usize, pending: Pending) {
        self.slots.insert(
            index,
            Slot {
                pending: Some(pending),
                evidence: None,
            },
        );
    }

    fn is_open(&self, index: usize) -> bool {
        self.slots
            .get(&index)
            .is_some_and(|slot| slot.pending.is_some())
    }

    fn open(&self) -> impl Iterator<Item = usize> + '_ {
        self.slots
            .iter()
            .filter_map(|(&index, slot)| slot.pending.as_ref().map(|_| index))
    }

    pub(crate) fn first_open(&self) -> Option<usize> {
        self.open().next()
    }
}

pub(crate) fn blocking(deduction: &Deduction, index: usize) -> Option<usize> {
    let mut open = deduction.open().peekable();
    open.peek()?;
    let function = deduction.function;
    let form = *function.signature().params()[index].ty();
    let references = staged::references(function.body(), form);
    open.find(|&hole| references.contains(&function.params()[hole]))
}

fn bind(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    body: &HMIRBodyContext,
    values: &mut [Option<HMIRValue>],
    span: &TokenRange,
) -> EvalResult<()> {
    for (&index, slot) in &mut deduction.slots {
        let Some(evidence) = &slot.evidence else {
            continue;
        };
        let Some(pending) = slot.pending.take() else {
            continue;
        };
        let (pending, source) = match pending {
            Pending::Value => (None, body),
            Pending::Argument(pending) => (Some(pending), body),
            Pending::Default(pending) => (Some(pending), deduction.function.prototype().defaults()),
        };
        let value = match pending {
            None => evidence.clone(),
            Some(pending) => {
                let ty = value::as_type(evidence).expect("a deferred type infers a type");
                Evaluation::Incomplete(pending).complete(context, source, Some(ty))?
            }
        };
        values[index] = Some(frame.argument(context, index, value, span)?);
    }
    Ok(())
}

fn record(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    index: usize,
    actual: HMIRValue,
    span: &TokenRange,
) -> CHLResult<()> {
    let Some(slot) = deduction.slots.get_mut(&index) else {
        return Ok(());
    };
    let actual = match value::as_type(&actual) {
        Some(ty) if context.env().is_unreachable(ty) => {
            return Ok(());
        }
        Some(_) => actual,
        None => {
            let ty = frame.parameter(context, index, span)?;
            frame.enter(context, span, |child| {
                let actual = coerce::implicit::standard(child, actual, ty, span)?;
                comptime::converge(child, actual, span)
            })?
        }
    };
    let Some(first) = &slot.evidence else {
        slot.evidence = Some(actual);
        return Ok(());
    };
    let agrees = match (value::as_type(first), value::as_type(&actual)) {
        (Some(first), Some(actual)) => alike(context.env(), first, actual, true),
        _ => first == &actual,
    };
    if agrees {
        return Ok(());
    }
    let describe = |value: &HMIRValue| {
        value::describe(context.env(), value)
            .unwrap_or_else(|| format!("a value of '{}'", types::name(context.env(), value.ty())))
    };
    Err(hmir_error(
        span,
        &typecheck::CONFLICTING_DEDUCTIONS,
        (
            parameter(deduction.function, index),
            describe(first),
            describe(&actual),
        ),
    ))
}

fn parameter(function: &HMIRFunction, index: usize) -> String {
    let local = function.params()[index];
    function
        .body()
        .local(local)
        .name()
        .map_or_else(|| index.to_string(), ToString::to_string)
}

pub(crate) fn undeduced(
    deduction: &Deduction,
    context: &LocalLoweringContext,
    index: usize,
    span: &TokenRange,
) -> CHLError {
    let subject = format!(
        "comptime argument '{}' of '{}'",
        parameter(deduction.function, index),
        symbol::name(context.env(), deduction.def)
    );
    hmir_error(span, &typecheck::TEMPLATE_DEDUCTION, subject)
}

// The arguments 'def' was applied to when it generated 'ty'.
//
// A generator that yields a type it did not define, such as one another generator defined, is not its origin, so
// 'foo(_)' only deduces from a type that 'foo' was the first to generate.
fn instance(
    context: &LocalLoweringContext,
    def: HMIRDefID,
    ty: HMIRTypeID,
) -> Option<Vec<HMIRValue>> {
    let ty = context.env().underlying(ty);
    let (origin, args) = context.env().resolver().origin(ty)?;
    (origin == def).then(|| args.to_vec())
}

pub(crate) fn generates_type(function: &HMIRFunction) -> bool {
    let signature = function.signature();
    function.returns_type()
        && (function.transparent_body().is_some()
            || signature.params().iter().all(|param| param.comptime()))
}

pub(crate) fn generated(
    deduction: Deduction,
    context: &LocalLoweringContext,
    values: Vec<Option<HMIRValue>>,
    undeduced_error: CHLError,
    span: &TokenRange,
) -> Evaluation<Vec<Option<HMIRValue>>> {
    let Deduction { def, slots, .. } = deduction;
    let span = span.clone();
    Evaluation::Incomplete(Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ExpectedType,
        None,
        move |context, body, expected| {
            let expected = context.env().referent(expected);
            let unit = context.unit(def.brand());
            let function = declaration::function(unit.resolve_def(def).expect("function exists"));
            if let Some(root) = function.structural_body() {
                let mut values = values;
                let mut frame = Frame::new(function, def, ComptimeContext::of(def), false);
                for (index, value) in values.iter().enumerate() {
                    if let Some(value) = value {
                        frame.argument(context, index, value.clone(), &span)?;
                    }
                }
                let mut deduction = Deduction {
                    function,
                    def,
                    slots,
                };
                matching::unify(
                    &mut deduction,
                    context,
                    &mut frame,
                    Form::new(function.body(), root),
                    expected,
                    &span,
                )?;
                bind(
                    &mut deduction,
                    context,
                    &mut frame,
                    body,
                    &mut values,
                    &span,
                )?;
                if let Some(index) = deduction.first_open() {
                    return Err(undeduced(&deduction, context, index, &span).into());
                }
                return Ok(Evaluation::Ready(values));
            }
            let instance = instance(context, def, expected).ok_or(undeduced_error)?;
            let applied = values.into_iter().zip(instance);
            Ok(Evaluation::Ready(
                applied
                    .map(|(value, bound)| Some(value.unwrap_or(bound)))
                    .collect(),
            ))
        },
    ))
}
