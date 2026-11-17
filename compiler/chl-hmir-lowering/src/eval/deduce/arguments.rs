mod matching;

use std::{collections::BTreeMap, mem};

use super::pattern::Form;

use chl_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRFunction,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRExprTypeKind, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use chl_log::{CHLResult, catalogue::typecheck, error::CHLError};
use chl_tokens::TokenRange;

use crate::{
    env::function::{ComptimeContext, LocalLoweringContext},
    eval::{
        EvalResult,
        call::dependent::Frame,
        coerce, comptime,
        deduce::{Incomplete, IncompleteReason, subject},
        eval_type,
        flow::{self, Evaluation},
        staged, symbol,
        types::{self, alike},
    },
    log::hmir_error,
    lower::declaration,
    value::{self, HMIRValue},
};

// The comptime arguments of a call deferred to deduction, by parameter, and the value deduced for each
pub(crate) struct Deduction<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    // Each deferred argument, with whether it is the parameter's default. A type is left incomplete, while a
    // value is a bare hole that the value deduced for it replaces
    deferred: BTreeMap<usize, (Option<Incomplete<HMIRValue>>, bool)>,
    inferred: BTreeMap<usize, HMIRValue>,
    bound: Vec<(usize, HMIRValue)>,
}

impl<'a> Deduction<'a> {
    pub(crate) fn new(function: &'a HMIRFunction, def: HMIRDefID) -> Self {
        Self {
            function,
            def,
            deferred: BTreeMap::new(),
            inferred: BTreeMap::new(),
            bound: Vec::new(),
        }
    }

    pub(crate) fn defer(
        &mut self,
        index: usize,
        pending: Option<Incomplete<HMIRValue>>,
        default: bool,
    ) {
        self.deferred.insert(index, (pending, default));
    }

    pub(crate) fn is_hole(&self, index: usize) -> bool {
        self.deferred.contains_key(&index)
    }

    pub(crate) fn first(&self) -> Option<usize> {
        self.deferred.keys().next().copied()
    }

    // The deferred argument that the type of the parameter 'index' depends on
    pub(crate) fn blocking(&self, index: usize) -> Option<usize> {
        let form = *self.function.signature().params()[index].ty();
        let references = staged::references(self.function.body(), form);
        let params = self.function.params();
        self.deferred
            .keys()
            .copied()
            .find(|hole| references.contains(&params[*hole]))
    }

    // Deduces the deferred arguments from the arguments from 'from' on, binding each one deduced in 'frame'.
    //
    // Every argument other than a closure is unified before any is bound. A closure is typed after them, in
    // order, with the parameter types that what is bound by then decides, and its result unified.
    pub(crate) fn infer(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        body: &HMIRBodyContext,
        args: &[Option<HMIRExprID>],
        from: usize,
        span: &TokenRange,
    ) -> EvalResult<Vec<(usize, HMIRValue)>> {
        let mut closures = Vec::new();
        for (index, arg) in args.iter().enumerate().skip(from) {
            let Some(arg) = arg.filter(|_| !self.is_hole(index)) else {
                continue;
            };
            if let Some(closure) = closure(body, arg) {
                closures.push((index, closure));
                continue;
            }
            let Some(actual) = actual_type(context, body, arg)? else {
                continue;
            };
            let form = *self.function.signature().params()[index].ty();
            let form = Form {
                argument: true,
                ..Form::new(self.function.body(), form)
            };
            self.unify(context, frame, form, Some(actual), span)?;
        }
        self.bind(context, frame, body, span)?;
        for (index, (params, root)) in closures {
            self.closure(context, frame, body, index, params, root, span)?;
            self.bind(context, frame, body, span)?;
        }
        Ok(mem::take(&mut self.bound))
    }

    // A closure passed to a parameter of function type: the types it declares are unified with those of the
    // parameter, and the result its body has with the parameter types decided by then with that of the parameter
    fn closure(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        body: &HMIRBodyContext,
        index: usize,
        params: &[HMIRLocalID],
        root: HMIRExprID,
        span: &TokenRange,
    ) -> EvalResult<()> {
        let function = self.function;
        let form = *function.signature().params()[index].ty();
        let HMIRExprKind::Type(ty) = function.body().expr(form).kind() else {
            return Ok(());
        };
        let HMIRExprTypeKind::Function { signature, .. } = ty.kind() else {
            return Ok(());
        };
        if signature.params().len() != params.len() {
            return Ok(());
        }
        let quote = staged::closure(context, body, params, root)?;
        let declared = staged::declared(context, &quote, None)?;
        for (declared, param) in declared.iter().zip(signature.params()) {
            self.unify(
                context,
                frame,
                Form::new(function.body(), *param.ty()),
                *declared,
                span,
            )?;
        }
        self.bind(context, frame, body, span)?;

        let mut types = Vec::with_capacity(params.len());
        for (declared, param) in declared.into_iter().zip(signature.params()) {
            let ty = match declared {
                Some(ty) => ty,
                None => {
                    let evaluated = frame.enter(context, span, |child| {
                        flow::top(
                            eval_type(child, function.body(), *param.ty())
                                .and_then(|ty| ty.description()),
                        )
                    });
                    match evaluated {
                        Ok(ty) => ty,
                        Err(_) => return Ok(()),
                    }
                }
            };
            types.push(ty);
        }
        let result = staged::synthesize(context, &quote, &types, span)?.ready()?;
        let result = staged::lift(context.env_mut(), result, span);
        self.unify(
            context,
            frame,
            Form::new(function.body(), *signature.return_type()),
            Some(result),
            span,
        )?;
        Ok(())
    }

    // Binds each deferred argument deduced so far in 'frame', completed from the type deduced for it
    fn bind(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        body: &HMIRBodyContext,
        span: &TokenRange,
    ) -> EvalResult<()> {
        let ready = self
            .deferred
            .keys()
            .copied()
            .filter(|index| self.inferred.contains_key(index))
            .collect::<Vec<_>>();
        for index in ready {
            let (pending, default) = self.deferred.remove(&index).expect("a deferred argument");
            let inferred = self.inferred[&index].clone();
            let Some(pending) = pending else {
                let value = frame.argument(context, index, inferred, span)?;
                self.bound.push((index, value));
                continue;
            };
            let source = match default {
                true => self.function.prototype().defaults(),
                false => body,
            };
            let inferred = value::as_type(&inferred).expect("a deferred type infers a type");
            let value =
                Evaluation::Incomplete(pending).complete(context, source, Some(inferred))?;
            let value = frame.argument(context, index, value, span)?;
            self.bound.push((index, value));
        }
        Ok(())
    }

    // Records 'actual' as deduced for the parameter 'index'. A value other than a type is compared as the type of
    // its parameter has it, which is known once its argument is deferred
    fn record(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        index: usize,
        actual: Option<HMIRValue>,
        span: &TokenRange,
    ) -> CHLResult<()> {
        let Some(actual) = actual else {
            return Ok(());
        };
        if !self.inferred.contains_key(&index) && !self.is_hole(index) {
            return Ok(());
        }
        let actual = match value::as_type(&actual) {
            Some(ty) if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Unreachable) => {
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
        let Some(first) = self.inferred.get(&index) else {
            self.inferred.insert(index, actual);
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
            value::describe(context.env(), value).unwrap_or_else(|| {
                format!("a value of '{}'", types::name(context.env(), value.ty()))
            })
        };
        Err(hmir_error(
            span,
            &typecheck::CONFLICTING_DEDUCTIONS,
            (self.parameter(index), describe(first), describe(&actual)),
        ))
    }

    fn parameter(&self, index: usize) -> String {
        let local = self.function.params()[index];
        match self.function.body().local(local).name() {
            Some(name) => name.to_string(),
            None => index.to_string(),
        }
    }

    pub(crate) fn undeduced(
        &self,
        context: &LocalLoweringContext,
        index: usize,
        span: &TokenRange,
    ) -> CHLError {
        let subject = format!(
            "comptime argument '{}' of '{}'",
            self.parameter(index),
            symbol::name(context.env(), self.def)
        );
        hmir_error(span, &typecheck::TEMPLATE_DEDUCTION, subject)
    }
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
    context: &LocalLoweringContext,
    def: HMIRDefID,
    values: Vec<Option<HMIRValue>>,
    undeduced: CHLError,
    span: &TokenRange,
) -> Evaluation<Vec<Option<HMIRValue>>> {
    let span = span.clone();
    Evaluation::Incomplete(Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ExpectedType,
        None,
        move |context, _, expected| {
            let expected = context.env().referent(expected);
            let unit = context.unit(def.brand());
            let function = declaration::function(unit.resolve_def(def).expect("function exists"));
            if let Some(root) = function.transparent_body() {
                let mut values = values;
                let mut frame = Frame::new(function, def, ComptimeContext::of(def), false);
                let mut deduction = Deduction::new(function, def);
                for (index, value) in values.iter().enumerate() {
                    match value {
                        Some(value) => {
                            frame.argument(context, index, value.clone(), &span)?;
                        }
                        None => deduction.defer(index, None, false),
                    }
                }
                deduction.unify(
                    context,
                    &mut frame,
                    Form::new(function.body(), root),
                    Some(expected),
                    &span,
                )?;
                for (index, value) in values.iter_mut().enumerate() {
                    if value.is_none() {
                        let Some(inferred) = deduction.inferred.remove(&index) else {
                            return Err(deduction.undeduced(context, index, &span).into());
                        };
                        *value = Some(inferred);
                    }
                }
                return Ok(Evaluation::Ready(values));
            }
            let Some(instance) = instance(context, def, expected) else {
                return Err(undeduced.into());
            };
            let applied = values.into_iter().zip(instance);
            Ok(Evaluation::Ready(
                applied
                    .map(|(value, bound)| Some(value.unwrap_or(bound)))
                    .collect(),
            ))
        },
    ))
}

// The parameters and body of a closure that takes parameters
fn closure(body: &HMIRBodyContext, mut arg: HMIRExprID) -> Option<(&[HMIRLocalID], HMIRExprID)> {
    while let HMIRExprKind::Piped(inner) = body.expr(arg).kind() {
        arg = *inner;
    }
    match body.expr(arg).kind() {
        HMIRExprKind::Quote { params, body } if !params.is_empty() => Some((params, *body)),
        _ => None,
    }
}

pub(crate) fn actual_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    mut arg: HMIRExprID,
) -> CHLResult<Option<HMIRTypeID>> {
    loop {
        match body.expr(arg).kind() {
            HMIRExprKind::Piped(inner) => arg = *inner,
            HMIRExprKind::Quote {
                params,
                body: inner,
            } if params.is_empty() => arg = *inner,
            HMIRExprKind::Hole(_) | HMIRExprKind::Quote { .. } => return Ok(None),
            _ => return Ok(Some(flow::top(subject(context, body, arg))?)),
        }
    }
}
