use std::{collections::BTreeMap, mem};

use cx_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRFunction,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::{CXResult, catalogue::typecheck, error::CXError};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        call::dependent::Frame,
        deduce::{Incomplete, IncompleteReason, subject},
        eval, eval_type,
        flow::{self, Evaluation},
        staged, symbol,
        types::{self, alike},
    },
    log::hmir_error,
    lower::declaration,
    value::{
        self, HMIRValue, HMIRValueKind,
        immediate::HMIRImmediate,
    },
};

// The type arguments of a call deferred to deduction, by parameter, and the type deduced for each
pub(crate) struct Deduction<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    // Each deferred argument left incomplete, with whether it is the parameter's default
    deferred: BTreeMap<usize, (Incomplete<HMIRValue>, bool)>,
    inferred: BTreeMap<usize, HMIRTypeID>,
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

    pub(crate) fn defer(&mut self, index: usize, pending: Incomplete<HMIRValue>, default: bool) {
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
        self.deferred.keys().copied().find(|hole| references.contains(&params[*hole]))
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
            let actual = self.argument(context, form, actual, span);
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
            self.unify(context, frame, *param.ty(), *declared, span)?;
        }
        self.bind(context, frame, body, span)?;

        let mut types = Vec::with_capacity(params.len());
        for (declared, param) in declared.into_iter().zip(signature.params()) {
            let ty = match declared {
                Some(ty) => ty,
                None => {
                    let evaluated = frame.enter(context, span, |child| {
                        flow::top(eval_type(child, function.body(), *param.ty()).and_then(|ty| ty.description()))
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
        self.unify(context, frame, *signature.return_type(), Some(result), span)?;
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
            let source = match default {
                true => self.function.prototype().defaults(),
                false => body,
            };
            let value = Evaluation::Incomplete(pending).complete(context, source, Some(self.inferred[&index]))?;
            let value = frame.argument(context, index, value, span)?;
            self.bound.push((index, value));
        }
        Ok(())
    }

    fn argument(
        &self,
        context: &mut LocalLoweringContext,
        form: HMIRExprID,
        actual: HMIRTypeID,
        span: &TokenRange,
    ) -> HMIRTypeID {
        match self.function.body().expr(form).kind() {
            HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRExprTypeKind::ReferenceTo { .. }) => {
                actual
            }
            _ => types::qualify::value(context, actual, span),
        }
    }

    fn unify(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        form: HMIRExprID,
        actual: Option<HMIRTypeID>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let function = self.function;
        match function.body().expr(form).kind() {
            HMIRExprKind::Local(local) => {
                match function.params().iter().position(|param| param == local) {
                    Some(index) => self.record(context, index, actual, span),
                    None => Ok(()),
                }
            }
            HMIRExprKind::Call {
                callee,
                args,
                named,
            } => {
                let applied = actual.and_then(|actual| {
                    self.applied(context, frame, *callee, args.len(), named, actual, span)
                });
                let forms = args.iter().chain(named.iter().map(|(_, form)| form));
                for (index, form) in forms.enumerate() {
                    let actual = applied
                        .as_ref()
                        .and_then(|values| values.get(index))
                        .and_then(Option::as_ref)
                        .and_then(value::as_type);
                    self.unify(context, frame, *form, actual, span)?;
                }
                Ok(())
            }
            HMIRExprKind::Type(ty) => self.structure(context, frame, ty, actual, span),
            _ => Ok(()),
        }
    }

    fn structure(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        ty: &'a HMIRExprType,
        actual: Option<HMIRTypeID>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let env = context.env();
        match ty.kind() {
            HMIRExprTypeKind::PointerTo(inner) => {
                let actual = actual.and_then(|actual| match env.resolve_kind(env.referent(actual)) {
                    HMIRTypeKind::Function { .. } => Some(env.referent(actual)),
                    _ => env
                        .pointer_inner(actual)
                        .or_else(|| env.array_inner(actual)),
                });
                self.unify(context, frame, *inner, actual, span)
            }
            HMIRExprTypeKind::ReferenceTo { inner, .. } => {
                let actual = actual.map(|actual| env.referent(actual));
                self.unify(context, frame, *inner, actual, span)
            }
            HMIRExprTypeKind::Array { element, .. } => {
                let actual = actual.and_then(|actual| {
                    env.array_inner(actual)
                        .or_else(|| env.pointer_inner(actual))
                });
                self.unify(context, frame, *element, actual, span)
            }
            HMIRExprTypeKind::Nominal(inner) => {
                let declared = ty.semantics();
                let qualifiers = HMIRTypeSemantics {
                    constant: declared.constant,
                    volatile: declared.volatile,
                    ..Default::default()
                };
                let actual = actual.filter(|_| {
                    !declared.unsafe_move && declared.move_semantics == HMIRDeclaredMove::default()
                });
                let actual =
                    actual.map(|actual| types::qualify::remove(context, actual, qualifiers, span));
                self.unify(context, frame, *inner, actual, span)
            }
            HMIRExprTypeKind::Staged(inner) => {
                let actual = actual.map(|actual| match env.resolve_kind(actual) {
                    HMIRTypeKind::Staged(inner) => *inner,
                    _ => actual,
                });
                self.unify(context, frame, *inner, actual, span)
            }
            HMIRExprTypeKind::Function { signature, .. } => {
                let actual = actual
                    .map(|actual| env.pointer_inner(actual).unwrap_or_else(|| env.referent(actual)))
                    .and_then(|actual| match env.resolve_kind(actual) {
                        HMIRTypeKind::Function { signature: actual, .. }
                            if actual.is_variadic() == signature.is_variadic()
                                && actual.params().len() == signature.params().len() =>
                        {
                            Some(actual.clone())
                        }
                        _ => None,
                    });
                for (index, param) in signature.params().iter().enumerate() {
                    let actual = actual.as_ref().map(|actual| *actual.params()[index].ty());
                    self.unify(context, frame, *param.ty(), actual, span)?;
                }
                let actual = actual.map(|actual| *actual.return_type());
                self.unify(context, frame, *signature.return_type(), actual, span)
            }
            _ => Ok(()),
        }
    }

    // The arguments a type generator named by 'callee' applied to produce 'actual', for each of the positional
    // and then the named arguments of a call to it, bound as the call binds them
    fn applied(
        &self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        callee: HMIRExprID,
        args: usize,
        named: &[(CXIdent, HMIRExprID)],
        actual: HMIRTypeID,
        span: &TokenRange,
    ) -> Option<Vec<Option<HMIRValue>>> {
        let body = self.function.body();
        let value = frame
            .enter(context, span, |child| flow::top(eval(child, body, callee)))
            .ok()?;
        let (callee, curried) = match value.kind() {
            HMIRValueKind::Immediate(HMIRImmediate::Curried { callee, args }) => {
                (callee.as_ref(), args.as_slice())
            }
            _ => (&value, [].as_slice()),
        };
        let def = value::as_function(callee)?;
        let instance = instance(context, def, actual)?;
        let source = context.env().symbols().resolve_definition(def);
        let params = declaration::function(source).prototype().params();
        let mut values = (0..params.len())
            .filter(|index| {
                curried.get(*index).is_none_or(Option::is_none) && params[*index].default().is_none()
            })
            .map(|index| instance.get(index).cloned())
            .take(args)
            .collect::<Vec<_>>();
        values.resize(args, None);
        values.extend(named.iter().map(|(name, _)| {
            (0..params.len())
                .find(|index| params[*index].name() == Some(name))
                .and_then(|index| instance.get(index).cloned())
        }));
        Some(values)
    }

    fn record(
        &mut self,
        context: &LocalLoweringContext,
        index: usize,
        actual: Option<HMIRTypeID>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let unreachable = |actual| {
            matches!(
                context.env().resolve_kind(actual),
                HMIRTypeKind::Unreachable
            )
        };
        if actual.is_some_and(unreachable) {
            return Ok(());
        }
        match (actual, self.inferred.get(&index).copied()) {
            (Some(actual), Some(first)) if !alike(context.env(), first, actual, true) => {
                Err(hmir_error(
                    span,
                    &typecheck::CONFLICTING_DEDUCTIONS,
                    (
                        self.parameter(index),
                        types::name(context.env(), first),
                        types::name(context.env(), actual),
                    ),
                ))
            }
            (Some(actual), None) if self.is_hole(index) => {
                self.inferred.insert(index, actual);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn parameter(&self, index: usize) -> String {
        let local = self.function.params()[index];
        match self.function.body().local(local).name() {
            Some(name) => name.to_string(),
            None => index.to_string(),
        }
    }

    pub(crate) fn undeduced(&self, context: &LocalLoweringContext, index: usize, span: &TokenRange) -> CXError {
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
fn instance(context: &LocalLoweringContext, def: HMIRDefID, ty: HMIRTypeID) -> Option<Vec<HMIRValue>> {
    let ty = context.env().underlying(ty);
    let (origin, args) = context.env().resolver().origin(ty)?;
    (origin == def).then(|| args.to_vec())
}

pub(crate) fn generates_type(function: &HMIRFunction) -> bool {
    let signature = function.signature();
    signature.params().iter().all(|param| param.comptime())
        && matches!(
            function.body().expr(*signature.return_type()).kind(),
            HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRExprTypeKind::Type)
        )
}

// The arguments of a type generator that a call left as holes are those the type expected of the call was
// generated from, when 'def' generated that type
pub(crate) fn generated(
    context: &LocalLoweringContext,
    def: HMIRDefID,
    values: Vec<Option<HMIRValue>>,
    undeduced: CXError,
    span: &TokenRange,
) -> Evaluation<Vec<Option<HMIRValue>>> {
    Evaluation::Incomplete(Incomplete::new(
        context,
        span,
        IncompleteReason::ExpectedType,
        None,
        move |context, _, expected| {
            let expected = context.env().referent(expected);
            let Some(instance) = instance(context, def, expected) else {
                return Err(undeduced.into());
            };
            let applied = values.into_iter().zip(instance);
            Ok(Evaluation::Ready(
                applied.map(|(value, bound)| Some(value.unwrap_or(bound))).collect(),
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
) -> CXResult<Option<HMIRTypeID>> {
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
