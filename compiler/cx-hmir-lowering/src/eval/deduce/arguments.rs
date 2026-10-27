use std::collections::BTreeMap;

use cx_hmir::{
    HMIRBody, HMIRDefID, HMIRDefKind, HMIRFunction,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::{CXResult, catalogue::typecheck, error::CXError};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        call::{self, Frame}, coerce,
        deduce::{Incomplete, IncompleteReason, subject},
        eval,
        flow::{self, Evaluation},
        types::{self, unspecify},
    },
    log::hmir_error,
    symbol::composite::alike,
    value::{
        HMIRValue, HMIRValueKind,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

// Comptime type arguments whose evaluation waits on the type of a later argument, keyed by parameter index
pub(crate) struct Deduction<'a> {
    function: &'a HMIRFunction,
    name: String,
    pending: BTreeMap<usize, Evaluation<HMIRValue>>,
    inferred: BTreeMap<usize, HMIRTypeID>,
}

pub(crate) struct Deduced {
    pub index: usize,
    pub argument: Evaluation<HMIRValue>,
    pub ty: HMIRTypeID,
}

impl<'a> Deduction<'a> {
    pub(crate) fn new(function: &'a HMIRFunction, name: String) -> Self {
        Self {
            function,
            name,
            pending: BTreeMap::new(),
            inferred: BTreeMap::new(),
        }
    }

    pub(crate) fn needs_actual(&self) -> bool {
        !self.pending.is_empty() || !self.inferred.is_empty()
    }

    pub(crate) fn defer(&mut self, index: usize, argument: Evaluation<HMIRValue>) {
        self.pending.insert(index, argument);
    }

    pub(crate) fn infer(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        index: usize,
        actual: Option<HMIRTypeID>,
        span: &TokenRange,
    ) -> CXResult<Vec<Deduced>> {
        if !self.needs_actual() {
            return Ok(Vec::new());
        }
        let form = *self.function.signature().params()[index].ty();
        let actual = actual.map(|actual| self.argument(context, form, actual, span));
        let mut unresolved = Vec::new();
        self.unify(context, frame, form, actual, &mut unresolved, span)?;

        let resolved = self
            .pending
            .keys()
            .copied()
            .filter(|index| self.inferred.contains_key(index))
            .collect::<Vec<_>>();
        let resolved = resolved
            .into_iter()
            .map(|index| Deduced {
                index,
                argument: self.pending.remove(&index).unwrap(),
                ty: self.inferred[&index],
            })
            .collect();
        match unresolved.into_iter().find(|index| self.pending.contains_key(index)) {
            Some(index) => Err(self.undeduced(index, span)),
            None => Ok(resolved),
        }
    }

    pub(crate) fn finish(&self, span: &TokenRange) -> CXResult<()> {
        match self.pending.keys().next() {
            Some(index) => Err(self.undeduced(*index, span)),
            None => Ok(()),
        }
    }

    fn argument(
        &self,
        context: &mut LocalLoweringContext,
        form: HMIRExprID,
        actual: HMIRTypeID,
        span: &TokenRange,
    ) -> HMIRTypeID {
        match self.function.body().expr(form).kind() {
            HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRExprTypeKind::ReferenceTo { .. }) => actual,
            _ => coerce::value_type(context, actual, span),
        }
    }

    fn unify(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        form: HMIRExprID,
        actual: Option<HMIRTypeID>,
        unresolved: &mut Vec<usize>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let function = self.function;
        match function.body().expr(form).kind() {
            HMIRExprKind::Local(local) => {
                match function.params().iter().position(|param| param == local) {
                    Some(index) => self.bind(context, index, actual, unresolved, span),
                    None => Ok(()),
                }
            }
            HMIRExprKind::Call { callee, args } => {
                self.unify(context, frame, *callee, None, unresolved, span)?;
                let applied = actual.and_then(|actual| self.applied(context, frame, *callee, actual, span));
                for (index, form) in args.iter().enumerate() {
                    let actual = applied
                        .as_ref()
                        .and_then(|values| values.get(index))
                        .and_then(HMIRValue::as_type);
                    self.unify(context, frame, *form, actual, unresolved, span)?;
                }
                Ok(())
            }
            HMIRExprKind::Type(ty) => self.structure(context, frame, ty, actual, unresolved, span),
            _ => Ok(()),
        }
    }

    fn structure(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        ty: &'a HMIRExprType,
        actual: Option<HMIRTypeID>,
        unresolved: &mut Vec<usize>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let env = context.env();
        match ty.kind() {
            HMIRExprTypeKind::PointerTo(inner) => {
                let actual = actual.and_then(|actual| match env.resolve_kind(actual) {
                    HMIRTypeKind::Function(_) => Some(actual),
                    _ => env
                        .pointer_inner(actual)
                        .or_else(|| env.array_inner(actual)),
                });
                self.unify(context, frame, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::ReferenceTo { inner, .. } => {
                let actual = actual.map(|actual| env.referent(actual));
                self.unify(context, frame, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::Array { element, .. } => {
                let actual = actual.and_then(|actual| {
                    env.array_inner(actual)
                        .or_else(|| env.pointer_inner(actual))
                });
                self.unify(context, frame, *element, actual, unresolved, span)
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
                let actual = actual.map(|actual| unspecify(context, actual, qualifiers, span));
                self.unify(context, frame, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::Staged(inner) => {
                let actual = actual.map(|actual| match env.resolve_kind(actual) {
                    HMIRTypeKind::Staged(inner) => *inner,
                    _ => actual,
                });
                self.unify(context, frame, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::Function(signature) => {
                let actual = actual
                    .map(|actual| env.pointer_inner(actual).unwrap_or(actual))
                    .and_then(|actual| match env.resolve_kind(actual) {
                        HMIRTypeKind::Function(actual)
                            if actual.is_variadic() == signature.is_variadic()
                                && actual.params().len() == signature.params().len() =>
                        {
                            Some(actual.clone())
                        }
                        _ => None,
                    });
                for (index, param) in signature.params().iter().enumerate() {
                    let actual = actual.as_ref().map(|actual| *actual.params()[index].ty());
                    self.unify(context, frame, *param.ty(), actual, unresolved, span)?;
                }
                let actual = actual.map(|actual| *actual.return_type());
                self.unify(
                    context,
                    frame,
                    *signature.return_type(),
                    actual,
                    unresolved,
                    span,
                )
            }
            _ => Ok(()),
        }
    }

    // The arguments a type generator named by 'callee' applied to produce 'actual', past those it was curried with
    fn applied(
        &self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        callee: HMIRExprID,
        actual: HMIRTypeID,
        span: &TokenRange,
    ) -> Option<Vec<HMIRValue>> {
        let body = self.function.body();
        let value = frame
            .enter(context, span, |child| flow::top(eval(child, body, callee)))
            .ok()?;
        let (callee, curried) = match value.kind() {
            HMIRValueKind::Constant(HMIRConstant::Curried { callee, args }) => {
                (callee.as_ref(), args.len())
            }
            _ => (&value, 0),
        };
        let HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) = callee.kind() else {
            return None;
        };
        Some(instance(context, *def, actual)?.into_iter().skip(curried).collect())
    }

    fn bind(
        &mut self,
        context: &LocalLoweringContext,
        index: usize,
        actual: Option<HMIRTypeID>,
        unresolved: &mut Vec<usize>,
        span: &TokenRange,
    ) -> CXResult<()> {
        let unreachable = |actual| matches!(context.env().resolve_kind(actual), HMIRTypeKind::Unreachable);
        if actual.is_some_and(unreachable) {
            return Ok(());
        }
        let pending = self.pending.contains_key(&index);
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
            (Some(actual), None) if pending => {
                self.inferred.insert(index, actual);
                Ok(())
            }
            (None, None) if pending => {
                unresolved.push(index);
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

    fn undeduced(&self, index: usize, span: &TokenRange) -> CXError {
        let subject = format!("comptime argument '{}' of '{}'", self.parameter(index), self.name);
        hmir_error(span, &typecheck::TEMPLATE_DEDUCTION, subject)
    }
}

// The arguments 'def' was applied to when one of its type expressions produced 'ty'
fn instance(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    ty: HMIRTypeID,
) -> Option<Vec<HMIRValue>> {
    let ty = context.env().underlying(ty);
    let (expr, bindings) = context.env().resolver().origin(ty)?;
    let bindings = bindings.to_vec();
    let unit = context.env_mut().symbols_mut().owner(def.brand())?;
    let generator = unit
        .resolve_def(def)?
        .declarations()
        .iter()
        .find_map(|declaration| match declaration.kind() {
            Some(HMIRDefKind::Function(function)) if function.body().id() == expr.body() => {
                Some(function)
            }
            _ => None,
        })?;
    generator
        .params()
        .iter()
        .map(|param| {
            bindings
                .iter()
                .find(|(local, _)| local == param)
                .map(|(_, value)| value.clone())
        })
        .collect()
}

pub(crate) fn generates_type(function: &HMIRFunction) -> bool {
    let signature = function.signature();
    signature.params().iter().all(|param| param.comptime())
        && matches!(
            function.body().expr(*signature.return_type()).kind(),
            HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRExprTypeKind::Type)
        )
}

// A type left with holes is the type expected of it, when this generator produced that type
pub(crate) fn generated(
    context: &LocalLoweringContext,
    callee: &HMIRValue,
    values: Vec<Option<HMIRValue>>,
    undeduced: CXError,
    span: &TokenRange,
) -> Evaluation<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) = *callee.kind() else {
        unreachable!("a generated type has a generator")
    };
    let (callee, call_span) = (callee.clone(), span.clone());
    Evaluation::Incomplete(Incomplete::new(context, span, IncompleteReason::ExpectedType, None, move |context, body, expected| {
        let expected = context.env().referent(expected);
        let Some(instance) = instance(context, def, expected) else {
            return Err(undeduced);
        };
        let applied = values
            .into_iter()
            .zip(instance)
            .map(|(value, bound)| value.unwrap_or(bound))
            .collect::<Vec<_>>();
        call::dependent(context, &callee, body, &[], &applied, &call_span)
    }))
}

pub(crate) fn actual_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    mut arg: HMIRExprID,
) -> CXResult<Option<HMIRTypeID>> {
    loop {
        match body.expr(arg).kind() {
            HMIRExprKind::Piped(inner) => arg = *inner,
            HMIRExprKind::Quote { params, body: inner } if params.is_empty() => arg = *inner,
            HMIRExprKind::Hole(_) | HMIRExprKind::Quote { .. } => return Ok(None),
            _ => return Ok(Some(flow::top(subject(context, body, arg))?)),
        }
    }
}
