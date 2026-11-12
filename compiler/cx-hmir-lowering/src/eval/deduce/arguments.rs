use std::collections::BTreeMap;

use cx_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRFunction,
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
        call::dependent::Frame,
        deduce::{Incomplete, IncompleteReason, subject},
        eval,
        flow::{self, Evaluation},
        symbol,
        types::{self, alike},
    },
    log::hmir_error,
    lower::declaration,
    value::{
        self, HMIRValue, HMIRValueKind,
        immediate::HMIRImmediate,
    },
};

// The type arguments that a call leaves as holes, by parameter, and the type deduced for each
pub(crate) struct Deduction<'a> {
    function: &'a HMIRFunction,
    def: HMIRDefID,
    holes: Vec<usize>,
    inferred: BTreeMap<usize, HMIRTypeID>,
}

impl<'a> Deduction<'a> {
    // Deduces each of 'holes' from the type of an argument in 'args' whose parameter's type names it.
    //
    // Every argument is unified before any is bound in 'frame'. Deduction used to run as the arguments were
    // bound in order, which also covered:
    //   - a parameter type that only evaluates once an earlier argument is bound, e.g. the callee of 'G(n)(T)'
    //     where 'n' is a parameter;
    //   - a type argument other than a lone '_' that is incomplete, e.g. '[_]int'.
    pub(crate) fn infer(
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        function: &'a HMIRFunction,
        def: HMIRDefID,
        holes: Vec<usize>,
        body: &HMIRBodyContext,
        args: &[Option<HMIRExprID>],
        span: &TokenRange,
    ) -> CXResult<Self> {
        let mut deduction = Self {
            function,
            def,
            holes,
            inferred: BTreeMap::new(),
        };
        if deduction.holes.is_empty() {
            return Ok(deduction);
        }
        for (index, arg) in args.iter().enumerate() {
            let Some(arg) = arg.filter(|_| !deduction.is_hole(index)) else {
                continue;
            };
            let Some(actual) = actual_type(context, body, arg)? else {
                continue;
            };
            let form = *function.signature().params()[index].ty();
            let actual = deduction.argument(context, form, actual, span);
            deduction.unify(context, frame, form, Some(actual), span)?;
        }
        Ok(deduction)
    }

    pub(crate) fn is_hole(&self, index: usize) -> bool {
        self.holes.contains(&index)
    }

    pub(crate) fn inferred(&self, index: usize) -> Option<HMIRTypeID> {
        self.inferred.get(&index).copied()
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
                    Some(index) => self.bind(context, index, actual, span),
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

    fn bind(
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
