mod arguments;
mod subject;

pub(crate) use arguments::{Deduction, actual_type, generated, generates_type};
pub(crate) use subject::{detached, subject};

use cx_hmir::{
    HMIRBody, HMIRUnitID,
    body::HMIRBodyID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::def::HMIRDeclarationPoint,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
    error::CXError,
};
use cx_tokens::TokenRange;

use crate::{
    env::function::{EvaluationID, Expectations, FnContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce,
        flow::{self, Evaluation, finally},
        name::type_value,
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

type Resolver<T> = dyn FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID) -> EvalResult<Evaluation<T>>;

pub(crate) enum IncompleteReason {
    ExpectedType,
    ArrayBound,
    YieldType,
}

// An evaluation suspended until the type expected of it is known, resumed only within the evaluation that made it
pub(crate) struct Incomplete<T> {
    reason: IncompleteReason,
    evaluation: EvaluationID,
    span: TokenRange,
    partial: Option<T>,
    resolve: Box<Resolver<T>>,
}

impl<T: 'static> Incomplete<T> {
    pub(crate) fn new(
        context: &LocalLoweringContext,
        span: &TokenRange,
        reason: IncompleteReason,
        partial: Option<T>,
        resolve: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID) -> EvalResult<Evaluation<T>>
        + 'static,
    ) -> Self {
        Self {
            reason,
            evaluation: context.evaluation(),
            span: span.clone(),
            partial,
            resolve: Box::new(resolve),
        }
    }

    pub(super) fn resolve(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: HMIRTypeID,
    ) -> EvalResult<Evaluation<T>> {
        assert_eq!(
            self.evaluation,
            context.evaluation(),
            "incomplete expression completed outside of its evaluation"
        );
        (self.resolve)(context, body, expected)
    }

    // The same suspension, resumed through 'wrap', which receives the suspended resolver
    pub(super) fn wrap<U: 'static>(
        self,
        partial: Option<U>,
        wrap: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID, Box<Resolver<T>>) -> EvalResult<Evaluation<U>>
        + 'static,
    ) -> Incomplete<U> {
        let Self { reason, evaluation, span, resolve, .. } = self;
        Incomplete {
            reason,
            evaluation,
            span,
            partial,
            resolve: Box::new(move |context, body, expected| wrap(context, body, expected, resolve)),
        }
    }

    pub(crate) fn yielding(mut self) -> Self {
        self.reason = IncompleteReason::YieldType;
        self
    }

    pub(crate) fn is_yielding(&self) -> bool {
        matches!(self.reason, IncompleteReason::YieldType)
    }

    pub(crate) fn in_source(
        mut self,
        source: HMIRUnitID,
        point: Option<HMIRDeclarationPoint>,
        body: HMIRBodyID,
    ) -> Self {
        let partial = self.partial.take();
        self.wrap(partial, move |context, _, expected, resolve| {
            let unit = context.unit(source);
            let source_body = unit.body(body).expect("a suspended source body exists");
            let result = context.relocated(source, point, |context| {
                resolve(context, source_body, expected)
            })?;
            Ok(match result {
                Evaluation::Incomplete(pending) => {
                    Evaluation::Incomplete(pending.in_source(source, point, body))
                }
                result => result,
            })
        })
    }

    pub(crate) fn in_context(mut self, state: FnContext) -> Self {
        let (partial, span) = (self.partial.take(), self.span.clone());
        self.wrap(partial, move |context, body, expected, resolve| {
            let evaluation = context.evaluation();
            let mut child = context.child(None, state, Some(evaluation));
            match resolve(&mut child, body, expected) {
                Ok(Evaluation::Incomplete(incomplete)) => Ok(Evaluation::Incomplete(
                    incomplete.in_context(child.into_context()),
                )),
                Err(flow::EvalExit::Stop(_)) => Err(hmir_error(&span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into()),
                result => result,
            }
        })
    }

    pub(super) fn scoped(mut self, expectations: Expectations) -> Self {
        let partial = self.partial.take();
        self.wrap(partial, move |context, body, expected, resolve| {
            flow::scoped(context, expectations, |context| resolve(context, body, expected))
        })
    }

    pub(crate) fn finally(
        self,
        finish: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, bool) -> CXResult<()> + 'static,
    ) -> Self {
        self.settle(move |context, body, result| finally(context, body, result, finish))
    }

    pub(crate) fn settle(
        mut self,
        settle: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, EvalResult<Evaluation<T>>) -> EvalResult<Evaluation<T>>
        + 'static,
    ) -> Self {
        let partial = self.partial.take();
        self.wrap(partial, move |context, body, expected, resolve| {
            let result = resolve(context, body, expected);
            settle(context, body, result)
        })
    }

    pub(crate) fn project<U: 'static>(self, project: impl Fn(T) -> U + 'static) -> Incomplete<U> {
        self.project_with(Box::new(project))
    }

    fn project_with<U: 'static>(mut self, project: Box<dyn Fn(T) -> U>) -> Incomplete<U> {
        let partial = self.partial.take().map(&project);
        self.wrap(partial, move |context, body, expected, resolve| {
            Ok(match resolve(context, body, expected)? {
                Evaluation::Ready(value) => Evaluation::Ready(project(value)),
                Evaluation::Incomplete(incomplete) => {
                    Evaluation::Incomplete(incomplete.project_with(project))
                }
            })
        })
    }
}

impl<T> Incomplete<T> {
    pub(super) fn partial(&self) -> Option<&T> {
        self.partial.as_ref()
    }

    pub(crate) fn error(&self) -> CXError {
        let subject = match self.reason {
            IncompleteReason::ExpectedType => "this type",
            IncompleteReason::ArrayBound => "the array bound",
            IncompleteReason::YieldType => "the yielded type",
        };
        hmir_error(&self.span, &typecheck::CANNOT_INFER, subject.into())
    }
}

// A type as a value; an array type left without a bound completes it from the array type expected of it
pub(crate) fn type_result(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = type_value(context, ty, span)?;
    if !context.env().resolver().settled(ty) {
        context.force_type(ty, span)?;
    }
    if !matches!(context.env().resolve_kind(ty), HMIRTypeKind::Array { length: None, .. }) {
        return Ok(Evaluation::Ready(value));
    }
    let bound = span.clone();
    let incomplete = Incomplete::new(context, span, IncompleteReason::ArrayBound, Some(value), move |context, _, expected| {
        let span = &bound;
        let env = context.env();
        let (HMIRTypeKind::Array { element: from, length: Some(length) }, HMIRTypeKind::Array { element: to, .. }) =
            (env.resolve_kind(expected), env.resolve_kind(ty))
        else {
            return Err(hmir_error(span, &typecheck::CANNOT_INFER, "the array bound".into()).into());
        };
        if !env.same_type(*from, *to) {
            let requirement = ("array initializer".into(), types::name(env, ty), Some(types::name(env, expected)));
            return Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, requirement).into());
        }
        let length = *length;
        let ty = types::array(context.env_mut(), ty, length, span);
        Ok(Evaluation::Ready(type_constant(context, ty, span)))
    });
    Ok(Evaluation::Incomplete(incomplete))
}

pub(crate) fn hole(context: &mut LocalLoweringContext, span: &TokenRange) -> EvalResult<Evaluation<HMIRValue>> {
    let hole = span.clone();
    let incomplete = Incomplete::new(context, span, IncompleteReason::ExpectedType, None, move |context, _, expected| {
        let expected = context.env().referent(expected);
        Ok(Evaluation::Ready(type_constant(context, expected, &hole)))
    });
    Ok(Evaluation::Incomplete(incomplete))
}

fn type_constant(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> HMIRValue {
    let universe = types::universe(context.env_mut(), span);
    HMIRValue::constant(universe, HMIRConstant::Type(ty))
}

// Completes an evaluation from the type expected of it, unless a type is expected and the evaluation describes one
pub(crate) fn complete(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    evaluation: Evaluation<HMIRValue>,
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    match expected {
        Some(expected) if matches!(context.env().resolve_kind(expected), HMIRTypeKind::Type) => {
            Ok(evaluation)
        }
        _ => evaluation.try_complete(context, body, expected),
    }
}

// The type a value describes, completed from the type expected of it where it is an unbounded array
pub(crate) fn described_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: Evaluation<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let partial = value.known().and_then(HMIRValue::as_type);
    let span = span.clone();
    value.and_then(context, body, partial, move |context, _, value, _| {
        let value = coerce::std_rvalue(context, value, &span)?;
        let Some(ty) = value.as_type() else {
            let found = format!("expression of type '{}'", types::name(context.env(), value.ty()));
            return Err(hmir_error(&span, &typecheck::EXPECTED_TYPE, found).into());
        };
        context.prepare_type(ty, &span)?;
        Ok(match type_result(context, ty, &span)? {
            Evaluation::Ready(_) => Evaluation::Ready(ty),
            Evaluation::Incomplete(incomplete) => Evaluation::Incomplete(
                incomplete.project(|value| value.as_type().expect("type completion returns a type")),
            ),
        })
    })
}
