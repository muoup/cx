use cx_hmir::{
    HMIRBody, HMIRUnitID, body::HMIRBodyID, ty::HMIRTypeID, unit::def::HMIRDeclarationPoint,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
    error::CXError,
};
use cx_tokens::TokenRange;

use crate::{
    env::function::{EvaluationID, Expectations, FnContext, LocalLoweringContext},
    eval::flow::{EvalResult, Evaluation, StopReason, finally},
    log::hmir_error,
};

type Resolver<T> = dyn FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID) -> EvalResult<T>;

pub(crate) enum IncompleteReason {
    ExpectedType,
    ArrayBound,
    YieldType,
}

struct Suspension {
    reason: IncompleteReason,
    evaluation: EvaluationID,
    span: TokenRange,
}

pub(crate) struct Incomplete<T> {
    suspension: Suspension,
    partial: Option<T>,
    resolve: Box<Resolver<T>>,
}

impl<T: 'static> Incomplete<T> {
    pub(crate) fn new(
        context: &LocalLoweringContext,
        span: &TokenRange,
        reason: IncompleteReason,
        partial: Option<T>,
        resolve: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID) -> EvalResult<T>
        + 'static,
    ) -> Self {
        let suspension = Suspension {
            reason,
            evaluation: context.evaluation(),
            span: span.clone(),
        };
        Self::resume(suspension, partial, resolve)
    }

    fn resume(
        suspension: Suspension,
        partial: Option<T>,
        resolve: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID) -> EvalResult<T>
        + 'static,
    ) -> Self {
        Self {
            suspension,
            partial,
            resolve: Box::new(resolve),
        }
    }

    pub(crate) fn yielding(mut self) -> Self {
        self.suspension.reason = IncompleteReason::YieldType;
        self
    }

    pub(crate) fn is_yielding(&self) -> bool {
        matches!(self.suspension.reason, IncompleteReason::YieldType)
    }

    pub(crate) fn in_source(
        self,
        source: HMIRUnitID,
        point: Option<HMIRDeclarationPoint>,
        body: HMIRBodyID,
    ) -> Self {
        let Self {
            suspension,
            partial,
            resolve,
        } = self;
        Self::resume(suspension, partial, move |context, _, expected| {
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

    pub(crate) fn in_context(self, state: FnContext) -> Self {
        let Self {
            suspension,
            partial,
            resolve,
        } = self;
        let span = suspension.span.clone();
        Self::resume(suspension, partial, move |context, body, expected| {
            let evaluation = context.evaluation();
            let mut child = context.child(None, state, Some(evaluation));
            match resolve(&mut child, body, expected)? {
                Evaluation::Incomplete(incomplete) => Ok(Evaluation::Incomplete(
                    incomplete.in_context(child.into_context()),
                )),
                Evaluation::Stop(_) => Err(hmir_error(&span, &mir::COMPTIME_CONTROL_ESCAPE, ())),
                result => Ok(result),
            }
        })
    }

    fn scoped(self, expectations: Expectations) -> Self {
        let Self {
            suspension,
            partial,
            resolve,
        } = self;
        Self::resume(suspension, partial, move |context, body, expected| {
            let result = context
                .with_expectations(expectations, |context| resolve(context, body, expected))?;
            Ok(result.scoped(expectations))
        })
    }

    pub(crate) fn finally(
        self,
        finish: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, bool) -> CXResult<()> + 'static,
    ) -> Self {
        self.settle(move |context, body, result| finally(context, body, result, finish))
    }

    pub(crate) fn settle(
        self,
        settle: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, EvalResult<T>) -> EvalResult<T>
        + 'static,
    ) -> Self {
        let Self {
            suspension,
            partial,
            resolve,
        } = self;
        Self::resume(suspension, partial, move |context, body, expected| {
            let result = resolve(context, body, expected);
            settle(context, body, result)
        })
    }

    pub(crate) fn project<U: 'static>(self, project: impl Fn(T) -> U + 'static) -> Incomplete<U> {
        self.project_with(Box::new(project))
    }

    fn project_with<U: 'static>(self, project: Box<dyn Fn(T) -> U>) -> Incomplete<U> {
        let Self {
            suspension,
            partial,
            resolve,
        } = self;
        let partial = partial.map(|value| project(value));
        Incomplete::resume(suspension, partial, move |context, body, expected| {
            Ok(match resolve(context, body, expected)? {
                Evaluation::Ready(value) => Evaluation::Ready(project(value)),
                Evaluation::Incomplete(incomplete) => {
                    Evaluation::Incomplete(incomplete.project_with(project))
                }
                Evaluation::Stop(stop) => Evaluation::Stop(stop),
            })
        })
    }
}

impl<T> Incomplete<T> {
    pub(crate) fn error(&self) -> CXError {
        let subject = match self.suspension.reason {
            IncompleteReason::ExpectedType => "this type",
            IncompleteReason::ArrayBound => "the array bound",
            IncompleteReason::YieldType => "the yielded type",
        };
        hmir_error(
            &self.suspension.span,
            &typecheck::CANNOT_INFER,
            subject.into(),
        )
    }
}

pub(crate) fn scoped<T: 'static>(
    context: &mut LocalLoweringContext,
    expectations: Expectations,
    eval: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<T>,
) -> EvalResult<T> {
    let result = context.with_expectations(expectations, eval)?;
    Ok(result.scoped(expectations))
}

impl<T: 'static> Evaluation<T> {
    fn scoped(self, expectations: Expectations) -> Self {
        match self {
            Self::Incomplete(incomplete) => Self::Incomplete(incomplete.scoped(expectations)),
            result => result,
        }
    }

    pub(crate) fn catch(
        self,
        handler: impl FnOnce(StopReason) -> EvalResult<T> + 'static,
    ) -> EvalResult<T> {
        match self {
            Self::Stop(stop) => handler(stop),
            Self::Incomplete(Incomplete {
                suspension,
                partial,
                resolve,
            }) => Ok(Self::Incomplete(Incomplete::resume(
                suspension,
                partial,
                move |context, body, expected| resolve(context, body, expected)?.catch(handler),
            ))),
            value => Ok(value),
        }
    }

    pub(crate) fn known(&self) -> Option<&T> {
        match self {
            Self::Ready(value) => Some(value),
            Self::Incomplete(incomplete) => incomplete.partial.as_ref(),
            Self::Stop(_) => None,
        }
    }

    pub(crate) fn complete(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
    ) -> EvalResult<T> {
        match (self, expected) {
            (Self::Incomplete(incomplete), Some(expected)) => {
                assert_eq!(
                    incomplete.suspension.evaluation,
                    context.evaluation(),
                    "incomplete expression completed outside of its evaluation"
                );
                (incomplete.resolve)(context, body, expected)
            }
            (result, _) => Ok(result),
        }
    }

    pub(crate) fn and_then<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        partial: Option<U>,
        then: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, T, Option<HMIRTypeID>) -> EvalResult<U>
        + 'static,
    ) -> EvalResult<U> {
        self.chain(context, body, None, partial, then)
    }

    fn chain<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
        partial: Option<U>,
        then: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, T, Option<HMIRTypeID>) -> EvalResult<U>
        + 'static,
    ) -> EvalResult<U> {
        match self {
            Self::Ready(value) => then(context, body, value, expected),
            Self::Stop(stop) => Ok(Evaluation::Stop(stop)),
            Self::Incomplete(Incomplete {
                suspension,
                resolve,
                ..
            }) => Ok(Evaluation::Incomplete(Incomplete::resume(
                suspension,
                partial,
                move |context, body, expected| {
                    resolve(context, body, expected)?.chain(
                        context,
                        body,
                        Some(expected),
                        None,
                        then,
                    )
                },
            ))),
        }
    }
}

impl Evaluation<HMIRTypeID> {
    pub(crate) fn description(self) -> EvalResult<HMIRTypeID> {
        match self {
            Self::Incomplete(mut incomplete) => match incomplete.partial.take() {
                Some(value) => Ok(Self::Ready(value)),
                None => Ok(Self::Incomplete(incomplete)),
            },
            result => Ok(result),
        }
    }
}
