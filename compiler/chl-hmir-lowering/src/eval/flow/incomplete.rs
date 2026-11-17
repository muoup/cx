use chl_hmir::{HMIRBodyContext, HMIRUnitID, body::HMIRBodyID, ty::HMIRTypeID};
use chl_log::{CHLResult, catalogue::{mir, typecheck}, error::CHLError};
use chl_tokens::TokenRange;

use crate::{
    env::function::{EvaluationID, Expectations, FnContext, LocalLoweringContext},
    log::hmir_error,
};

use super::{self as flow, EvalResult, Evaluation, finally};

// Completes from the type expected, or from none where the evaluation synthesizes its own
type Resolver<T> = dyn FnOnce(
    &mut LocalLoweringContext,
    &HMIRBodyContext,
    Option<HMIRTypeID>,
) -> EvalResult<Evaluation<T>>;

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
    // Whether the partial may stand as the result when nothing completes it
    fallback: bool,
    synthesizes: bool,
    resolve: Box<Resolver<T>>,
}

impl<T: 'static> Incomplete<T> {
    pub(crate) fn new(
        context: &LocalLoweringContext,
        span: &TokenRange,
        reason: IncompleteReason,
        partial: Option<T>,
        resolve: impl FnOnce(
            &mut LocalLoweringContext,
            &HMIRBodyContext,
            HMIRTypeID,
        ) -> EvalResult<Evaluation<T>>
        + 'static,
    ) -> Self {
        Self {
            reason,
            evaluation: context.evaluation(),
            span: span.clone(),
            partial,
            fallback: true,
            synthesizes: false,
            resolve: Box::new(move |context, body, expected| {
                let expected = expected.expect("only a synthesizing evaluation completes without a type");
                resolve(context, body, expected)
            }),
        }
    }

    // An evaluation that is completed from the type expected of it, or else from the type it synthesizes
    pub(crate) fn synthesizing(
        context: &LocalLoweringContext,
        span: &TokenRange,
        resolve: impl FnOnce(
            &mut LocalLoweringContext,
            &HMIRBodyContext,
            Option<HMIRTypeID>,
        ) -> EvalResult<Evaluation<T>>
        + 'static,
    ) -> Self {
        Self {
            reason: IncompleteReason::ExpectedType,
            evaluation: context.evaluation(),
            span: span.clone(),
            partial: None,
            fallback: false,
            synthesizes: true,
            resolve: Box::new(resolve),
        }
    }

    pub(super) fn synthesizes(&self) -> bool {
        self.synthesizes
    }

    pub(super) fn resolve(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBodyContext,
        expected: Option<HMIRTypeID>,
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
        wrap: impl FnOnce(
            &mut LocalLoweringContext,
            &HMIRBodyContext,
            Option<HMIRTypeID>,
            Box<Resolver<T>>,
        ) -> EvalResult<Evaluation<U>>
        + 'static,
    ) -> Incomplete<U> {
        let Self {
            reason,
            evaluation,
            span,
            fallback,
            synthesizes,
            resolve,
            ..
        } = self;
        Incomplete {
            reason,
            evaluation,
            span,
            partial,
            fallback,
            synthesizes,
            resolve: Box::new(move |context, body, expected| {
                wrap(context, body, expected, resolve)
            }),
        }
    }

    pub(crate) fn without_fallback(mut self) -> Self {
        self.fallback = false;
        self
    }

    pub(crate) fn yielding(mut self) -> Self {
        self.reason = IncompleteReason::YieldType;
        self
    }

    pub(crate) fn is_yielding(&self) -> bool {
        matches!(self.reason, IncompleteReason::YieldType)
    }

    pub(crate) fn in_source(mut self, source: HMIRUnitID, body: HMIRBodyID) -> Self {
        let partial = self.partial.take();
        self.wrap(partial, move |context, _, expected, resolve| {
            let unit = context.unit(source);
            let source_body = unit.body(body).expect("a suspended source body exists");
            let result =
                context.relocated(source, |context| resolve(context, source_body, expected))?;
            Ok(match result {
                Evaluation::Incomplete(pending) => {
                    Evaluation::Incomplete(pending.in_source(source, body))
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
                Err(flow::EvalExit::Stop(_)) => {
                    Err(hmir_error(&span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into())
                }
                result => result,
            }
        })
    }

    pub(super) fn scoped(mut self, expectations: Expectations) -> Self {
        let partial = self.partial.take();
        self.wrap(partial, move |context, body, expected, resolve| {
            flow::scoped(context, expectations, |context| {
                resolve(context, body, expected)
            })
        })
    }

    pub(crate) fn finally(
        self,
        finish: impl FnOnce(&mut LocalLoweringContext, &HMIRBodyContext, bool) -> CHLResult<()> + 'static,
    ) -> Self {
        self.settle(move |context, body, result| finally(context, body, result, finish))
    }

    pub(crate) fn settle(
        mut self,
        settle: impl FnOnce(
            &mut LocalLoweringContext,
            &HMIRBodyContext,
            EvalResult<Evaluation<T>>,
        ) -> EvalResult<Evaluation<T>>
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

    pub(super) fn fallback(&self) -> Option<&T> {
        self.partial.as_ref().filter(|_| self.fallback)
    }

    pub(crate) fn error(&self) -> CHLError {
        let subject = match self.reason {
            IncompleteReason::ExpectedType => "this type",
            IncompleteReason::ArrayBound => "the array bound",
            IncompleteReason::YieldType => "the yielded type",
        };
        hmir_error(&self.span, &typecheck::CANNOT_INFER, subject.into())
    }
}
