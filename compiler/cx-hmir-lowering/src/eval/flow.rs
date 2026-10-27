use cx_hmir::{HMIRBody, ty::HMIRTypeID};
use cx_log::{CXResult, error::CXError};

use crate::{
    env::function::{Expectations, LocalLoweringContext},
    eval::deduce::Incomplete,
    value::HMIRValue,
};

pub(crate) type EvalResult<T> = Result<T, EvalExit>;

pub(crate) enum EvalExit {
    Diagnostic(CXError),
    Stop(StopReason),
}

impl From<CXError> for EvalExit {
    fn from(error: CXError) -> Self {
        Self::Diagnostic(error)
    }
}

pub(crate) enum Evaluation<T> {
    Ready(T),
    Incomplete(Incomplete<T>),
}

pub(crate) enum StopReason {
    Comptime(ComptimeStop),
    Runtime(RuntimeStop),
}

#[derive(Debug)]
pub(crate) enum ComptimeStop {
    Return(HMIRValue),
    Yield(HMIRValue),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeStop {
    Diverged,
}

pub(crate) fn diverged<T>() -> EvalResult<Evaluation<T>> {
    Err(EvalExit::Stop(RuntimeStop::Diverged.into()))
}

impl<T> Evaluation<T> {
    pub(crate) fn ready(self) -> EvalResult<T> {
        match self {
            Self::Ready(value) => Ok(value),
            Self::Incomplete(incomplete) => Err(incomplete.error().into()),
        }
    }

    pub(crate) fn known(&self) -> Option<&T> {
        match self {
            Self::Ready(value) => Some(value),
            Self::Incomplete(incomplete) => incomplete.partial(),
        }
    }
}

impl<T: 'static> Evaluation<T> {
    pub(crate) fn try_complete(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
    ) -> EvalResult<Evaluation<T>> {
        match (self, expected) {
            (Self::Incomplete(incomplete), Some(expected)) => incomplete.resolve(context, body, expected),
            (result, _) => Ok(result),
        }
    }

    pub(crate) fn complete(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
    ) -> EvalResult<T> {
        self.try_complete(context, body, expected)?.ready()
    }

    pub(crate) fn and_then<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        partial: Option<U>,
        then: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, T, Option<HMIRTypeID>) -> EvalResult<Evaluation<U>> + 'static,
    ) -> EvalResult<Evaluation<U>> {
        self.chain(context, body, None, partial, then)
    }

    fn chain<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
        partial: Option<U>,
        then: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, T, Option<HMIRTypeID>) -> EvalResult<Evaluation<U>> + 'static,
    ) -> EvalResult<Evaluation<U>> {
        match self {
            Self::Ready(value) => then(context, body, value, expected),
            Self::Incomplete(incomplete) => Ok(Evaluation::Incomplete(incomplete.wrap(
                partial,
                move |context, body, expected, resolve| {
                    resolve(context, body, expected)?.chain(context, body, Some(expected), None, then)
                },
            ))),
        }
    }

    pub(crate) fn map<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        map: impl FnOnce(&mut LocalLoweringContext, T) -> CXResult<U> + 'static,
    ) -> EvalResult<Evaluation<U>> {
        match self {
            Self::Ready(value) => Ok(Evaluation::Ready(map(context, value)?)),
            Self::Incomplete(incomplete) => Ok(Evaluation::Incomplete(incomplete.wrap(
                None,
                move |context, body, expected, resolve| resolve(context, body, expected)?.map(context, map),
            ))),
        }
    }

    fn scoped(self, expectations: Expectations) -> Self {
        match self {
            Self::Incomplete(incomplete) => Self::Incomplete(incomplete.scoped(expectations)),
            result => result,
        }
    }
}

impl Evaluation<HMIRTypeID> {
    pub(crate) fn description(self) -> EvalResult<Evaluation<HMIRTypeID>> {
        Ok(match self {
            Self::Incomplete(incomplete) => match incomplete.partial().copied() {
                Some(value) => Self::Ready(value),
                None => Self::Incomplete(incomplete),
            },
            result => result,
        })
    }
}

pub(crate) fn scoped<T: 'static>(
    context: &mut LocalLoweringContext,
    expectations: Expectations,
    eval: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<Evaluation<T>>,
) -> EvalResult<Evaluation<T>> {
    let result = context.with_expectations(expectations, eval)?;
    Ok(result.scoped(expectations))
}

pub(crate) fn catch<T: 'static>(
    result: EvalResult<Evaluation<T>>,
    handler: impl FnOnce(StopReason) -> EvalResult<Evaluation<T>> + 'static,
) -> EvalResult<Evaluation<T>> {
    match result {
        Err(EvalExit::Stop(stop)) => handler(stop),
        Ok(Evaluation::Incomplete(incomplete)) => Ok(Evaluation::Incomplete(incomplete.settle(
            move |_, _, result| catch(result, handler),
        ))),
        result => result,
    }
}

pub(crate) fn catch_diverged<T: 'static>(result: EvalResult<Evaluation<T>>) -> EvalResult<Evaluation<Option<T>>> {
    let result = result.map(|value| match value {
        Evaluation::Ready(value) => Evaluation::Ready(Some(value)),
        Evaluation::Incomplete(incomplete) => Evaluation::Incomplete(incomplete.project(Some)),
    });
    catch(result, |stop| match stop {
        StopReason::Runtime(RuntimeStop::Diverged) => Ok(Evaluation::Ready(None)),
        stop => Err(EvalExit::Stop(stop)),
    })
}

pub(crate) fn finally<T: 'static>(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    result: EvalResult<Evaluation<T>>,
    finish: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, bool) -> CXResult<()> + 'static,
) -> EvalResult<Evaluation<T>> {
    match result {
        Ok(Evaluation::Incomplete(incomplete)) => Ok(Evaluation::Incomplete(incomplete.finally(finish))),
        Ok(result) => {
            finish(context, body, true)?;
            Ok(result)
        }
        Err(error) => {
            finish(context, body, false)?;
            Err(error)
        }
    }
}

pub(crate) fn top<T>(result: EvalResult<Evaluation<T>>) -> CXResult<T> {
    boundary(result.and_then(Evaluation::ready))
}

pub(crate) fn boundary<T>(result: EvalResult<T>) -> CXResult<T> {
    match result {
        Ok(value) => Ok(value),
        Err(EvalExit::Diagnostic(error)) => Err(error),
        Err(EvalExit::Stop(StopReason::Comptime(stop))) => unreachable!("uncaught comptime stop: {stop:?}"),
        Err(EvalExit::Stop(StopReason::Runtime(stop))) => unreachable!("uncaught runtime stop: {stop:?}"),
    }
}

impl From<ComptimeStop> for StopReason {
    fn from(stop: ComptimeStop) -> Self { Self::Comptime(stop) }
}

impl From<RuntimeStop> for StopReason {
    fn from(stop: RuntimeStop) -> Self { Self::Runtime(stop) }
}
