use cx_hmir::{HMIRBody, ty::HMIRTypeID};
use cx_log::CXResult;

use crate::{
    env::function::{Expectations, LocalLoweringContext},
    eval::deduce::Incomplete,
    value::HMIRValue,
};

pub(crate) type EvalResult<T> = CXResult<Evaluation<T>>;

pub(crate) enum Evaluation<T> {
    Ready(T),
    Incomplete(Incomplete<T>),
    Stop(StopReason),
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

macro_rules! ready {
    ($result:expr) => {
        match $result? {
            $crate::eval::flow::Evaluation::Ready(value) => value,
            $crate::eval::flow::Evaluation::Incomplete(incomplete) => {
                return Err(incomplete.error());
            }
            $crate::eval::flow::Evaluation::Stop(stop) => {
                return Ok($crate::eval::flow::Evaluation::Stop(stop));
            }
        }
    };
}

pub(crate) use ready;

impl<T> Evaluation<T> {
    pub(crate) fn diverged() -> Self {
        Self::Stop(RuntimeStop::Diverged.into())
    }

    pub(crate) fn is_diverged(&self) -> bool {
        matches!(self, Self::Stop(StopReason::Runtime(RuntimeStop::Diverged)))
    }

    pub(crate) fn known(&self) -> Option<&T> {
        match self {
            Self::Ready(value) => Some(value),
            Self::Incomplete(incomplete) => incomplete.partial(),
            Self::Stop(_) => None,
        }
    }
}

impl<T: 'static> Evaluation<T> {
    pub(crate) fn complete(
        self,
        context: &mut LocalLoweringContext,
        body: &HMIRBody,
        expected: Option<HMIRTypeID>,
    ) -> EvalResult<T> {
        match (self, expected) {
            (Self::Incomplete(incomplete), Some(expected)) => {
                incomplete.resolve(context, body, expected)
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
    ) -> EvalResult<U> {
        match self {
            Self::Ready(value) => Ok(Evaluation::Ready(map(context, value)?)),
            Self::Stop(stop) => Ok(Evaluation::Stop(stop)),
            Self::Incomplete(incomplete) => Ok(Evaluation::Incomplete(incomplete.wrap(
                None,
                move |context, body, expected, resolve| resolve(context, body, expected)?.map(context, map),
            ))),
        }
    }

    pub(crate) fn catch(
        self,
        handler: impl FnOnce(StopReason) -> EvalResult<T> + 'static,
    ) -> EvalResult<T> {
        match self {
            Self::Stop(stop) => handler(stop),
            Self::Incomplete(incomplete) => Ok(Self::Incomplete(incomplete.settle(
                move |_, _, result| result?.catch(handler),
            ))),
            value => Ok(value),
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
    pub(crate) fn description(self) -> EvalResult<HMIRTypeID> {
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
    eval: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<T>,
) -> EvalResult<T> {
    let result = context.with_expectations(expectations, eval)?;
    Ok(result.scoped(expectations))
}

pub(crate) fn catch_diverged<T: 'static>(result: EvalResult<T>) -> EvalResult<Option<T>> {
    let value = match result? {
        Evaluation::Ready(value) => Evaluation::Ready(Some(value)),
        Evaluation::Incomplete(incomplete) => Evaluation::Incomplete(incomplete.project(Some)),
        Evaluation::Stop(stop) => Evaluation::Stop(stop),
    };
    value.catch(|stop| match stop {
        StopReason::Runtime(RuntimeStop::Diverged) => Ok(Evaluation::Ready(None)),
        stop => Ok(Evaluation::Stop(stop)),
    })
}

pub(crate) fn finally<T: 'static>(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    result: EvalResult<T>,
    finish: impl FnOnce(&mut LocalLoweringContext, &HMIRBody, bool) -> CXResult<()> + 'static,
) -> EvalResult<T> {
    match result {
        Ok(Evaluation::Incomplete(incomplete)) => {
            Ok(Evaluation::Incomplete(incomplete.finally(finish)))
        }
        Ok(result) => {
            finish(context, body, matches!(result, Evaluation::Ready(_)))?;
            Ok(result)
        }
        Err(error) => {
            finish(context, body, false)?;
            Err(error)
        }
    }
}

pub(crate) fn top<T>(result: EvalResult<T>) -> CXResult<T> {
    match result? {
        Evaluation::Ready(value) => Ok(value),
        Evaluation::Incomplete(incomplete) => Err(incomplete.error()),
        Evaluation::Stop(StopReason::Comptime(stop)) => {
            unreachable!("uncaught comptime stop: {stop:?}")
        }
        Evaluation::Stop(StopReason::Runtime(stop)) => {
            unreachable!("uncaught runtime stop: {stop:?}")
        }
    }
}

impl From<ComptimeStop> for StopReason {
    fn from(stop: ComptimeStop) -> Self {
        Self::Comptime(stop)
    }
}

impl From<RuntimeStop> for StopReason {
    fn from(stop: RuntimeStop) -> Self {
        Self::Runtime(stop)
    }
}
