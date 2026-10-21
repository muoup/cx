use cx_log::CXResult;

use crate::value::HMIRValue;

mod incomplete;

pub(crate) use incomplete::{Incomplete, IncompleteReason};

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

pub(crate) fn catch_diverged<T: 'static>(result: EvalResult<T>) -> EvalResult<Option<T>> {
    match result? {
        Evaluation::Stop(StopReason::Runtime(RuntimeStop::Diverged)) => Ok(Evaluation::Ready(None)),
        Evaluation::Ready(value) => Ok(Evaluation::Ready(Some(value))),
        Evaluation::Incomplete(incomplete) => Ok(Evaluation::Incomplete(incomplete.project(Some))),
        Evaluation::Stop(stop) => Ok(Evaluation::Stop(stop)),
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
