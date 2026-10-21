use cx_hmir::{HMIRBody, ty::HMIRTypeID};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
    error::CXError,
};
use cx_tokens::TokenRange;

use crate::{
    env::function::{
        ComptimeContext, FnContext, LocalLoweringContext,
        elaboration::{ElaborationScope, ElaborationScopeID},
    },
    eval::flow::{EvalResult, Evaluation},
    log::hmir_error,
};

type Resolver<T> = dyn FnOnce(&mut LocalLoweringContext, &HMIRBody, HMIRTypeID) -> EvalResult<T>;

pub(crate) enum IncompleteReason {
    ExpectedType,
    ArrayBound,
}

pub(crate) struct Incomplete<T> {
    reason: IncompleteReason,
    partial: Option<T>,
    scope: ElaborationScopeID,
    span: TokenRange,
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
        Self {
            reason,
            partial,
            scope: context.elaboration().id(),
            span: span.clone(),
            resolve: Box::new(resolve),
        }
    }

    pub(crate) fn in_comptime(self, state: ComptimeContext, elaboration: ElaborationScope) -> Self {
        let span = self.span.clone();
        Self {
            reason: self.reason,
            partial: self.partial,
            scope: self.scope,
            span: self.span,
            resolve: Box::new(move |context, body, expected| {
                let mut child = context.child(None, FnContext::Comptime(state));
                child.inherit_elaboration(elaboration);
                let result = (self.resolve)(&mut child, body, expected)?;
                match result {
                    Evaluation::Incomplete(incomplete) => {
                        let elaboration = child.elaboration().clone();
                        let FnContext::Comptime(state) = child.into_context() else {
                            unreachable!()
                        };
                        Ok(Evaluation::Incomplete(
                            incomplete.in_comptime(state, elaboration),
                        ))
                    }
                    Evaluation::Stop(_) => {
                        Err(hmir_error(&span, &mir::COMPTIME_CONTROL_ESCAPE, ()))
                    }
                    result => Ok(result),
                }
            }),
        }
    }

    pub(crate) fn project<U: 'static>(self, project: impl Fn(T) -> U + 'static) -> Incomplete<U> {
        self.project_with(Box::new(project))
    }

    fn project_with<U: 'static>(self, project: Box<dyn Fn(T) -> U>) -> Incomplete<U> {
        let partial = self.partial.map(|value| project(value));
        Incomplete {
            reason: self.reason,
            partial,
            scope: self.scope,
            span: self.span,
            resolve: Box::new(move |context, body, expected| {
                let result = (self.resolve)(context, body, expected)?;
                Ok(match result {
                    Evaluation::Ready(value) => Evaluation::Ready(project(value)),
                    Evaluation::Incomplete(incomplete) => {
                        Evaluation::Incomplete(incomplete.project_with(project))
                    }
                    Evaluation::Stop(stop) => Evaluation::Stop(stop),
                })
            }),
        }
    }
}

impl<T> Incomplete<T> {
    pub(crate) fn error(&self) -> CXError {
        let subject = match self.reason {
            IncompleteReason::ExpectedType => "this type",
            IncompleteReason::ArrayBound => "the array bound",
        };
        hmir_error(&self.span, &typecheck::CANNOT_INFER, subject.into())
    }
}

impl<T: 'static> Evaluation<T> {
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
        match self {
            Self::Incomplete(incomplete) => {
                assert_eq!(
                    incomplete.scope,
                    context.elaboration().id(),
                    "incomplete expression completed outside of its elaboration scope"
                );
                let Some(expected) = expected else {
                    return Err(incomplete.error());
                };
                (incomplete.resolve)(context, body, expected)
            }
            result => Ok(result),
        }
    }

    pub(crate) fn map<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        map: impl Fn(&mut LocalLoweringContext, T) -> CXResult<U> + 'static,
    ) -> EvalResult<U> {
        match self {
            Self::Ready(value) => Ok(Evaluation::Ready(map(context, value)?)),
            Self::Stop(stop) => Ok(Evaluation::Stop(stop)),
            result => result.map_with(context, Box::new(map)),
        }
    }

    fn map_with<U: 'static>(
        self,
        context: &mut LocalLoweringContext,
        map: Box<dyn Fn(&mut LocalLoweringContext, T) -> CXResult<U>>,
    ) -> EvalResult<U> {
        match self {
            Self::Ready(value) => Ok(Evaluation::Ready(map(context, value)?)),
            Self::Stop(stop) => Ok(Evaluation::Stop(stop)),
            Self::Incomplete(incomplete) => {
                let partial = incomplete
                    .partial
                    .map(|value| map(context, value))
                    .transpose()?;
                Ok(Evaluation::Incomplete(Incomplete {
                    reason: incomplete.reason,
                    partial,
                    scope: incomplete.scope,
                    span: incomplete.span,
                    resolve: Box::new(move |context, body, expected| {
                        let result = (incomplete.resolve)(context, body, expected)?;
                        result.map_with(context, map)
                    }),
                }))
            }
        }
    }

    pub(crate) fn finish(
        self,
        context: &mut LocalLoweringContext,
        finish: impl FnOnce(&mut LocalLoweringContext) + 'static,
    ) -> Self {
        match self {
            Self::Incomplete(incomplete) => Self::Incomplete(Incomplete {
                reason: incomplete.reason,
                partial: incomplete.partial,
                scope: incomplete.scope,
                span: incomplete.span,
                resolve: Box::new(move |context, body, expected| {
                    match (incomplete.resolve)(context, body, expected) {
                        Ok(result) => Ok(result.finish(context, finish)),
                        Err(error) => {
                            finish(context);
                            Err(error)
                        }
                    }
                }),
            }),
            result => {
                finish(context);
                result
            }
        }
    }
}

impl Evaluation<HMIRTypeID> {
    pub(crate) fn description(self) -> EvalResult<HMIRTypeID> {
        match self {
            Self::Incomplete(mut incomplete) => match incomplete.partial.take() {
                Some(value) => Ok(Self::Ready(value)),
                None => Err(incomplete.error()),
            },
            result => Ok(result),
        }
    }
}
