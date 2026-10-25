use std::iter;

use cx_hmir::{
    HMIRFunction,
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
    eval::{coerce, flow::Evaluation, op::unspecify, types},
    log::hmir_error,
    symbol::composite::alike,
    value::HMIRValue,
};

pub struct Deduction<'a> {
    function: &'a HMIRFunction,
    name: String,
    pending: Vec<(usize, Evaluation<HMIRValue>)>,
    inferred: Vec<(usize, HMIRTypeID)>,
}

impl<'a> Deduction<'a> {
    pub fn new(function: &'a HMIRFunction, name: String) -> Self {
        Self {
            function,
            name,
            pending: Vec::new(),
            inferred: Vec::new(),
        }
    }

    pub fn needs_actual(&self) -> bool {
        !self.pending.is_empty() || !self.inferred.is_empty()
    }

    pub fn defer(&mut self, index: usize, argument: Evaluation<HMIRValue>) {
        self.pending.push((index, argument));
    }

    pub fn infer(
        &mut self,
        context: &mut LocalLoweringContext,
        index: usize,
        actual: Option<HMIRTypeID>,
        span: &TokenRange,
    ) -> CXResult<Vec<(usize, Evaluation<HMIRValue>, HMIRTypeID)>> {
        if self.pending.is_empty() && self.inferred.is_empty() {
            return Ok(Vec::new());
        }
        let form = *self.function.signature().params()[index].ty();
        let actual = actual.map(|actual| self.argument(context, form, actual, span));
        let mut unresolved = Vec::new();
        self.unify(context, form, actual, &mut unresolved, span)?;

        let (resolved, pending) = self
            .pending
            .drain(..)
            .partition::<Vec<_>, _>(|(index, _)| self.inferred.iter().any(|(i, _)| i == index));
        self.pending = pending;
        if let Some(index) = unresolved.into_iter().find(|index| self.is_pending(*index)) {
            return Err(self.undeduced(index, span));
        }
        Ok(resolved
            .into_iter()
            .map(|(index, argument)| (index, argument, self.deduced(index).unwrap()))
            .collect())
    }

    pub fn finish(&self, span: &TokenRange) -> CXResult<()> {
        match self.pending.first() {
            Some((index, _)) => Err(self.undeduced(*index, span)),
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
        if matches!(
            self.function.body().expr(form).kind(),
            HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRExprTypeKind::ReferenceTo(_))
        ) {
            return actual;
        }
        coerce::value_type(context, actual, span)
    }

    fn unify(
        &mut self,
        context: &mut LocalLoweringContext,
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
                for form in iter::once(callee).chain(args) {
                    self.unify(context, *form, None, unresolved, span)?;
                }
                Ok(())
            }
            HMIRExprKind::Type(ty) => self.structure(context, ty, actual, unresolved, span),
            _ => Ok(()),
        }
    }

    fn structure(
        &mut self,
        context: &mut LocalLoweringContext,
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
                self.unify(context, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::ReferenceTo(inner) => {
                let actual = actual.map(|actual| env.reference_inner(actual).unwrap_or(actual));
                self.unify(context, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::Array { element, .. } => {
                let actual = actual.and_then(|actual| {
                    env.array_inner(actual)
                        .or_else(|| env.pointer_inner(actual))
                });
                self.unify(context, *element, actual, unresolved, span)
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
                self.unify(context, *inner, actual, unresolved, span)
            }
            HMIRExprTypeKind::Staged(inner) => {
                let actual = actual.map(|actual| match env.resolve_kind(actual) {
                    HMIRTypeKind::Staged(inner) => *inner,
                    _ => actual,
                });
                self.unify(context, *inner, actual, unresolved, span)
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
                    self.unify(context, *param.ty(), actual, unresolved, span)?;
                }
                let actual = actual.map(|actual| *actual.return_type());
                self.unify(context, *signature.return_type(), actual, unresolved, span)
            }
            _ => Ok(()),
        }
    }

    fn bind(
        &mut self,
        context: &LocalLoweringContext,
        index: usize,
        actual: Option<HMIRTypeID>,
        unresolved: &mut Vec<usize>,
        span: &TokenRange,
    ) -> CXResult<()> {
        match (actual, self.deduced(index)) {
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
            (Some(actual), None) if self.is_pending(index) => {
                self.inferred.push((index, actual));
                Ok(())
            }
            (None, None) if self.is_pending(index) => {
                unresolved.push(index);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn is_pending(&self, index: usize) -> bool {
        self.pending.iter().any(|(pending, _)| *pending == index)
    }

    fn deduced(&self, index: usize) -> Option<HMIRTypeID> {
        self.inferred
            .iter()
            .find(|(inferred, _)| *inferred == index)
            .map(|(_, ty)| *ty)
    }

    fn parameter(&self, index: usize) -> String {
        let local = self.function.params()[index];
        match self.function.body().local(local).name() {
            Some(name) => name.to_string(),
            None => index.to_string(),
        }
    }

    fn undeduced(&self, index: usize, span: &TokenRange) -> CXError {
        hmir_error(
            span,
            &typecheck::TEMPLATE_DEDUCTION,
            format!(
                "comptime argument '{}' of '{}'",
                self.parameter(index),
                self.name
            ),
        )
    }
}
