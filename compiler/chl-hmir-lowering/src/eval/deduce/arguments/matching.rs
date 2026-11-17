use chl_hmir::{
    expr::HMIRExprKind,
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::{HMIRDeclaredMove, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
};
use chl_log::{CHLResult, catalogue::mir};
use chl_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        call::{self, dependent::Frame},
        deduce::{
            self,
            pattern::{AliasScope, Form},
        },
        eval, flow, symbol, types,
    },
    log::hmir_error,
    lower::declaration,
    value::{self, HMIRValue},
};

use super::{Deduction, instance};

impl Deduction<'_> {
    pub(super) fn unify(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        form: Form<'_>,
        actual: Option<HMIRTypeID>,
        span: &TokenRange,
    ) -> CHLResult<()> {
        let (Some(form), Some(actual)) = (form.substitute(), actual) else {
            return Ok(());
        };
        match form.body.expr(form.expr).kind() {
            HMIRExprKind::Local(_) => {
                if let Some(index) = self.param(form) {
                    let actual = if form.argument {
                        types::qualify::value(context, actual, span)
                    } else {
                        actual
                    };
                    let actual = deduce::type_constant(context, actual, span);
                    self.record(context, frame, index, Some(actual), span)?;
                }
                Ok(())
            }
            HMIRExprKind::Call {
                callee,
                args,
                named,
            } => {
                let value = frame.enter(context, span, |child| {
                    let source = form.source.unwrap_or_else(|| child.source());
                    child.relocated(source, |child| flow::top(eval(child, form.body, *callee)))
                });
                let Ok(value) = value else {
                    return Ok(());
                };
                let (callee, curried) = match value::into_application(value) {
                    Ok(application) => (*application.callee, application.args),
                    Err(callee) => (callee, Vec::new()),
                };
                let Some(def) = value::as_function(&callee) else {
                    return Ok(());
                };
                let unit = context.unit(def.brand());
                let function =
                    declaration::function(unit.resolve_def(def).expect("function exists"));
                if let Some(root) = function.transparent_body() {
                    if form.scope.is_some_and(|scope| scope.contains(def)) {
                        return Err(hmir_error(
                            span,
                            &mir::DEPENDENCY_CYCLE,
                            format!(
                                "transparent type alias '{}'",
                                symbol::name(context.env(), def)
                            ),
                        ));
                    }
                    let (arguments, partial) = call::bind(
                        Some(function),
                        function.signature(),
                        &curried,
                        form.body,
                        args,
                        named,
                        &symbol::name(context.env(), def),
                        span,
                    )?;
                    if partial {
                        return Ok(());
                    }
                    let arguments = arguments
                        .into_iter()
                        .enumerate()
                        .map(|(index, arg)| {
                            if let Some(arg) = arg {
                                Some(form.at(arg))
                            } else if curried.get(index).is_some_and(Option::is_some) {
                                None
                            } else {
                                function.prototype().params()[index]
                                    .default()
                                    .map(|expr| Form {
                                        source: Some(def.brand()),
                                        ..Form::new(function.prototype().defaults(), expr)
                                    })
                            }
                        })
                        .collect();
                    let scope = AliasScope {
                        def,
                        function,
                        arguments,
                        parent: form.scope,
                    };
                    let form = Form {
                        body: function.body(),
                        expr: root,
                        scope: Some(&scope),
                        source: Some(def.brand()),
                        argument: form.argument,
                    };
                    return self.unify(context, frame, form, Some(actual), span);
                }

                let actual = if form.argument {
                    types::qualify::value(context, actual, span)
                } else {
                    actual
                };
                let Some(instance) = instance(context, def, actual) else {
                    return Ok(());
                };
                let params = function.prototype().params();
                let positional = (0..params.len()).filter(|index| {
                    curried.get(*index).is_none_or(Option::is_none)
                        && params[*index].default().is_none()
                });
                for (index, arg) in positional.zip(args) {
                    self.unify_argument(context, frame, form.at(*arg), instance.get(index), span)?;
                }
                for (name, arg) in named {
                    if let Some(index) = params.iter().position(|param| param.name() == Some(name))
                    {
                        self.unify_argument(
                            context,
                            frame,
                            form.at(*arg),
                            instance.get(index),
                            span,
                        )?;
                    }
                }
                Ok(())
            }
            HMIRExprKind::Type(ty) => {
                let actual = if form.argument
                    && !matches!(ty.kind(), HMIRExprTypeKind::ReferenceTo { .. })
                {
                    types::qualify::value(context, actual, span)
                } else {
                    actual
                };
                self.structure(context, frame, form, ty, actual, span)
            }
            _ => Ok(()),
        }
    }

    fn unify_argument(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        form: Form<'_>,
        actual: Option<&HMIRValue>,
        span: &TokenRangestream(T) this,,
    ) -> CHLResult<()> {
        match (actual.and_then(value::as_type), self.param(form)) {
            (None, Some(param)) => self.record(context, frame, param, actual.cloned(), span),
            (ty, _) => self.unify(context, frame, form, ty, span),
        }
    }

    fn structure(
        &mut self,
        context: &mut LocalLoweringContext,
        frame: &mut Frame,
        form: Form<'_>,
        ty: &HMIRExprType,
        actual: HMIRTypeID,
        span: &TokenRange,
    ) -> CHLResult<()> {
        let env = context.env();
        match ty.kind() {
            HMIRExprTypeKind::PointerTo(inner) => {
                let actual = match env.resolve_kind(env.referent(actual)) {
                    HMIRTypeKind::Function { .. } => Some(env.referent(actual)),
                    _ => env
                        .pointer_inner(actual)
                        .or_else(|| env.array_inner(actual)),
                };
                self.unify(context, frame, form.at(*inner), actual, span)
            }
            HMIRExprTypeKind::ReferenceTo { inner, .. } => {
                let actual = env.referent(actual);
                self.unify(context, frame, form.at(*inner), Some(actual), span)
            }
            HMIRExprTypeKind::Array { element, length } => {
                let actual_element = env
                    .array_inner(actual)
                    .or_else(|| env.pointer_inner(actual));
                let actual_length = match env.resolve_kind(actual) {
                    HMIRTypeKind::Array { length, .. } => Some(*length),
                    _ => None,
                };
                self.unify(context, frame, form.at(*element), actual_element, span)?;
                if let Some(actual_length) = actual_length
                    && (form.scope.is_some() || self.function.transparent_body().is_some())
                    && let Some(index) = self.param(form.at(*length))
                {
                    let ty = types::int(context.env_mut(), HMIRIntWidth::I64, false, span);
                    let value = value::int(context.env(), i128::from(actual_length), ty);
                    self.record(context, frame, index, Some(value), span)?;
                }
                Ok(())
            }
            HMIRExprTypeKind::Undefined(HMIRUndefined::Array { element })
                if form.scope.is_some() || self.function.transparent_body().is_some() =>
            {
                let actual = env.array_inner(actual);
                self.unify(context, frame, form.at(*element), actual, span)
            }
            HMIRExprTypeKind::Nominal(inner) => {
                let declared = ty.semantics();
                if declared.unsafe_move || declared.move_semantics != HMIRDeclaredMove::default() {
                    return Ok(());
                }
                let qualifiers = HMIRTypeSemantics {
                    constant: declared.constant,
                    volatile: declared.volatile,
                    ..Default::default()
                };
                let actual = types::qualify::remove(context, actual, qualifiers, span);
                self.unify(context, frame, form.at(*inner), Some(actual), span)
            }
            HMIRExprTypeKind::Staged(inner) => {
                let actual = match env.resolve_kind(actual) {
                    HMIRTypeKind::Staged(inner) => *inner,
                    _ => actual,
                };
                self.unify(context, frame, form.at(*inner), Some(actual), span)
            }
            HMIRExprTypeKind::Function { signature, .. } => {
                let actual = env
                    .pointer_inner(actual)
                    .unwrap_or_else(|| env.referent(actual));
                let HMIRTypeKind::Function {
                    signature: actual, ..
                } = env.resolve_kind(actual)
                else {
                    return Ok(());
                };
                if actual.is_variadic() != signature.is_variadic()
                    || actual.params().len() != signature.params().len()
                {
                    return Ok(());
                }
                let actual = actual.clone();
                for (param, actual) in signature.params().iter().zip(actual.params()) {
                    self.unify(
                        context,
                        frame,
                        form.at(*param.ty()),
                        Some(*actual.ty()),
                        span,
                    )?;
                }
                self.unify(
                    context,
                    frame,
                    form.at(*signature.return_type()),
                    Some(*actual.return_type()),
                    span,
                )
            }
            _ => Ok(()),
        }
    }

    fn param(&self, form: Form<'_>) -> Option<usize> {
        let form = form.substitute()?;
        let HMIRExprKind::Local(local) = form.body.expr(form.expr).kind() else {
            return None;
        };
        self.function
            .params()
            .iter()
            .position(|param| param == local)
    }
}
