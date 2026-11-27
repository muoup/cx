use chl_hmir::{
    HMIRDefID, HMIRFunction, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::{HMIRDeclaredMove, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
};
use chl_log::{CHLResult, catalogue::mir};
use chl_tokens::TokenRange;
use chl_util::identifier::CHLIdent;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        call::{self, dependent::Frame},
        deduce::{
            self,
            pattern::{AliasScope, Form},
        },
        eval, staged, symbol, types,
    },
    log::hmir_error,
    value::{self, HMIRValue},
};

use super::{Deduction, inference::try_probe, instance, record};

pub(super) fn unify(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    form: Form<'_>,
    actual: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<()> {
    let Some(form) = form.substitute() else {
        return Ok(());
    };
    match form.body.expr(form.expr).kind() {
        HMIRExprKind::Local(_) => {
            if let Some(index) = parameter(deduction.function, form) {
                let actual = argument_type(context, form, actual, span);
                let actual = deduce::type_constant(context, actual, span);
                record(deduction, context, frame, index, actual, span)?;
            }
            Ok(())
        }
        HMIRExprKind::Call {
            callee,
            args,
            named,
        } => {
            let Some(callee) = try_probe(context, frame, form.at(*callee), span, eval)? else {
                return Ok(());
            };
            let (callee, curried) = match value::into_application(callee) {
                Ok(application) => (*application.callee, application.args),
                Err(callee) => (callee, Vec::new()),
            };
            let Some(def) = value::as_function(&callee) else {
                return Ok(());
            };
            let function = symbol::function(context.env_mut(), def)?;
            let pattern = CallPattern {
                form,
                def,
                function: &function,
                curried,
                args,
                named,
            };
            match function.structural_body() {
                Some(root) => unfold(deduction, context, frame, pattern, root, actual, span),
                None => match_instance(deduction, context, frame, pattern, actual, span),
            }
        }
        HMIRExprKind::Type(ty) => {
            let actual = if matches!(ty.kind(), HMIRExprTypeKind::ReferenceTo { .. }) {
                actual
            } else {
                argument_type(context, form, actual, span)
            };
            structure(deduction, context, frame, form, ty, actual, span)
        }
        _ => Ok(()),
    }
}

struct CallPattern<'a> {
    form: Form<'a>,
    def: HMIRDefID,
    function: &'a HMIRFunction,
    curried: Vec<Option<HMIRValue>>,
    args: &'a [HMIRExprID],
    named: &'a [(CHLIdent, HMIRExprID)],
}

fn unfold(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    pattern: CallPattern<'_>,
    root: HMIRExprID,
    actual: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<()> {
    let CallPattern {
        form,
        def,
        function,
        ..
    } = pattern;
    if form.scope.is_some_and(|scope| scope.contains(def)) {
        let subject = format!(
            "transparent type alias '{}'",
            symbol::name(context.env(), def)
        );
        return Err(hmir_error(span, &mir::DEPENDENCY_CYCLE, subject));
    }

    let Some(arguments) = try_align(context, &pattern)? else {
        return Ok(());
    };
    let arguments = arguments
        .into_iter()
        .enumerate()
        .map(|(index, arg)| {
            if let Some(arg) = arg {
                Some(form.at(arg))
            } else if pattern.curried.get(index).is_some_and(Option::is_some) {
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
    let expanded = Form {
        body: function.body(),
        expr: root,
        scope: Some(&scope),
        source: Some(def.brand()),
        argument: form.argument,
        eta: form.eta,
    };
    unify(deduction, context, frame, expanded, actual, span)
}

fn match_instance(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    pattern: CallPattern<'_>,
    actual: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<()> {
    let form = pattern.form;
    let actual = argument_type(context, form, actual, span);
    let Some(instance) = instance(context, pattern.def, actual) else {
        return Ok(());
    };
    let Some(arguments) = try_align(context, &pattern)? else {
        return Ok(());
    };
    for (arg, actual) in arguments.into_iter().zip(instance) {
        let Some(arg) = arg else {
            continue;
        };
        let arg = form.at(arg);
        if let Some(ty) = value::as_type(&actual) {
            unify(deduction, context, frame, arg, ty, span)?;
        } else if let Some(index) = arg
            .substitute()
            .and_then(|arg| parameter(deduction.function, arg))
        {
            record(deduction, context, frame, index, actual, span)?;
        }
    }
    Ok(())
}

fn try_align(
    context: &LocalLoweringContext,
    pattern: &CallPattern<'_>,
) -> CHLResult<Option<Vec<Option<HMIRExprID>>>> {
    let span = pattern.form.body.expr(pattern.form.expr).span();
    let (arguments, partial) = call::bind(
        Some(pattern.function),
        pattern.function.signature(),
        &pattern.curried,
        pattern.form.body,
        pattern.args,
        pattern.named,
        &symbol::name(context.env(), pattern.def),
        span,
    )?;
    Ok((!partial).then_some(arguments))
}

fn argument_type(
    context: &mut LocalLoweringContext,
    form: Form<'_>,
    actual: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    if form.argument {
        types::qualify::value(context, actual, span)
    } else {
        actual
    }
}

fn structure(
    deduction: &mut Deduction,
    context: &mut LocalLoweringContext,
    frame: &mut Frame,
    form: Form<'_>,
    ty: &HMIRExprType,
    actual: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<()> {
    let env = context.env();
    let transparent = form.scope.is_some() || deduction.function.transparent_body().is_some();
    let (inner, actual) = match ty.kind() {
        HMIRExprTypeKind::PointerTo(inner) => {
            let actual = match env.resolve_kind(env.referent(actual)) {
                HMIRTypeKind::Function { .. } => Some(env.referent(actual)),
                _ => env
                    .pointer_inner(actual)
                    .or_else(|| env.array_inner(actual)),
            };
            (*inner, actual)
        }
        HMIRExprTypeKind::ReferenceTo { inner, .. } => (*inner, Some(env.referent(actual))),
        HMIRExprTypeKind::Array { element, length } => {
            let actual_element = env
                .array_inner(actual)
                .or_else(|| env.pointer_inner(actual));
            let actual_length = match env.resolve_kind(actual) {
                HMIRTypeKind::Array { length, .. } => Some(*length),
                _ => None,
            };
            if let Some(actual_element) = actual_element {
                unify(
                    deduction,
                    context,
                    frame,
                    form.at(*element),
                    actual_element,
                    span,
                )?;
            }
            if let Some(actual_length) = actual_length.filter(|_| transparent)
                && let Some(index) = form
                    .at(*length)
                    .substitute()
                    .and_then(|bound| parameter(deduction.function, bound))
            {
                let ty = types::int(context.env_mut(), HMIRIntWidth::I64, false, span);
                let value = value::int(context.env(), i128::from(actual_length), ty);
                record(deduction, context, frame, index, value, span)?;
            }
            return Ok(());
        }
        HMIRExprTypeKind::Undefined(HMIRUndefined::Array { element }) if transparent => {
            (*element, env.array_inner(actual))
        }
        HMIRExprTypeKind::Struct { fields }
        | HMIRExprTypeKind::Union { fields }
        | HMIRExprTypeKind::TaggedUnion { fields } if transparent => {
            let actual_fields = match (ty.kind(), env.resolve_kind(actual)) {
                (HMIRExprTypeKind::Struct { .. }, HMIRTypeKind::Struct { fields })
                | (HMIRExprTypeKind::Union { .. }, HMIRTypeKind::Union { fields })
                | (HMIRExprTypeKind::TaggedUnion { .. }, HMIRTypeKind::TaggedUnion { fields }) => fields,
                _ => return Ok(()),
            };
            let HMIRDeclaredMove::Explicit(move_semantics) = ty.semantics().move_semantics else {
                return Ok(());
            };
            let actual_semantics = env.semantics(actual);
            if env.strong_identifier(actual).is_some()
                || move_semantics != actual_semantics.move_semantics
                || ty.semantics().unsafe_move != actual_semantics.unsafe_move
                || fields.len() != actual_fields.len()
                || fields.iter().zip(actual_fields).any(|(pattern, actual)| {
                    pattern.name() != actual.name() || pattern.bit_width() != actual.bit_width()
                })
            {
                return Ok(());
            }

            let actual_types = actual_fields.iter().map(|field| *field.ty()).collect::<Vec<_>>();
            for (pattern, actual) in fields.iter().zip(actual_types) {
                unify(deduction, context, frame, form.at(*pattern.ty()), actual, span)?;
            }
            return Ok(());
        }
        HMIRExprTypeKind::Nominal { inner, .. } => {
            let declared = ty.semantics();
            if declared.unsafe_move || declared.move_semantics != HMIRDeclaredMove::default() {
                return Ok(());
            }
            let qualifiers = HMIRTypeSemantics {
                constant: declared.constant,
                volatile: declared.volatile,
                ..Default::default()
            };
            (
                *inner,
                Some(types::qualify::remove(context, actual, qualifiers, span)),
            )
        }
        HMIRExprTypeKind::Staged(inner) => {
            let actual = match env.resolve_kind(actual) {
                HMIRTypeKind::Staged(inner) => *inner,
                HMIRTypeKind::Function { signature, .. } if form.eta => {
                    staged::eta_result(env, signature, 0).unwrap_or(actual)
                }
                _ => actual,
            };
            (*inner, Some(actual))
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
                unify(
                    deduction,
                    context,
                    frame,
                    form.at(*param.ty()),
                    *actual.ty(),
                    span,
                )?;
            }
            let result = if form.eta
                && signature.stage() == HMIRFunctionStage::Comptime
                && let Some(result) =
                    staged::eta_result(context.env(), &actual, signature.params().len())
            {
                staged::lift(context.env_mut(), result, span)
            } else {
                *actual.return_type()
            };
            return unify(
                deduction,
                context,
                frame,
                form.at(*signature.return_type()),
                result,
                span,
            );
        }
        _ => return Ok(()),
    };
    if let Some(actual) = actual {
        unify(deduction, context, frame, form.at(inner), actual, span)?;
    }
    Ok(())
}

fn parameter(function: &HMIRFunction, form: Form<'_>) -> Option<usize> {
    let HMIRExprKind::Local(local) = form.body.expr(form.expr).kind() else {
        return None;
    };
    function.params().iter().position(|param| param == local)
}
