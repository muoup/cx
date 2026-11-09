pub(crate) mod contract;
mod dependent;
mod specialize;

pub(crate) use dependent::{Frame, body, dependent, parameter_name};

use cx_hmir::{
    HMIRBodyContext, HMIRDefKind, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit, env::function::LocalLoweringContext, eval::{
        EvalResult, aggregate::variant, coerce, comptime, eval, expr, flow::Evaluation, helpers, staged, types,
    }, log::hmir_error, symbol, value::{
        self, HMIRValue, HMIRValueKind,
        immediate::{HMIRFunctionRef, HMIRImmediate},
    },
};

pub(crate) fn eval_call(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = span.clone();
    let args = args
        .iter()
        .map(|expr| eval(context, body, *expr))
        .collect::<EvalResult<Vec<_>>>()?;

    let named = named
        .iter()
        .map(|(name, expr)| eval(context, body, *expr).map(|expr| (name.clone(), expr)))
        .collect::<EvalResult<Vec<_>>>()?;

    eval(context, body, callee)?.and_then(context, body, None, move |context, body, callee, _| {
        hmir_call(context, body, &span, callee, &args, &named)
    })
}

pub(crate) fn hmir_call(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    callee: HMIRValue,
    args: &[Evaluation<HMIRValue>],
    named: &[(CXIdent, Evaluation<HMIRValue>)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let callee = coerce::collapse(context, callee, span)?;
    let ty = callee.ty();

    let (callee, applied) = match callee.take_kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Curried { callee, args }) => {
            (*callee, Some(args.as_slice()))
        }

        HMIRValueKind::Immediate(HMIRImmediate::Quote(_)) => {
            if !named.is_empty() {
                // ERROR: A quote does not have named arguments, so named parameters cannot match

                let context = ("a named argument".into(), "a staged expression call".into());
                return Err(hmir_error(&span, &typecheck::INVALID_CONTEXT, context).into());
            }

            return staged::apply(context, body, callee, &args, &span);
        }

        _ => (callee.with_ty(ty), None),
    };

    // To simplify semantics, all function callees are unified under a @fn& types, this gives us three implicitly
    // supported no ops:
    // 
    // @fn* -> @fn& -- 
    // @fn -> @fn& -- either a 
    let ty = match context.env().resolve_kind(ty) {
        HMIRTypeKind::ReferenceTo { inner, .. } => *inner,
        HMIRTypeKind::PointerTo(base) => {
            helpers::null_check(context, callee.clone(), span)?;
            types::reference(context.env_mut(), *base, None, span)
        }
        
        _ => types::reference(context.env_mut(), ty, None, span),
    };

    let env = context.env();
    let subject = || match value::as_function(&callee) {
        Some(HMIRFunctionRef::Def(def)) => {
            format!("'{}'", env.symbols().resolve_definition(def).name())
        }
        Some(HMIRFunctionRef::Constructor { ty, variant }) => {
            format!("'{}'", variant::name(env, ty, variant))
        }
        _ => "function".into(),
    };
    if let Some((name, value)) = named.first() {
        let unknown = (subject(), name.to_string());
        return Err(hmir_error(
            body.expr(*value).span(),
            &typecheck::UNKNOWN_PARAMETER,
            unknown,
        )
        .into());
    }
    let HMIRTypeKind::Function(signature) = env.resolve_kind(referent) else {
        let kind = (types::name(env, callee.ty()), "callable".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    };
    let signature = signature.clone();
    let params = signature.params();
    let applied = applied.into_iter().flatten().collect::<Vec<_>>();
    let count = applied.len() + args.len();
    if count < params.len() || (!signature.is_variadic() && count > params.len()) {
        let counts = (subject(), params.len(), count, signature.is_variadic());
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts).into());
    }
    let source_defined = constructs(&callee).is_none();
    require(
        context,
        span,
        !source_defined || signature.contract().is_safe(),
        source_defined,
    )?;

    let mut values = applied;
    for arg in args {
        let span = body.expr(*arg).span();
        let param = params.get(values.len()).map(|param| *param.ty());
        let value = eval(context, body, *arg)?.complete(context, body, param)?;
        values.push(match param {
            Some(param) => coerce::implicit::standard(context, value, param, span)?,
            None => coerce::default_argument(context, value, span)?,
        });
    }

    let return_type = *signature.return_type();
    match value::as_function(&callee) {
        Some(HMIRFunctionRef::Def(def)) if context.is_runtime() && !context.is_unevaluated() => {
            contract::call(context, def, &values, span, |context| {
                finish(context, &callee, &values, return_type, span)
            })
        }
        _ => finish(context, &callee, &values, return_type, span),
    }
}

fn require(
    context: &LocalLoweringContext,
    span: &TokenRange,
    safe: bool,
    source_defined: bool,
) -> CXResult<()> {
    if !safe {
        expr::require_nonsafe(context, span, "call to non-safe function")?;
    }
    if source_defined {
        expr::require_impure(context, span, "call to a user-defined function")?;
    }
    Ok(())
}