pub(crate) mod contract;
pub(crate) mod dependent;

use cx_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRFnSignature, HMIRFunction,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        aggregate::variant,
        coerce, eval, eval_comptime, expr,
        flow::{self, Evaluation},
        place, staged, types,
    },
    log::hmir_error,
    lower::declaration,
    value::{
        self, HMIRValue, HMIRValueKind,
        address::Addressable,
        immediate::HMIRImmediate,
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
    let (span, args, named) = (span.clone(), args.to_vec(), named.to_vec());
    eval(context, body, callee)?.chain(
        context,
        body,
        None,
        |_| None,
        move |context, body, callee, expected| {
            let callee = coerce::collapse(context, callee, &span)?;
            apply(context, body, &span, callee, &args, &named)?
                .try_complete(context, body, expected)
        },
    )
}

// Binds the arguments of a call to the parameters of its callee, evaluates them, and calls the callee with the
// values, or curries it with them where the call is partial
fn apply(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    callee: HMIRValue,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    if matches!(
        callee.kind(),
        HMIRValueKind::Immediate(HMIRImmediate::Quote(_))
    ) {
        if let Some((_, argument)) = named.first() {
            let named = ("a named argument".into(), "a staged expression call".into());
            let span = body.expr(*argument).span();
            return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, named).into());
        }
        return staged::apply(context, body, callee, args, span);
    }

    let (callee, applied) = value::uncurried(callable(context, callee, span)?);
    let (callee, signature) = coerce_callee(context, callee, span)?;
    let def = value::as_function(&callee);

    // Only a callee that names a function knows the names and the defaults of its parameters
    let unit = def.map(|def| {
        let symbols = context.env_mut().symbols_mut();
        symbols.owner(def.brand()).expect("function owner exists")
    });
    let declared = match (def, &unit) {
        (Some(def), Some(unit)) => {
            let source = unit.resolve_def(def).expect("function exists");
            Some((def, declaration::function(source)))
        }
        _ => None,
    };
    let function = declared.map(|(_, function)| function);
    let name = name(context, &callee);
    let (args, partial) = bind(function, &signature, &applied, body, args, named, &name, span)?;

    let referent = context.env().referent(callee.ty());
    if let Some((def, function)) = declared.filter(|_| dependent::applies(context, referent)) {
        if !partial {
            context_require(context, span, signature.contract().is_safe(), false)?;
        }
        return dependent::call(context, body, def, function, applied, &args, partial, span);
    }
    let values = arguments(context, body, &signature, declared, applied, &args, partial)?;
    if partial {
        let residual = types::residual(&signature, |index| values[index].is_some());
        let ty = types::callable(context.env_mut(), residual, span);
        return Ok(Evaluation::Ready(value::curried(ty, callee, values)));
    }
    hmir_call(context, span, callee, values.into_iter().flatten().collect())
}

// The argument of each parameter of a callee, then those past the parameters of a variadic one:
//   - 'named' bind the parameters that have a default, by name;
//   - 'args' bind the parameters that have none, in order, less those that 'applied' has bound.
// A call that leaves one of the latter unbound is partial, which only a call whose every argument binds a comptime
// parameter may be
fn bind(
    function: Option<&HMIRFunction>,
    signature: &HMIRFnSignature<HMIRTypeID>,
    applied: &[Option<HMIRValue>],
    body: &HMIRBodyContext,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
    name: &str,
    span: &TokenRange,
) -> CXResult<(Vec<Option<HMIRExprID>>, bool)> {
    let params = signature.params();
    let declared = function.map_or(&[][..], |function| function.prototype().params());
    let defaulted = |index: usize| declared.get(index).is_some_and(|param| param.default().is_some());
    let applied = |index: usize| applied.get(index).is_some_and(Option::is_some);

    let mut bound = vec![None; params.len()];
    for (argument, expr) in named {
        let named = |index: &usize| defaulted(*index) && declared[*index].name() == Some(argument);
        let arg_span = body.expr(*expr).span();
        let Some(index) = (0..params.len()).find(named) else {
            let unknown = (format!("'{name}'"), argument.to_string());
            return Err(hmir_error(arg_span, &typecheck::UNKNOWN_PARAMETER, unknown));
        };
        if applied(index) || bound[index].is_some() {
            let duplicate = (format!("argument '{argument}'"), format!("call to '{name}'"));
            return Err(hmir_error(arg_span, &typecheck::DUPLICATE_ITEM, duplicate));
        }
        bound[index] = Some(*expr);
    }

    let positional = (0..params.len()).filter(|index| !applied(*index) && !defaulted(*index));
    let positional = positional.collect::<Vec<_>>();
    for (index, expr) in positional.iter().zip(args) {
        bound[*index] = Some(*expr);
    }
    let partial = args.len() < positional.len();
    let curries = (!args.is_empty() || !named.is_empty())
        && (0..params.len()).all(|index| bound[index].is_none() || signature.is_comptime_param(index));
    if (partial && !curries) || (args.len() > positional.len() && !signature.is_variadic()) {
        let counts = (format!("'{name}'"), positional.len(), args.len(), signature.is_variadic());
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts));
    }
    bound.extend(args.iter().skip(positional.len()).map(|arg| Some(*arg)));
    Ok((bound, partial))
}

// The value of each argument of a call that is emitted as it is. A parameter without one takes its default,
// unless the call is partial, which leaves it open
fn arguments(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    signature: &HMIRFnSignature<HMIRTypeID>,
    declared: Option<(HMIRDefID, &HMIRFunction)>,
    applied: Vec<Option<HMIRValue>>,
    args: &[Option<HMIRExprID>],
    partial: bool,
) -> EvalResult<Vec<Option<HMIRValue>>> {
    let mut applied = applied.into_iter();
    let mut values = Vec::with_capacity(args.len());
    for (index, arg) in args.iter().enumerate() {
        let param = signature.params().get(index).map(|param| *param.ty());
        values.push(match (applied.next().flatten(), *arg, param) {
            (Some(value), _, _) => Some(value),
            (None, Some(arg), Some(ty)) => {
                let value = eval(context, body, arg)?.complete(context, body, Some(ty))?;
                Some(coerce::implicit::standard(context, value, ty, body.expr(arg).span())?)
            }
            (None, Some(arg), None) => {
                let value = eval(context, body, arg)?.complete(context, body, None)?;
                Some(coerce::default_argument(context, value, body.expr(arg).span())?)
            }
            (None, None, Some(ty)) => match declared {
                Some((def, function))
                    if !partial && function.prototype().params()[index].default().is_some() =>
                {
                    Some(default(context, def, function, index, ty)?)
                }
                _ => None,
            },
            (None, None, None) => None,
        });
    }
    Ok(values)
}

// The default of a parameter means the same at every call: it is evaluated apart from the call, in the unit
// that declares the function
fn default(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    function: &HMIRFunction,
    index: usize,
    ty: HMIRTypeID,
) -> CXResult<HMIRValue> {
    let prototype = function.prototype();
    let default = prototype.params()[index].default();
    let default = default.expect("a parameter left to its default has one");
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut child = context.child(None, comptime, None).sourced(def.brand());
    eval_comptime(&mut child, prototype.defaults(), default, Some(ty))
}

// Calls 'callee', which is not curried, with a value for each of its parameters
pub(crate) fn hmir_call(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    callee: HMIRValue,
    args: Vec<HMIRValue>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let (callee, signature) = coerce_callee(context, callee, span)?;
    let params = signature.params();
    if args.len() < params.len() || (args.len() > params.len() && !signature.is_variadic()) {
        let counts = (
            format!("'{}'", name(context, &callee)),
            params.len(),
            args.len(),
            signature.is_variadic(),
        );
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts).into());
    }

    let constructor = match *callee.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Constructor { ty, variant }) => Some((ty, variant)),
        _ => None,
    };
    if constructor.is_none() {
        context_require(context, span, signature.contract().is_safe(), false)?;

        let referent = context.env().referent(callee.ty());
        if dependent::applies(context, referent) {
            let Some(def) = value::as_function(&callee) else {
                let unavailable = "this function".into();
                return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, unavailable).into());
            };
            return dependent::apply(context, def, args.into_iter().map(Some).collect(), span);
        }
    }

    let mut values = Vec::with_capacity(args.len());
    for (index, value) in args.into_iter().enumerate() {
        values.push(match params.get(index) {
            Some(param) => coerce::implicit::standard(context, value, *param.ty(), span)?,
            None => coerce::default_argument(context, value, span)?,
        });
    }
    let return_type = *signature.return_type();
    match (constructor, value::as_function(&callee)) {
        (Some((ty, variant)), _) if !context.is_runtime() => {
            let payload = values.into_iter().next();
            let value = variant::construct(context, ty, variant, payload, span)?;
            Ok(Evaluation::Ready(value))
        }
        (None, Some(def)) if !context.is_unevaluated() => {
            contract::call(context, def, &values, span, |context| {
                finish(context, &callee, &values, return_type, span)
            })
        }
        _ => finish(context, &callee, &values, return_type, span),
    }
}

// To simplify semantics, all function callees are unified into fn& types. Whether a direct or indirect
// call is generated at runtime depends on the underlying MIRValue produced, not on the type.
// A callee read out of the storage holding it, unless it names a function itself
fn callable(context: &mut LocalLoweringContext, callee: HMIRValue, span: &TokenRange) -> CXResult<HMIRValue> {
    let env = context.env();
    match env.is_function(env.referent(callee.ty())) {
        true => Ok(callee),
        false => coerce::promotion::std_rvalue(context, callee, span),
    }
}

fn coerce_callee(
    context: &mut LocalLoweringContext,
    callee: HMIRValue,
    span: &TokenRange,
) -> CXResult<(HMIRValue, HMIRFnSignature<HMIRTypeID>)> {
    let callee = callable(context, callee, span)?;
    let ty = callee.ty();
    let (subject, ty) = match *context.env().resolve_kind(ty) {
        HMIRTypeKind::ReferenceTo { inner, .. } => (inner, ty),
        HMIRTypeKind::PointerTo(base) => {
            place::null_check(context, &callee, span)?;
            (base, types::reference(context.env_mut(), base, None, span))
        }
        _ => (ty, types::reference(context.env_mut(), ty, None, span)),
    };
    let HMIRTypeKind::Function { signature, .. } = context.env().resolve_kind(subject) else {
        let kind = (types::name(context.env(), callee.ty()), "callable".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind));
    };
    Ok((callee.with_ty(ty), signature.clone()))
}

fn name(context: &LocalLoweringContext, callee: &HMIRValue) -> String {
    match callee.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Address(address))
            if let Addressable::Def(def) = address.base() =>
        {
            context
                .env()
                .symbols()
                .resolve_definition(*def)
                .name()
                .to_string()
        }

        HMIRValueKind::Immediate(HMIRImmediate::Constructor { ty, variant }) => {
            variant::name(context.env(), *ty, *variant)
        }

        _ => "function".into(),
    }
}

fn context_require(
    context: &LocalLoweringContext,
    span: &TokenRange,
    safe: bool,
    pure: bool,
) -> CXResult<()> {
    if !safe {
        expr::require_nonsafe(context, span, "call to non-safe function")?;
    }

    if !pure {
        expr::require_impure(context, span, "call to a user-defined function")?;
    }

    Ok(())
}

fn finish(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    return_type: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let result = match context.is_unevaluated() {
        true => HMIRValue::new(return_type, emit::runtime::register(context, return_type, span)?),
        false => emit::call(context, callee, args, return_type, span)?,
    };

    match context.env().resolve_kind(return_type) {
        HMIRTypeKind::Unreachable => {
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());

            flow::diverged()
        },

        _ => Ok(Evaluation::Ready(result))
    }
}
