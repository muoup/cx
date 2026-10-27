use std::mem;

use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRUnitID,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{aggregate::HMIRTypeSemantics, context::HMIRTypeContext},
    unit::{def::HMIRDeclarationPoint, function::HMIRContract},
};
use cx_log::{
    CXResult,
    catalogue::{self, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{
        ComptimeContext, Expectations, FnContext, LocalLoweringContext, Permission,
        runtime::RuntimeScope,
    },
    eval::{EvalResult, coerce, eval, flow::Evaluation, types},
    log::hmir_error,
    lower::mir,
    symbol,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

struct Shadowed {
    local: HMIRLocalID,
    previous: Option<HMIRValue>,
}

pub(crate) fn check(
    context: &mut LocalLoweringContext,
    contract: &HMIRContract,
    args: &[HMIRValue],
    result: Option<&HMIRValue>,
    name: &str,
    span: &TokenRange,
) -> CXResult<()> {
    let (expression, label) = match result {
        None => match contract.precondition() {
            Some(expression) => (expression, "precondition"),
            None => return Ok(()),
        },
        Some(_) => match contract.postcondition() {
            Some((_, expression)) => (expression, "postcondition"),
            None => return Ok(()),
        },
    };
    let mut bindings = bind_params(context, contract, args, span)?;
    if let (Some(result), Some((local, _))) = (result, contract.postcondition()) {
        bindings.extend(bind_result(context, local, Some(result), span)?);
    }
    let (owner, point) = (context.source(), context.visible_through());
    let value = condition(context, contract, owner, point, expression, span);
    restore(context, bindings);
    if !matches!(value?.kind(), HMIRValueKind::Constant(HMIRConstant::Int(value)) if *value != 0) {
        return Err(hmir_error(
            span,
            &catalogue::mir::COMPTIME_ASSERTION,
            Some(format!("{label} of '{name}' does not hold at compile time")),
        ));
    }
    Ok(())
}

pub(crate) fn call(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    args: &[HMIRValue],
    span: &TokenRange,
    emit: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<HMIRValue>,
) -> EvalResult<HMIRValue> {
    let unit = context.env_mut().symbols_mut().owner(def.brand()).expect("function owner exists");
    let source = unit.resolve_def(def).expect("function exists");
    let selected = symbol::select(source, None)?;
    let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind() else {
        unreachable!("a called definition is a function")
    };
    let contract = function.signature().contract();
    if contract.precondition().is_none() && contract.postcondition().is_none() {
        return emit(context);
    }
    let point = function.body().visible_through();

    let mut bindings = bind_params(context, contract, args, span)?;
    if let Some(expression) = contract.precondition() {
        let value = condition(context, contract, def.brand(), point, expression, span)?;
        mir::assert(context, &value, Some("precondition failed"), span)?;
    }
    let result = emit(context)?;
    if let (Evaluation::Ready(value), Some((local, expression))) = (&result, contract.postcondition()) {
        bindings.extend(bind_result(context, local, Some(value), span)?);
        let value = condition(context, contract, def.brand(), point, expression, span)?;
        mir::assert(context, &value, None, span)?;
    }
    restore(context, bindings);
    Ok(result)
}

pub(crate) fn enter(
    context: &mut LocalLoweringContext,
    params: &[HMIRLocalID],
    span: &TokenRange,
) -> CXResult<()> {
    let contract = context.signature().expect("a lowered function has a signature").contract();
    if contract.precondition().is_none() && contract.postcondition().is_none() {
        return Ok(());
    }
    for (local, param) in contract.params().iter().zip(params) {
        let value = context.get_local(param).cloned().expect("parameters are bound before the contract");
        let value = match value.kind() {
            HMIRValueKind::Constant(_) => value,
            _ => emit::copy(context, value, span)?,
        };
        let value = observe(context, value, span)?;
        emit::bind_local(context, *local, value);
    }
    if let Some(expression) = contract.precondition() {
        let (owner, point) = (context.source(), context.visible_through());
        let value = condition(context, contract, owner, point, expression, span)?;
        mir::assert(context, &value, None, span)?;
    }
    Ok(())
}

pub(crate) fn exit(
    context: &mut LocalLoweringContext,
    result: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<()> {
    let origin = context
        .runtime()
        .scopes()
        .controls()
        .find_map(|(index, scope)| Some((index, scope.returning()?)));
    let Some((index, (source, point))) = origin else {
        return exit_condition(context, result, span);
    };
    let view = RuntimeScope::default();
    context.runtime_mut().scopes_mut().push(view, Some(index), Default::default());
    let checked = context.relocated(source, point, |context| exit_condition(context, result, span));
    context.runtime_mut().scopes_mut().pop();
    checked
}

fn exit_condition(
    context: &mut LocalLoweringContext,
    result: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<()> {
    let Some(signature) = context.signature() else {
        return Ok(());
    };
    let contract = signature.contract();
    let Some((local, expression)) = contract.postcondition() else {
        return Ok(());
    };
    let bindings = Vec::from_iter(bind_result(context, local, result, span)?);
    let (owner, point) = (context.source(), context.visible_through());
    let value = condition(context, contract, owner, point, expression, span);
    restore(context, bindings);
    mir::assert(context, &value?, Some("postcondition failed"), span)
}

fn condition(
    context: &mut LocalLoweringContext,
    contract: &HMIRContract,
    owner: HMIRUnitID,
    point: Option<HMIRDeclarationPoint>,
    expression: HMIRExprID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let state = mem::replace(context.context_mut(), FnContext::Comptime(ComptimeContext::new()));
    let evaluation = context.evaluation();
    let mut child = context.child(None, state, Some(evaluation)).sourced(owner).at(point);
    let permission = match contract.is_safe() {
        true => Permission::Safe,
        false => Permission::Nonsafe,
    };
    let expectations = Expectations::new(permission).expecting_pure();
    let value = child.with_expectations(expectations, |child| {
        match eval(child, contract.body(), expression)? {
            Evaluation::Ready(value) => coerce::condition(child, value, span),
            Evaluation::Incomplete(incomplete) => Err(incomplete.error()),
            Evaluation::Stop(_) => {
                let requirement = ("a contract condition".into(), "a value".into(), None);
                Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, requirement))
            }
        }
    });
    *context.context_mut() = child.into_context();
    value
}

fn observe(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if let HMIRValueKind::Constant(_) = value.kind() {
        return Ok(value);
    }
    let storage = emit::temporary(context, value, span)?;
    let constant = HMIRTypeSemantics { constant: true, ..Default::default() };
    let ty = types::specify(context, storage.ty(), constant, span);
    let reference = types::reference(context.env_mut(), ty, None, span);
    Ok(storage.with_ty(reference))
}

fn bind_params(
    context: &mut LocalLoweringContext,
    contract: &HMIRContract,
    args: &[HMIRValue],
    span: &TokenRange,
) -> CXResult<Vec<Shadowed>> {
    let mut bindings = Vec::with_capacity(args.len());
    for (local, value) in contract.params().iter().zip(args) {
        let value = observe(context, value.clone(), span)?;
        bindings.push(bind(context, *local, value));
    }
    Ok(bindings)
}

fn bind_result(
    context: &mut LocalLoweringContext,
    local: Option<HMIRLocalID>,
    result: Option<&HMIRValue>,
    span: &TokenRange,
) -> CXResult<Option<Shadowed>> {
    let Some(local) = local else {
        return Ok(None);
    };
    let Some(result) = result.filter(|result| !context.env().is_void(result.ty())) else {
        return Err(hmir_error(span, &typecheck::VOID_POSTCONDITION_BINDING, ()));
    };
    let value = observe(context, result.clone(), span)?;
    Ok(Some(bind(context, local, value)))
}

fn bind(context: &mut LocalLoweringContext, local: HMIRLocalID, value: HMIRValue) -> Shadowed {
    let previous = context.get_local(&local).cloned();
    emit::bind_local(context, local, value);
    Shadowed { local, previous }
}

fn restore(context: &mut LocalLoweringContext, bindings: Vec<Shadowed>) {
    for Shadowed { local, previous } in bindings.into_iter().rev() {
        match previous {
            Some(value) => emit::bind_local(context, local, value),
            None => context.remove_local(local),
        }
    }
}
