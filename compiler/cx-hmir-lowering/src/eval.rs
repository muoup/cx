mod aggregate;
mod binding;
mod call;
mod complete;
mod control;
mod def;
mod contract;
mod function;
mod literal;
mod name;
mod place;
mod storage;

pub(crate) mod comptime;
pub(crate) mod op;
pub(crate) mod coerce;

pub(crate) mod flow;
pub(crate) mod types;

pub(crate) use comptime::eval_comptime;
pub(crate) use def::type_value;
pub(crate) use flow::EvalResult;

use cx_hmir::{
    HMIRBody, HMIRCoerceMode, HMIRControlOp, HMIRDefID, HMIRFnSignature, HMIRFunction, HMIROp,
    HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::HMIRTypeID,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::MIRFunction;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext, RuntimeContext},
        resolver::HMIRResolver,
    },
    eval::{
        aggregate::eval_aggregate,
        binding::{eval_block, eval_let, eval_local},
        call::eval_call,
        control::{eval_for, eval_if, eval_loop_exit, eval_return, eval_unsafe, eval_while},
        def::eval_def,
        flow::ComptimeStop,
        flow::{Evaluation, ready},
        literal::eval_literal,
        op::{eval_binary, eval_declared_type, eval_type_op, eval_unary},
        place::{eval_address_of, eval_assign, eval_dereference, eval_leak, eval_move},
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn lower_runtime_function<'hmir>(
    env: &mut HMIREnvironment<'_, 'hmir>,
    id: HMIRDefID,
    function: &HMIRFunction,
    signature: &HMIRFnSignature<HMIRTypeID>,
) -> CXResult<MIRFunction> {
    let body = function.body();
    let source_signature = function.signature();
    let def = function
        .def()
        .expect("Runtime function lowered without a definition");
    let span = body.expr(def.root()).span();

    let prototype = types::lower_prototype(env, id, None, span)?;
    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = LocalLoweringContext::new(env, Some(source_signature), runtime)
        .sourced(id.brand())
        .at(body.visible_through())
        .with_return_type(*signature.return_type());

    emit::open_scope(&mut context, span.clone());
    let completion = runtime_definition(&mut context, function, signature);
    emit::close_scope(&mut context, span.clone());
    flow::top(flow::catch_diverged(completion))?;

    match context.into_context() {
        FnContext::Runtime(runtime) => Ok(runtime.finish()),
        _ => unreachable!("runtime function lowered into a non-runtime context"),
    }
}

fn runtime_definition(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
    signature: &HMIRFnSignature<HMIRTypeID>,
) -> EvalResult<HMIRValue> {
    let body = function.body();

    for (param, local) in signature.params().iter().zip(function.params()) {
        let decl = body.local(*local);
        let ty = *param.ty();
        let value = runtime::parameter(context, ty, decl.name().cloned(), decl.span())?;
        let reference = types::reference(context.env_mut(), ty, decl.span());
        let value = value.with_ty(reference);

        emit::storage::bind_local(context, *local, value);
    }

    function::body(context, function)
}

pub(crate) fn eval(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let expr = body.expr(id);
    let span = expr.span();

    match expr.kind() {
        HMIRExprKind::Literal(literal) => {
            Ok(Evaluation::Ready(eval_literal(context, span, literal)))
        }
        HMIRExprKind::Type(_) => {
            let ty = eval_declared_type(context, body, id)?;
            complete::type_result(context, ty, span)
        }
        HMIRExprKind::Local(local) => eval_local(context, body, span, *local),
        HMIRExprKind::Let { local, initializer } => {
            eval_let(context, body, span, *local, *initializer)
        }
        HMIRExprKind::Block {
            statements,
            scoped,
            yields,
        } => eval_block(context, body, span, statements, *scoped, *yields),

        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => eval_if(context, body, span, *condition, *then_branch, *else_branch),
        HMIRExprKind::While {
            condition,
            body: loop_body,
            pre_eval,
        } => eval_while(context, body, span, *condition, *loop_body, None, *pre_eval),
        HMIRExprKind::For {
            init,
            condition,
            increment,
            body: loop_body,
        } => eval_for(
            context, body, span, *init, *condition, *increment, *loop_body,
        ),

        HMIRExprKind::Op(op) => match op {
            HMIROp::BinOp { op, lhs, rhs } => eval_binary(context, body, span, *op, *lhs, *rhs),
            HMIROp::UnOp { op, operand } => eval_unary(context, body, span, *op, *operand),
            HMIROp::Coerce {
                mode,
                value,
                target,
            } => match mode {
                HMIRCoerceMode::Truthy => {
                    let value = ready!(eval(context, body, *value));
                    Ok(Evaluation::Ready(coerce::condition(context, value, span)?))
                }
                HMIRCoerceMode::Implicit | HMIRCoerceMode::Convert | HMIRCoerceMode::CCast => {
                    let target = ready!(eval_type(context, body, *target));
                    let value = eval(context, body, *value)?;
                    let value = ready!(value.complete(context, body, Some(target)));
                    Ok(Evaluation::Ready(match mode {
                        HMIRCoerceMode::Implicit => coerce::implicit(context, value, target, span)?,
                        _ => coerce::explicit(context, value, target, span)?,
                    }))
                }
            },

            HMIROp::Control(HMIRControlOp::Return(value)) => {
                eval_return(context, body, span, *value)
            }
            HMIROp::Control(HMIRControlOp::Break) => {
                eval_loop_exit(context, span, ComptimeStop::Break)
            }
            HMIROp::Control(HMIRControlOp::Continue) => {
                eval_loop_exit(context, span, ComptimeStop::Continue)
            }
            HMIROp::Control(HMIRControlOp::Unsafe(operand)) => eval_unsafe(context, body, *operand),

            HMIROp::Assign { target, op, value } => {
                eval_assign(context, body, span, *target, *op, *value)
            }
            HMIROp::AddressOf(operand) => eval_address_of(context, body, span, *operand),
            HMIROp::Dereference(pointer) => eval_dereference(context, body, span, *pointer),

            HMIROp::Type(op) => eval_type_op(context, body, span, op),
            HMIROp::Control(control) => todo!("HMIROp::Control({})", control.path()),
            HMIROp::OwnershipOp(HMIROwnershipOp::Move(operand)) => {
                eval_move(context, body, span, *operand)
            }
            HMIROp::OwnershipOp(HMIROwnershipOp::Leak(operand)) => {
                eval_leak(context, body, span, *operand)
            }
            HMIROp::OwnershipOp(ownership) => {
                todo!("HMIROp::OwnershipOp({})", ownership.path())
            }
            HMIROp::AggregateOp(aggregate) => eval_aggregate(context, body, span, aggregate),
        },

        HMIRExprKind::Def(def) => eval_def(context, span, *def),
        HMIRExprKind::Name { name, tag } => name::eval_name(context, span, name, *tag),
        HMIRExprKind::Piped(value) => eval(context, body, *value),
        HMIRExprKind::Call { callee, args } => eval_call(context, body, span, *callee, args),

        HMIRExprKind::Hole(_) => complete::hole(context, span),
        HMIRExprKind::Comptime(inner) => Ok(Evaluation::Ready(eval_comptime(
            context, body, *inner, None,
        )?)),
        HMIRExprKind::Quote { .. } => todo!("HMIRExprKind::Quote"),
        HMIRExprKind::Splice { .. } => todo!("HMIRExprKind::Splice"),
        HMIRExprKind::Intrinsic(_) => todo!("HMIRExprKind::Intrinsic"),
        HMIRExprKind::Error(_) => todo!("HMIRExprKind::Error"),
        HMIRExprKind::Switch { .. } => todo!("HMIRExprKind::Switch"),
        HMIRExprKind::Case { .. } => todo!("HMIRExprKind::Case"),
        HMIRExprKind::Match { .. } => todo!("HMIRExprKind::Match"),
        HMIRExprKind::Label { .. } => todo!("HMIRExprKind::Label"),
    }
}

pub(crate) fn eval_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    if matches!(context.context(), FnContext::Runtime(_)) {
        return comptime::type_expression(context, body, id);
    }
    let ty = context.deferred_type(id);
    let value = context.evaluate(
        HMIRResolver::type_memo(ty),
        body.expr(id).span(),
    )?;
    if matches!(value, Evaluation::Stop(_)) {
        return Err(hmir_error(
            body.expr(id).span(),
            &mir::COMPTIME_CONTROL_ESCAPE,
            (),
        ));
    }
    described_type(context, body, value, body.expr(id).span())
}

fn described_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: Evaluation<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<HMIRTypeID> {
    let partial = value.known().and_then(type_constant);
    let span = span.clone();
    value.and_then(context, body, partial, move |context, _, value, _| {
        let ty = require_type(context, value, &span)?;
        Ok(match complete::type_result(context, ty, &span)? {
            Evaluation::Ready(_) => Evaluation::Ready(ty),
            Evaluation::Incomplete(incomplete) => {
                Evaluation::Incomplete(incomplete.project(|value| {
                    type_constant(&value).expect("type completion returns a type")
                }))
            }
            Evaluation::Stop(stop) => Evaluation::Stop(stop),
        })
    })
}

pub(crate) fn type_constant(value: &HMIRValue) -> Option<HMIRTypeID> {
    match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Type(ty)) => Some(*ty),
        _ => None,
    }
}

fn require_type(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    let value = coerce::std_rvalue(context, value, span)?;
    match type_constant(&value) {
        Some(ty) => {
            context.prepare_type(ty, span)?;
            Ok(ty)
        }
        None => Err(hmir_error(
            span,
            &typecheck::EXPECTED_TYPE,
            format!(
                "expression of type '{}'",
                types::name(context.env(), value.ty())
            ),
        )),
    }
}

pub(crate) fn operand(
    _context: &mut LocalLoweringContext,
    _result: EvalResult<HMIRValue>,
    _span: &TokenRange,
) -> EvalResult<HMIRValue> {
    todo!("Propagate comptime exits; suppress runtime emission and provide a bottom operand")
}

pub(crate) fn quote(
    _context: &mut LocalLoweringContext,
    _body: &HMIRBody,
    _params: &[HMIRLocalID],
    _root: HMIRExprID,
) -> EvalResult<HMIRValue> {
    todo!("Capture an unevaluated HMIR body and frozen values as a staged DAG")
}

pub(crate) fn splice(
    _context: &mut LocalLoweringContext,
    _quote: HMIRValue,
    _args: &[HMIRValue],
) -> EvalResult<HMIRValue> {
    todo!("Materialize staged code into the runtime context with fresh local bindings")
}
