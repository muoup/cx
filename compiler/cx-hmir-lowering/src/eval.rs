mod aggregate;
mod binding;
mod call;
mod coerce;
mod complete;
mod comptime;
mod control;
mod def;
mod literal;
mod op;
mod place;
mod storage;

pub(crate) mod flow;
pub(crate) mod types;

pub(crate) use comptime::eval_comptime;
pub(crate) use flow::EvalResult;

use cx_hmir::{
    HMIRBody, HMIRCoerceMode, HMIRControlOp, HMIRFunction, HMIROp, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::HMIRTypeID,
    unit::function::HMIRFnDefinition,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::MIRFunction;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext, RuntimeContext},
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
    function: &'hmir HMIRFunction,
) -> CXResult<MIRFunction> {
    let body = function.body();
    let signature = function.signature();
    let def = function
        .def()
        .expect("Runtime function lowered without a definition");
    let span = body.expr(def.root()).span();

    let mut staging = LocalLoweringContext::new(
        env,
        Some(signature),
        FnContext::Comptime(ComptimeContext::new()),
    );
    let evaluated = flow::top(types::eval_signature(&mut staging, body, signature))?;
    let prototype =
        types::lower_prototype(staging.env_mut(), function.prototype(), &evaluated, span)?;

    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = staging.child(Some(signature), runtime, None);

    emit::open_scope(&mut context, span.clone());
    let completion = runtime_definition(&mut context, function, def);
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
    def: &HMIRFnDefinition,
) -> EvalResult<HMIRValue> {
    let body = function.body();
    let root = body.expr(def.root());
    let Some(signature) = context.signature() else {
        unreachable!("runtime function lowered without a signature")
    };

    for (param, local) in signature.params().iter().zip(def.params()) {
        let decl = body.local(*local);
        let ty = ready!(eval_type(context, body, *param.ty())?.description());
        let value = runtime::parameter(context, ty, decl.name().cloned(), decl.span())?;
        let reference = types::reference(context.env_mut(), ty, decl.span());
        let value = value.with_ty(reference);

        emit::storage::bind_local(context, *local, value);
    }

    match root.kind() {
        HMIRExprKind::Block { .. } => {
            ready!(eval(context, body, def.root()));
            let is_main = function.prototype().symbol_name().as_str() == "main";
            control::fallthrough(context, body, is_main, root.span())
        }
        _ => {
            let value = eval(context, body, def.root())?;
            control::return_value(context, body, Some(value), false, root.span())
        }
    }
}

pub(crate) fn eval_comptime_function<'hmir>(
    _parent: &mut LocalLoweringContext<'_, '_, 'hmir>,
    _function: &'hmir HMIRFunction,
    _args: &[HMIRConstant],
) -> CXResult<HMIRConstant> {
    todo!("Evaluate a comptime child, catch its return, and freeze its result")
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
        HMIRExprKind::TypeRef(ty) => complete::type_result(context, *ty, span),
        HMIRExprKind::Type(ty) => {
            let ty = eval_declared_type(context, body, id, ty)?;
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

            HMIROp::Type(op) => {
                let ty = eval_type_op(context, body, span, op)?;
                complete::type_result(context, ty, span)
            }
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

        HMIRExprKind::Def(def) => eval_def(context, span, def),
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
    let value = eval(context, body, id)?;
    if matches!(value, Evaluation::Stop(_)) {
        return Err(hmir_error(
            body.expr(id).span(),
            &cx_log::catalogue::mir::COMPTIME_CONTROL_ESCAPE,
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
        Ok(Evaluation::Ready(require_type(context, value, &span)?))
    })
}

fn type_constant(value: &HMIRValue) -> Option<HMIRTypeID> {
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
        Some(ty) => Ok(ty),
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
