mod binding;
mod coerce;
mod constant;
mod control;
mod flow;
mod op;
mod place;
mod promote;
pub(crate) mod types;

pub(crate) use flow::EvalResult;

use cx_hmir::{
    HMIRBody, HMIRControlOp, HMIRFunction, HMIROp,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind, constant::HMIRConstant},
    ty::HMIRTypeID,
    unit::function::HMIRFnDefinition,
    value::HMIRValue,
};
use cx_log::CXResult;
use cx_mir::MIRFunction;
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::{
        HMIREnvironment,
        function::{FnContext, FnLoweringContext, RuntimeContext},
    },
    eval::{
        binding::{eval_block, eval_let, eval_local},
        coerce::eval_coerce,
        constant::eval_constant,
        control::{eval_if, eval_loop_exit, eval_return, eval_while},
        flow::ComptimeStop,
        op::{eval_binary, eval_unary},
        place::{eval_address_of, eval_assign, eval_dereference},
    },
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

    let mut staging = FnLoweringContext::for_signature(env, signature);
    let prototype = flow::top(types::lower_prototype(
        &mut staging,
        body,
        function.prototype(),
    ))?;

    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = FnLoweringContext::new(env, signature, runtime);

    emit::open_scope(&mut context, span.clone());
    let completion = runtime_definition(&mut context, function, def);
    emit::close_scope(&mut context, span.clone());
    flow::top(flow::catch_diverged(completion))?;

    Ok(runtime::finish(context))
}

fn runtime_definition(
    context: &mut FnLoweringContext,
    function: &HMIRFunction,
    def: &HMIRFnDefinition,
) -> EvalResult<HMIRValue> {
    let body = function.body();
    let root = body.expr(def.root());

    for (param, local) in context.signature().params().iter().zip(def.params()) {
        let decl = body.local(*local);
        let ty = eval_type(context, body, param.ty())?;
        let value = runtime::parameter(
            context,
            ty,
            decl.name().cloned(),
            decl.nodrop(),
            decl.span(),
        )?;

        context.insert_local(*local, value);
    }

    let value = eval(context, body, def.root())?;
    match root.kind() {
        HMIRExprKind::Block { .. } => {
            let is_main = function.prototype().symbol_name().as_str() == "main";
            control::fallthrough(context, body, is_main, root.span())
        }
        _ => control::return_value(context, body, Some(value), root.span()),
    }
}

pub(crate) fn eval_comptime_function<'hmir>(
    _parent: &mut FnLoweringContext<'_, '_, 'hmir>,
    _function: &'hmir HMIRFunction,
    _args: &[HMIRConstant],
) -> CXResult<HMIRConstant> {
    todo!("Evaluate a comptime child, catch its return, and freeze its result")
}

pub(crate) fn eval(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let expr = body.expr(expr);
    let span = expr.span();

    match expr.kind() {
        HMIRExprKind::Constant(constant) => eval_constant(context, span, constant),
        HMIRExprKind::Local(local) => eval_local(context, span, *local),
        HMIRExprKind::Let { local, initializer } => {
            eval_let(context, body, span, *local, *initializer)
        }
        HMIRExprKind::Block { statements } => eval_block(context, body, span, statements),

        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => eval_if(context, body, span, *condition, *then_branch, *else_branch),
        HMIRExprKind::While {
            condition,
            body: loop_body,
            pre_eval,
        } => eval_while(context, body, span, *condition, *loop_body, *pre_eval),

        HMIRExprKind::Op(op) => match op {
            HMIROp::BinOp { op, lhs, rhs } => eval_binary(context, body, span, *op, *lhs, *rhs),
            HMIROp::UnOp { op, operand } => eval_unary(context, body, span, *op, *operand),
            HMIROp::Coerce {
                mode,
                value,
                target,
            } => eval_coerce(context, body, span, *mode, *value, *target),

            HMIROp::Control(HMIRControlOp::Return(value)) => {
                eval_return(context, body, span, *value)
            }
            HMIROp::Control(HMIRControlOp::Break) => eval_loop_exit(context, ComptimeStop::Break),
            HMIROp::Control(HMIRControlOp::Continue) => {
                eval_loop_exit(context, ComptimeStop::Continue)
            }

            HMIROp::Assign { target, op, value } => {
                eval_assign(context, body, span, *target, *op, *value)
            }
            HMIROp::AddressOf(operand) => eval_address_of(context, body, span, *operand),
            HMIROp::Dereference(pointer) => eval_dereference(context, body, span, *pointer),

            HMIROp::Type(_) => todo!("HMIROp::Type"),
            HMIROp::Control(control) => todo!("HMIROp::Control({})", control.path()),
            HMIROp::OwnershipOp(ownership) => {
                todo!("HMIROp::OwnershipOp({})", ownership.path())
            }
            HMIROp::AggregateOp(_) => todo!("HMIROp::AggregateOp"),
        },

        HMIRExprKind::Def(_) => todo!("HMIRExprKind::Def"),
        HMIRExprKind::Hole(_) => todo!("HMIRExprKind::Hole"),
        HMIRExprKind::Comptime(_) => todo!("HMIRExprKind::Comptime"),
        HMIRExprKind::Quote { .. } => todo!("HMIRExprKind::Quote"),
        HMIRExprKind::Splice { .. } => todo!("HMIRExprKind::Splice"),
        HMIRExprKind::Intrinsic(_) => todo!("HMIRExprKind::Intrinsic"),
        HMIRExprKind::Error(_) => todo!("HMIRExprKind::Error"),
        HMIRExprKind::Call { .. } => todo!("HMIRExprKind::Call"),
        HMIRExprKind::For { .. } => todo!("HMIRExprKind::For"),
        HMIRExprKind::Switch { .. } => todo!("HMIRExprKind::Switch"),
        HMIRExprKind::Case { .. } => todo!("HMIRExprKind::Case"),
        HMIRExprKind::Match { .. } => todo!("HMIRExprKind::Match"),
        HMIRExprKind::Label { .. } => todo!("HMIRExprKind::Label"),
    }
}

pub(crate) fn eval_comptime(
    _context: &mut FnLoweringContext,
    _body: &HMIRBody,
    _expr: HMIRExprID,
) -> CXResult<HMIRConstant> {
    todo!("Evaluate an isolated comptime expression with frozen inputs")
}

pub(crate) fn eval_type(
    _context: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    match body.expr(expr).kind() {
        HMIRExprKind::Constant(HMIRConstant::Type(ty)) => Ok(*ty),
        _ => todo!("Evaluate a type expression using the available comptime bindings"),
    }
}

pub(crate) fn operand(
    _context: &mut FnLoweringContext,
    _result: EvalResult<HMIRValue>,
    _span: &TokenRange,
) -> EvalResult<HMIRValue> {
    todo!("Propagate comptime exits; suppress runtime emission and provide a bottom operand")
}

pub(crate) fn quote(
    _context: &mut FnLoweringContext,
    _body: &HMIRBody,
    _params: &[HMIRLocalID],
    _root: HMIRExprID,
) -> EvalResult<HMIRValue> {
    todo!("Capture an unevaluated HMIR body and frozen values as a staged DAG")
}

pub(crate) fn splice(
    _context: &mut FnLoweringContext,
    _quote: HMIRValue,
    _args: &[HMIRValue],
) -> EvalResult<HMIRValue> {
    todo!("Materialize staged code into the runtime context with fresh local bindings")
}
