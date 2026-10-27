pub(crate) mod deduce;
mod expr;
pub(crate) mod flow;

pub(crate) use expr::{
    aggregate, binding, call, coerce, comptime, control, name, op, place, staged, types,
};
pub(crate) use flow::EvalResult;

use cx_hmir::{
    HMIRBody, HMIROp,
    expr::{HMIRExprID, HMIRExprKind},
    ty::HMIRTypeID,
};
use cx_log::{CXResult, catalogue::mir};

use crate::{
    env::{
        function::{FnContext, LocalLoweringContext, runtime::Emission},
        resolver::HMIRResolver,
    },
    eval::flow::Evaluation,
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let expr = body.expr(id);
    let span = expr.span();
    let selected = match context.context() {
        FnContext::Comptime(comptime) => comptime.selected_case(),
        FnContext::Runtime(_) => None,
    };
    let result = match expr.kind() {
        _ if let Some(selected) = selected => control::seek(context, body, id, selected),
        HMIRExprKind::Literal(literal) => Ok(Evaluation::Ready(expr::eval_literal(context, span, literal))),
        HMIRExprKind::Type(_) => {
            let ty = types::eval_declared_type(context, body, id)?;
            deduce::type_result(context, ty, span)
        }
        HMIRExprKind::Local(local) => binding::eval_local(context, body, span, *local),
        HMIRExprKind::Let { local, initializer } => {
            binding::eval_let(context, body, span, *local, *initializer)
        }
        HMIRExprKind::Block { statements, scoped, yields } => {
            binding::eval_block(context, body, span, statements, *scoped, *yields)
        }

        HMIRExprKind::If { condition, then_branch, else_branch } => {
            control::eval_if(context, body, span, *condition, *then_branch, *else_branch)
        }
        HMIRExprKind::While { condition, body: loop_body, pre_eval } => {
            control::eval_while(context, body, span, *condition, *loop_body, None, *pre_eval)
        }
        HMIRExprKind::For { init, condition, increment, body: loop_body } => {
            control::eval_for(context, body, span, *init, *condition, *increment, *loop_body)
        }
        HMIRExprKind::Switch { condition, body: switch_body } => {
            control::eval_switch(context, body, span, *condition, *switch_body)
        }
        HMIRExprKind::Case { body: statement, .. } => control::eval_case(context, body, id, *statement),
        HMIRExprKind::Match { scrutinee, subject, arms } => {
            control::eval_match(context, body, span, *scrutinee, *subject, arms)
        }
        HMIRExprKind::Label { name, body: statement } => {
            control::eval_label(context, body, span, name, *statement)
        }

        HMIRExprKind::Op(op) => match op {
            HMIROp::BinOp { op, lhs, rhs } => op::eval_binary(context, body, span, *op, *lhs, *rhs),
            HMIROp::UnOp { op, operand } => op::eval_unary(context, body, span, *op, *operand),
            HMIROp::Coerce { mode, value, target } => {
                coerce::eval_coerce(context, body, span, *mode, *value, *target)
            }
            HMIROp::Control(control) => control::eval_control(context, body, span, control),
            HMIROp::Assign { target, op, value } => {
                place::eval_assign(context, body, span, *target, *op, *value)
            }
            HMIROp::AddressOf(operand) => place::eval_address_of(context, body, span, *operand),
            HMIROp::Dereference(pointer) => place::eval_dereference(context, body, span, *pointer),
            HMIROp::OwnershipOp(op) => place::eval_ownership(context, body, span, op),
            HMIROp::Type(op) => types::eval_type_op(context, body, span, op),
            HMIROp::AggregateOp(aggregate) => aggregate::eval_aggregate(context, body, span, aggregate),
        },

        HMIRExprKind::Def(def) => name::eval_def(context, span, *def),
        HMIRExprKind::Name { name, tag } => name::eval_name(context, span, name, *tag),
        HMIRExprKind::ScopeAccess { base, member } => {
            name::eval_scope_access(context, body, span, *base, member)
        }
        HMIRExprKind::Piped(value) => eval(context, body, *value),
        HMIRExprKind::Call { callee, args } => call::eval_call(context, body, span, *callee, args),

        HMIRExprKind::Hole(_) => deduce::hole(context, span),
        HMIRExprKind::Comptime(inner) => Ok(Evaluation::Ready(eval_comptime(context, body, *inner, None)?)),
        HMIRExprKind::Quote { params, body: root } => staged::capture(context, body, params, *root),
        HMIRExprKind::Splice { quote, args } => staged::eval_splice(context, body, span, *quote, args),
        HMIRExprKind::Intrinsic(op) => expr::eval_intrinsic(context, body, span, op),
        HMIRExprKind::Error(_) => todo!("HMIRExprKind::Error"),
    }?;

    let partial = result.known().cloned();
    result.and_then(context, body, partial, |context, _, value, _| {
        Ok(match context.context() {
            FnContext::Runtime(runtime) if runtime.emission() == Emission::Suppressed => {
                Evaluation::diverged()
            }
            _ => Evaluation::Ready(value),
        })
    })
}

pub(crate) fn eval_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    let span = body.expr(id).span();
    if context.is_runtime() {
        return comptime::frozen(context, span, |child| eval_type(child, body, id));
    }
    let ty = context.deferred_type(id);
    match context.evaluate(HMIRResolver::type_memo(ty), span)? {
        Evaluation::Stop(_) => Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ())),
        value => deduce::described_type(context, body, value, span),
    }
}

pub(crate) fn eval_comptime(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> CXResult<HMIRValue> {
    match comptime::freeze(context, body, expr, expected)? {
        Evaluation::Ready(value) => Ok(value),
        Evaluation::Incomplete(incomplete) => Err(incomplete.error()),
        Evaluation::Stop(_) => unreachable!("a frozen evaluation diagnoses its stops"),
    }
}
