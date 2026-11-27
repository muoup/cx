pub(crate) mod deduce;
pub(crate) mod flow;
pub(crate) mod storage;
pub(crate) mod symbol;

mod expr;
mod subject;

pub(crate) use expr::{
    aggregate, arithmetic, binding, call, coerce, comptime, control, name, place, staged, types,
};
pub(crate) use flow::EvalResult;

use chl_hmir::{
    HMIRBodyContext, HMIROp,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRExprTypeKind, HMIRTypeID},
};
use chl_log::{CHLResult, catalogue::mir};
use chl_pipeline_data::hmir::facts::HMIRTypeSubject;

use crate::{
    env::{
        function::{FnContext, LocalLoweringContext, runtime::Emission},
        resolver::NominalSite,
    },
    eval::flow::Evaluation,
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    id: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    eval_expected(context, body, id, None)
}

pub(crate) fn eval_expected(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    id: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let expr = body.expr(id);
    let span = expr.span();
    let selected = match context.context() {
        FnContext::Comptime(comptime) => comptime.selected_case(),
        FnContext::Runtime(_) => None,
    };
    let result = match expr.kind() {
        _ if let Some(selected) = selected => control::seek(context, body, id, selected),
        HMIRExprKind::Literal(literal) => Ok(Evaluation::Ready(expr::eval_literal(
            context, span, literal,
        ))),
        HMIRExprKind::Type(ty) if let Some(element) = types::hole_bounded(body, ty) => {
            deduce::hole_bound(context, body, ty, element, span)
        }
        HMIRExprKind::Type(ty) if let HMIRExprTypeKind::Strong { inner, binding } = ty.kind() => {
            if binding.is_some() {
                let aggregate =
                    types::aggregate(body, id).expect("a scoped tag defines an aggregate");
                let ty = types::define_aggregate(context, body, aggregate, None)?;
                return deduce::type_result(context, ty, span);
            }
            let inner = eval_type(context, body, *inner)?.description()?;
            let span = span.clone();
            inner.and_then(context, body, move |context, _, inner| {
                let site = NominalSite {
                    unit: context.source(),
                    expression: id,
                    specialization: context.nominal_scope(),
                    inner,
                };
                let ty = context.env_mut().resolver_mut().nominal_at(site, &span);
                deduce::type_result(context, ty, &span)
            })
        }
        HMIRExprKind::Type(ty) => {
            let ty = types::construct_type(context, body, ty, None)?;
            deduce::type_result(context, ty, span)
        }
        HMIRExprKind::Local(local) => binding::eval_local(context, body, span, *local),
        HMIRExprKind::Let { local, initializer } => {
            binding::eval_let(context, body, span, *local, *initializer)
        }
        HMIRExprKind::Block {
            statements,
            scoped,
            yields,
        } => binding::eval_block(context, body, span, statements, *scoped, *yields, expected),

        HMIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => control::eval_if(
            context,
            body,
            span,
            *condition,
            *then_branch,
            *else_branch,
            expected,
        ),
        HMIRExprKind::While {
            condition,
            body: loop_body,
            pre_eval,
        } => control::eval_while(context, body, span, *condition, *loop_body, None, *pre_eval),
        HMIRExprKind::For {
            init,
            condition,
            increment,
            body: loop_body,
        } => control::eval_for(
            context, body, span, *init, *condition, *increment, *loop_body,
        ),
        HMIRExprKind::Switch {
            condition,
            body: switch_body,
        } => control::eval_switch(context, body, span, *condition, *switch_body),
        HMIRExprKind::Case {
            body: statement, ..
        } => control::eval_case(context, body, id, *statement),
        HMIRExprKind::Match {
            scrutinee,
            subject,
            arms,
        } => control::eval_match(context, body, span, *scrutinee, *subject, arms, expected),
        HMIRExprKind::Label {
            name,
            body: statement,
        } => control::eval_label(context, body, span, name, *statement),

        HMIRExprKind::Op(op) => match op {
            HMIROp::BinOp { op, lhs, rhs } => {
                arithmetic::eval_binary(context, body, span, *op, *lhs, *rhs)
            }
            HMIROp::UnOp { op, operand } => {
                arithmetic::eval_unary(context, body, span, *op, *operand)
            }
            HMIROp::Coerce {
                mode,
                value,
                target,
            } => coerce::eval_coerce(context, body, span, *mode, *value, *target),
            HMIROp::EnumMember { value, target } => {
                symbol::eval_enum_member(context, body, span, *value, *target)
            }
            HMIROp::Control(control) => {
                control::eval_control(context, body, span, control, expected)
            }
            HMIROp::Assign { target, op, value } => {
                place::eval_assign(context, body, span, *target, *op, *value)
            }
            HMIROp::AddressOf(operand) => place::eval_address_of(context, body, span, *operand),
            HMIROp::Dereference(pointer) => place::eval_dereference(context, body, span, *pointer),
            HMIROp::StrConcat { lhs, rhs } => expr::string::concat(context, body, span, *lhs, *rhs),
            HMIROp::OwnershipOp(op) => place::eval_ownership(context, body, span, op),
            HMIROp::Type(op) => types::query::eval_type_op(context, body, span, op),
            HMIROp::AggregateOp(aggregate) => {
                aggregate::eval_aggregate(context, body, span, aggregate)
            }
        },

        HMIRExprKind::Def(def) => name::eval_def(context, span, *def),
        HMIRExprKind::Name { name, tag } => name::eval_name(context, span, name, *tag),
        HMIRExprKind::ScopeAccess { base, member } => {
            name::eval_scope_access(context, body, span, *base, member)
        }
        HMIRExprKind::Piped(value) => eval_expected(context, body, *value, expected),
        HMIRExprKind::Call {
            callee,
            args,
            named,
        } => call::eval_call(context, body, span, *callee, args, named),

        HMIRExprKind::Hole(_) => deduce::eval_hole(context, span),
        HMIRExprKind::Comptime(inner) => Ok(Evaluation::Ready(eval_comptime(
            context, body, *inner, None,
        )?)),
        HMIRExprKind::Quote { params, body: root } => {
            staged::eval_quote(context, body, params, *root, span, expected)
        }
        HMIRExprKind::Splice { quote, args } => {
            staged::eval_splice(context, body, span, *quote, args)
        }
        HMIRExprKind::Intrinsic(op) => expr::intrinsic::eval_intrinsic(context, body, span, op),
    }?;

    result.chain(
        context,
        body,
        None,
        |value| Some(value.clone()),
        move |context, _, value, _| match context.context() {
            FnContext::Runtime(runtime) if runtime.emission() == Emission::Suppressed => {
                flow::diverged()
            }
            FnContext::Runtime(_) => {
                context.env_mut().record_type(HMIRTypeSubject::Expr(id), &value);
                Ok(Evaluation::Ready(value))
            }
            FnContext::Comptime(_) => Ok(Evaluation::Ready(value)),
        },
    )
}

pub(crate) fn eval_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    id: HMIRExprID,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let span = body.expr(id).span();
    if context.is_runtime() {
        return comptime::frozen(context, span, |child| eval_type(child, body, id));
    }
    match eval(context, body, id)
        .and_then(|value| deduce::described_type(context, body, value, span))
    {
        Err(flow::EvalExit::Stop(_)) => {
            Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into())
        }
        result => result,
    }
}

pub(crate) fn eval_comptime(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> CHLResult<HMIRValue> {
    flow::top(comptime::freeze(context, body, expr, expected))
}
