use cx_hir::ast::{
    expression::{HIRBinOp, HIRExprKind, HIRExpression},
    function::HIRComptimeFnPrototype,
    template::HIRTemplateInput,
    types::HIRType,
};
use cx_hmir::{HMIRAggregateOp, HMIRConstant, HMIRDefRef, HMIRExprID, HMIRExprKind};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    expr::{lower_expr, lower_identifier, lower_quote},
    resolve::GlobalSymbol,
    ty::{lower_constructor_sum, lower_template_args},
};

fn comma_separated(expr: &HIRExpression) -> Vec<&HIRExpression> {
    match &expr.kind {
        HIRExprKind::Void => Vec::new(),
        HIRExprKind::BinOp {
            op: HIRBinOp::Comma,
            lhs,
            rhs,
        } => {
            let mut exprs = comma_separated(lhs);
            exprs.push(rhs);
            exprs
        }
        _ => vec![expr],
    }
}

pub(crate) fn lower_call<'h>(
    cx: &mut BodyLowering<'_>,
    call: &'h HIRExpression,
    prepend: Vec<&'h HIRExpression>,
    append: Vec<&'h HIRExpression>,
) -> HMIRExprID {
    let HIRExprKind::BinOp { op, lhs, rhs } = &call.kind else {
        return cx.error(&call.range);
    };
    match op {
        HIRBinOp::MethodCall => {
            let args = prepend
                .into_iter()
                .chain(comma_separated(rhs))
                .chain(append)
                .collect();
            lower_callee_call(cx, lhs, args, &call.range)
        }
        HIRBinOp::Pipe => {
            let prepend = std::iter::once(lhs.as_ref()).chain(prepend).collect();
            lower_call(cx, rhs, prepend, append)
        }
        HIRBinOp::BackwardPipe => {
            let append = std::iter::once(rhs.as_ref()).chain(append).collect();
            lower_call(cx, lhs, prepend, append)
        }
        _ => cx.error(&call.range),
    }
}

fn lower_callee_call(
    cx: &mut BodyLowering<'_>,
    callee: &HIRExpression,
    args: Vec<&HIRExpression>,
    span: &TokenRange,
) -> HMIRExprID {
    let HIRExprKind::Identifier {
        name,
        template_input,
    } = &callee.kind
    else {
        let callee = lower_expr(cx, callee);
        return lower_call_args(cx, callee, Vec::new(), &args, span);
    };

    if let Some(root) = name.root_name_ref()
        && let Some(intrinsic) = lower_builtin_call(cx, root.as_str(), &args, span)
    {
        return intrinsic;
    }

    match cx.lookup(name, None) {
        Symbol::Local(binding) if binding.is_quoted() => {
            let quote = cx.push(HMIRExprKind::Local(binding.local()), &callee.range);
            let args = args.iter().map(|arg| lower_expr(cx, arg)).collect();
            cx.push(HMIRExprKind::Splice { quote, args }, span)
        }
        Symbol::Global(GlobalSymbol::ComptimeFunction(def, prototype)) => {
            lower_comptime_call(cx, def, &prototype, template_input.as_ref(), &args, span)
        }
        Symbol::Global(GlobalSymbol::Constructor(data, variant)) => {
            let value = match args.as_slice() {
                [] => cx.push(HMIRExprKind::Constant(HMIRConstant::Unit), span),
                [value] => lower_expr(cx, value),
                _ => cx.error(span),
            };
            lower_construct(
                cx,
                &data.union_type,
                template_input.as_ref(),
                variant,
                value,
                span,
            )
        }
        _ => {
            let callee_id = lower_identifier(cx, name, None, &callee.range);
            let leading = lower_template_args(cx, template_input.as_ref());
            lower_call_args(cx, callee_id, leading, &args, span)
        }
    }
}

fn lower_call_args(
    cx: &mut BodyLowering<'_>,
    callee: HMIRExprID,
    mut leading: Vec<HMIRExprID>,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> HMIRExprID {
    leading.extend(args.iter().map(|arg| lower_expr(cx, arg)));
    cx.push(
        HMIRExprKind::Call {
            callee,
            args: leading,
        },
        span,
    )
}

fn lower_comptime_call(
    cx: &mut BodyLowering<'_>,
    def: HMIRDefRef,
    prototype: &HIRComptimeFnPrototype,
    template_input: Option<&HIRTemplateInput>,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> HMIRExprID {
    let callee = cx.push(HMIRExprKind::Def(def), span);
    let mut lowered = lower_template_args(cx, template_input);
    for (index, arg) in args.iter().enumerate() {
        let quoted = prototype
            .params
            .get(index)
            .is_some_and(|param| param.value_type.expr);
        lowered.push(if quoted && !cx.is_comptime() {
            lower_quote_argument(cx, arg)
        } else {
            lower_expr(cx, arg)
        });
    }
    let call = cx.push(
        HMIRExprKind::Call {
            callee,
            args: lowered,
        },
        span,
    );
    if cx.is_comptime() {
        return call;
    }

    let staged = cx.push(HMIRExprKind::Comptime(call), span);
    if !prototype.return_type.expr {
        return staged;
    }
    cx.push(
        HMIRExprKind::Splice {
            quote: staged,
            args: Vec::new(),
        },
        span,
    )
}

fn lower_quote_argument(cx: &mut BodyLowering<'_>, arg: &HIRExpression) -> HMIRExprID {
    match &arg.kind {
        HIRExprKind::ParamStagedExpression { .. } | HIRExprKind::Emit { .. } => lower_expr(cx, arg),
        _ => lower_quote(cx, &[], arg, &arg.range),
    }
}

pub(crate) fn lower_construct(
    cx: &mut BodyLowering<'_>,
    union_type: &HIRType,
    template_input: Option<&HIRTemplateInput>,
    variant: CXIdent,
    value: HMIRExprID,
    span: &TokenRange,
) -> HMIRExprID {
    let ty = lower_constructor_sum(cx, union_type, template_input, span);
    cx.aggregate_op(
        HMIRAggregateOp::Initialize {
            ty,
            fields: vec![(Some(variant), value)],
        },
        span,
    )
}

fn lower_builtin_call(
    cx: &mut BodyLowering<'_>,
    name: &str,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> Option<HMIRExprID> {
    let intrinsic = match (name, args) {
        ("va_start" | "__builtin_va_start", [list, last]) => VAIntrinsic::Start {
            list: lower_expr(cx, list),
            last: lower_expr(cx, last),
        },
        ("va_end" | "__builtin_va_end", [list]) => VAIntrinsic::End {
            list: lower_expr(cx, list),
        },
        _ => return None,
    };
    Some(cx.push(HMIRExprKind::Intrinsic(Intrinsic::VA(intrinsic)), span))
}
