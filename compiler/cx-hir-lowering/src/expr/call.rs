use cx_hir::ast::{
    expression::{HIRBinOp, HIRExprKind, HIRExpression},
    types::{HIRType, HIRTypeKind},
};
use cx_hmir::{HMIRAggregateOp, HMIRConstant, HMIRExprID, HMIRExprKind};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    expr::{lower_expr, lower_identifier, lower_quote},
    resolve::GlobalSymbol,
    ty::{lower_constructor_sum, lower_type},
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

// 'piped' values are placed at the argument index their pipe names, 'append' after the rest
pub(crate) fn lower_call<'h>(
    cx: &mut BodyLowering<'_>,
    call: &'h HIRExpression,
    mut piped: Vec<(usize, &'h HIRExpression)>,
    append: Vec<&'h HIRExpression>,
) -> HMIRExprID {
    let HIRExprKind::BinOp { op, lhs, rhs } = &call.kind else {
        return cx.error(
            &call.range,
            &typecheck::UNEXPECTED_KIND,
            ("pipe target".into(), "a call".into()),
        );
    };
    match op {
        HIRBinOp::MethodCall => {
            let mut args = comma_separated(rhs)
                .into_iter()
                .map(|arg| (arg, false))
                .collect::<Vec<_>>();
            for (index, value) in piped.into_iter().rev() {
                if index > args.len() {
                    return cx.error(
                        &value.range,
                        &typecheck::INDEX_BOUNDS,
                        ("pipe".into(), index.to_string(), Some(args.len().to_string())),
                    );
                }
                args.insert(index, (value, true));
            }
            args.extend(append.into_iter().map(|arg| (arg, false)));
            lower_callee_call(cx, lhs, args, &call.range)
        }
        HIRBinOp::Pipe(index) => {
            piped.push((*index as usize, lhs.as_ref()));
            lower_call(cx, rhs, piped, append)
        }
        HIRBinOp::BackwardPipe => {
            let append = std::iter::once(rhs.as_ref()).chain(append).collect();
            lower_call(cx, lhs, piped, append)
        }
        _ => cx.error(
            &call.range,
            &typecheck::UNEXPECTED_KIND,
            ("pipe target".into(), "a call".into()),
        ),
    }
}

// Each argument is paired with whether a pipe supplied it
fn lower_callee_call(
    cx: &mut BodyLowering<'_>,
    callee: &HIRExpression,
    args: Vec<(&HIRExpression, bool)>,
    span: &TokenRange,
) -> HMIRExprID {
    let subjects = args.iter().map(|(_, piped)| *piped).collect::<Vec<_>>();
    let args = args.into_iter().map(|(arg, _)| arg).collect::<Vec<_>>();
    let name = match &callee.kind {
        HIRExprKind::Identifier { name } => name,
        HIRExprKind::ScopeAccess { base, member } => {
            let sum = lower_scope_base(cx, base);
            let value = lower_payload(cx, &args, span);
            return lower_construct(cx, sum, member.clone(), value, span);
        }
        _ => {
            let callee = lower_expr(cx, callee);
            return lower_call_args(cx, callee, &args, span);
        }
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
        Symbol::Global(GlobalSymbol::ComptimeFunction(def, code, staged)) => {
            let callee = cx.push(HMIRExprKind::Def(def), span);
            let args = args
                .iter()
                .enumerate()
                .map(|(index, arg)| {
                    // A piped subject is passed as code where the function takes code;
                    // every other argument is quoted by the caller with 'emit'
                    let quote = subjects[index]
                        && staged.get(index).copied().unwrap_or(false)
                        && !cx.is_comptime()
                        && !matches!(arg.kind, HIRExprKind::Emit { .. });
                    if quote {
                        lower_quote(cx, &[], arg, &arg.range)
                    } else {
                        lower_expr(cx, arg)
                    }
                })
                .collect();
            lower_comptime_call(cx, callee, code, args, span)
        }
        Symbol::Global(GlobalSymbol::Constructor(data, variant, _)) => {
            let sum = lower_constructor_sum(cx, &data.union_type, span);
            let value = lower_payload(cx, &args, span);
            lower_construct(cx, sum, variant, value, span)
        }
        _ => {
            let callee_id = lower_identifier(cx, name, &callee.range);
            lower_call_args(cx, callee_id, &args, span)
        }
    }
}

fn lower_call_args(
    cx: &mut BodyLowering<'_>,
    callee: HMIRExprID,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> HMIRExprID {
    let args = args.iter().map(|arg| lower_expr(cx, arg)).collect();
    cx.push(HMIRExprKind::Call { callee, args }, span)
}

// Outside comptime code the call runs at compile time, and code it returns is spliced in place
fn lower_comptime_call(
    cx: &mut BodyLowering<'_>,
    callee: HMIRExprID,
    code: bool,
    args: Vec<HMIRExprID>,
    span: &TokenRange,
) -> HMIRExprID {
    let call = cx.push(HMIRExprKind::Call { callee, args }, span);
    if cx.is_comptime() {
        return call;
    }

    let staged = cx.push(HMIRExprKind::Comptime(call), span);
    if !code {
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

// The type a 'base::variant(...)' constructor builds. A base left to the compiler, '_' or a
// generator applied to holes, is completed from the type the value is expected to have.
pub(crate) fn lower_scope_base(cx: &mut BodyLowering<'_>, base: &HIRExpression) -> HMIRExprID {
    match &base.kind {
        HIRExprKind::Hole => cx.hole(&base.range),
        HIRExprKind::Type(ty) => match &ty.kind {
            HIRTypeKind::Identifier {
                name,
                lookup,
                args: Some(args),
            } if args.iter().any(|arg| matches!(arg.kind, HIRExprKind::Hole)) => {
                let generator = HIRType {
                    kind: HIRTypeKind::Identifier {
                        name: name.clone(),
                        lookup: *lookup,
                        args: None,
                    },
                    specifiers: ty.specifiers,
                    range: ty.range.clone(),
                };
                lower_type(cx, &generator)
            }
            _ => lower_type(cx, ty),
        },
        _ => lower_expr(cx, base),
    }
}

fn lower_payload(
    cx: &mut BodyLowering<'_>,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> HMIRExprID {
    match args {
        [] => cx.push(HMIRExprKind::Constant(HMIRConstant::Unit), span),
        [value] => lower_expr(cx, value),
        _ => cx.error(
            span,
            &typecheck::ARGUMENT_COUNT,
            ("variant constructor".into(), 1, args.len(), false),
        ),
    }
}

pub(crate) fn lower_construct(
    cx: &mut BodyLowering<'_>,
    sum: HMIRExprID,
    variant: CXIdent,
    value: HMIRExprID,
    span: &TokenRange,
) -> HMIRExprID {
    cx.aggregate_op(
        HMIRAggregateOp::Initialize {
            ty: sum,
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
