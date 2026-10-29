use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression};
use cx_hmir::expr::{HMIRExprID, HMIRExprKind};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    expr::{lower_expr, lower_identifier},
    log::log_hir_error,
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

type Named<'h> = Vec<(&'h CXIdent, &'h HIRExpression)>;

// The positional arguments in order, apart from those that name the parameter they bind
fn split_arguments<'h>(args: impl IntoIterator<Item = &'h HIRExpression>) -> (Vec<&'h HIRExpression>, Named<'h>) {
    let mut positional = Vec::new();
    let mut named = Vec::new();
    for arg in args {
        match &arg.kind {
            HIRExprKind::NamedArgument { name, value } => named.push((name, value.as_ref())),
            _ => positional.push(arg),
        }
    }
    (positional, named)
}

fn lower_named(context: &mut BodyLowering<'_>, named: Named<'_>) -> CXResult<Vec<(CXIdent, HMIRExprID)>> {
    named
        .into_iter()
        .map(|(name, value)| Ok((name.clone(), lower_expr(context, value)?)))
        .collect()
}

pub(crate) fn lower_arguments<'h>(
    context: &mut BodyLowering<'_>,
    args: impl IntoIterator<Item = &'h HIRExpression>,
) -> CXResult<(Vec<HMIRExprID>, Vec<(CXIdent, HMIRExprID)>)> {
    let (positional, named) = split_arguments(args);
    let positional = lower_args(context, &positional)?;
    Ok((positional, lower_named(context, named)?))
}

// 'piped' values are placed at the argument index their pipe names, 'append' after the rest
pub(crate) fn lower_call<'h>(
    context: &mut BodyLowering<'_>,
    call: &'h HIRExpression,
    mut piped: Vec<(usize, &'h HIRExpression)>,
    append: Vec<&'h HIRExpression>,
) -> CXResult<HMIRExprID> {
    let HIRExprKind::BinOp { op, lhs, rhs } = &call.kind else {
        return log_hir_error(
            &call.range,
            &typecheck::UNEXPECTED_KIND,
            ("pipe target".into(), "a call".into()),
        );
    };
    match op {
        HIRBinOp::MethodCall => {
            let (args, named) = split_arguments(comma_separated(rhs));
            let mut args = args.into_iter().map(|arg| (arg, false)).collect::<Vec<_>>();
            for (index, value) in piped.into_iter().rev() {
                if index > args.len() {
                    return log_hir_error(
                        &value.range,
                        &typecheck::INDEX_BOUNDS,
                        (
                            "pipe".into(),
                            index.to_string(),
                            Some(args.len().to_string()),
                        ),
                    );
                }
                args.insert(index, (value, true));
            }
            args.extend(append.into_iter().map(|arg| (arg, false)));
            lower_callee_call(context, lhs, args, named, &call.range)
        }
        HIRBinOp::Pipe(index) => {
            // REVIEW: We might want to consider consolidating this into HMIR eval, i.e. have this be a standard operation
            piped.push((*index as usize, lhs.as_ref()));
            lower_call(context, rhs, piped, append)
        }
        HIRBinOp::BackwardPipe => {
            let append = std::iter::once(rhs.as_ref()).chain(append).collect();
            lower_call(context, lhs, piped, append)
        }
        _ => log_hir_error(
            &call.range,
            &typecheck::UNEXPECTED_KIND,
            ("pipe target".into(), "a call".into()),
        ),
    }
}

// Each argument is paired with whether a pipe supplied it
fn lower_callee_call(
    context: &mut BodyLowering<'_>,
    callee: &HIRExpression,
    args: Vec<(&HIRExpression, bool)>,
    named: Named<'_>,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let subjects = args.iter().map(|(_, piped)| *piped).collect::<Vec<_>>();
    let args = args.into_iter().map(|(arg, _)| arg).collect::<Vec<_>>();
    let name = match &callee.kind {
        HIRExprKind::Identifier { name } => name,
        _ => {
            let callee = lower_expr(context, callee)?;
            let args = lower_args(context, &args)?;
            let named = lower_named(context, named)?;
            return Ok(context.push(HMIRExprKind::Call { callee, args, named }, span));
        }
    };

    let unnamed = |named: &Named<'_>, feature: &str| match named.first() {
        Some((_, value)) => log_hir_error(
            &value.range,
            &typecheck::INVALID_CONTEXT,
            ("a named argument".into(), feature.into()),
        ),
        None => Ok(()),
    };
    if let Some(root) = name.root_name_ref()
        && let Some(intrinsic) = lower_builtin_call(context, root.as_str(), &args, span)?
    {
        unnamed(&named, "a builtin call")?;
        return Ok(intrinsic);
    }

    match context.lookup(name, None) {
        Symbol::Local(binding) if binding.is_quoted() => {
            unnamed(&named, "a staged expression call")?;
            let quote = context.push(HMIRExprKind::Local(binding.local()), &callee.range);
            let args = lower_args(context, &args)?;
            Ok(context.push(HMIRExprKind::Splice { quote, args }, span))
        }
        _ => {
            let callee = lower_identifier(context, name, &callee.range)?;
            let args = args
                .iter()
                .zip(&subjects)
                .map(|(arg, piped)| {
                    let value = lower_expr(context, arg)?;
                    Ok(if *piped {
                        context.push(HMIRExprKind::Piped(value), &arg.range)
                    } else {
                        value
                    })
                })
                .collect::<CXResult<_>>()?;
            let named = lower_named(context, named)?;
            Ok(context.push(HMIRExprKind::Call { callee, args, named }, span))
        }
    }
}

fn lower_args(
    context: &mut BodyLowering<'_>,
    args: &[&HIRExpression],
) -> CXResult<Vec<HMIRExprID>> {
    args.iter().map(|arg| lower_expr(context, arg)).collect()
}

fn lower_builtin_call(
    context: &mut BodyLowering<'_>,
    name: &str,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> CXResult<Option<HMIRExprID>> {
    let intrinsic = match (name, args) {
        ("va_start" | "__builtin_va_start", [list, last]) => VAIntrinsic::Start {
            list: lower_expr(context, list)?,
            last: lower_expr(context, last)?,
        },
        ("va_end" | "__builtin_va_end", [list]) => VAIntrinsic::End {
            list: lower_expr(context, list)?,
        },
        _ => return Ok(None),
    };

    Ok(Some(context.push(
        HMIRExprKind::Intrinsic(Intrinsic::VA(intrinsic)),
        span,
    )))
}
