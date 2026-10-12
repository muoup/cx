use cx_hir::ast::{
    expression::{HIRBinOp, HIRExprKind, HIRExpression},
    types::{HIRType, HIRTypeKind},
};
use cx_hmir::{
    HMIRAggregateOp,
    expr::{HMIRExprID, HMIRExprKind, constant::HMIRConstant},
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    expr::{lower_expr, lower_identifier, lower_quote},
    log::log_hir_error,
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
            let mut args = comma_separated(rhs)
                .into_iter()
                .map(|arg| (arg, false))
                .collect::<Vec<_>>();
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
            lower_callee_call(context, lhs, args, &call.range)
        }
        HIRBinOp::Pipe(index) => {
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
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let subjects = args.iter().map(|(_, piped)| *piped).collect::<Vec<_>>();
    let args = args.into_iter().map(|(arg, _)| arg).collect::<Vec<_>>();
    let name = match &callee.kind {
        HIRExprKind::Identifier { name } => name,
        HIRExprKind::ScopeAccess { base, member } => {
            let sum = lower_scope_base(context, base)?;
            let value = lower_payload(context, &args, span)?;
            return Ok(lower_construct(context, sum, member.clone(), value, span));
        }
        _ => {
            let callee = lower_expr(context, callee)?;
            return lower_call_args(context, callee, &args, span);
        }
    };

    if let Some(root) = name.root_name_ref()
        && let Some(intrinsic) = lower_builtin_call(context, root.as_str(), &args, span)?
    {
        return Ok(intrinsic);
    }

    match context.lookup(name, None, &callee.range)? {
        Symbol::Local(binding) if binding.is_quoted() => {
            let quote = context.push(HMIRExprKind::Local(binding.local()), &callee.range);
            let args = lower_args(context, &args)?;
            Ok(context.push(HMIRExprKind::Splice { quote, args }, span))
        }
        Symbol::Global(GlobalSymbol::ComptimeFunction(def, code, staged)) => {
            let callee = context.push(HMIRExprKind::Def(def), span);
            let args = args
                .iter()
                .enumerate()
                .map(|(index, arg)| {
                    // A piped subject is passed as code where the function takes code;
                    // every other argument is quoted by the caller with 'emit'
                    let quote = subjects[index]
                        && staged.get(index).copied().unwrap_or(false)
                        && !context.is_comptime()
                        && !matches!(arg.kind, HIRExprKind::Emit { .. });
                    if quote {
                        lower_quote(context, &[], arg, &arg.range)
                    } else {
                        lower_expr(context, arg)
                    }
                })
                .collect::<CXResult<_>>()?;
            Ok(lower_comptime_call(context, callee, code, args, span))
        }
        Symbol::Global(GlobalSymbol::Constructor(data, variant, _)) => {
            let sum = lower_constructor_sum(context, &data.union_type, span)?;
            let value = lower_payload(context, &args, span)?;
            Ok(lower_construct(context, sum, variant, value, span))
        }
        _ => {
            let callee = lower_identifier(context, name, &callee.range)?;
            lower_call_args(context, callee, &args, span)
        }
    }
}

fn lower_args(
    context: &mut BodyLowering<'_>,
    args: &[&HIRExpression],
) -> CXResult<Vec<HMIRExprID>> {
    args.iter().map(|arg| lower_expr(context, arg)).collect()
}

fn lower_call_args(
    context: &mut BodyLowering<'_>,
    callee: HMIRExprID,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let args = lower_args(context, args)?;
    Ok(context.push(HMIRExprKind::Call { callee, args }, span))
}

// Outside comptime code the call runs at compile time, and code it returns is spliced in place
fn lower_comptime_call(
    context: &mut BodyLowering<'_>,
    callee: HMIRExprID,
    code: bool,
    args: Vec<HMIRExprID>,
    span: &TokenRange,
) -> HMIRExprID {
    let call = context.push(HMIRExprKind::Call { callee, args }, span);
    if context.is_comptime() {
        return call;
    }

    let staged = context.push(HMIRExprKind::Comptime(call), span);
    if !code {
        return staged;
    }
    context.push(
        HMIRExprKind::Splice {
            quote: staged,
            args: Vec::new(),
        },
        span,
    )
}

// The type a 'base::variant(...)' constructor builds. A base left to the compiler, '_' or a
// generator applied to holes, is completed from the type the value is expected to have.
pub(crate) fn lower_scope_base(
    context: &mut BodyLowering<'_>,
    base: &HIRExpression,
) -> CXResult<HMIRExprID> {
    match &base.kind {
        HIRExprKind::Hole => Ok(context.hole(&base.range)),
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
                lower_type(context, &generator)
            }
            _ => lower_type(context, ty),
        },
        _ => lower_expr(context, base),
    }
}

fn lower_payload(
    context: &mut BodyLowering<'_>,
    args: &[&HIRExpression],
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    match args {
        [] => Ok(context.push(HMIRExprKind::Constant(HMIRConstant::Unit), span)),
        [value] => lower_expr(context, value),
        _ => log_hir_error(
            span,
            &typecheck::ARGUMENT_COUNT,
            ("variant constructor".into(), 1, args.len(), false),
        ),
    }
}

pub(crate) fn lower_construct(
    context: &mut BodyLowering<'_>,
    sum: HMIRExprID,
    variant: CXIdent,
    value: HMIRExprID,
    span: &TokenRange,
) -> HMIRExprID {
    context.aggregate_op(
        HMIRAggregateOp::Initialize {
            ty: sum,
            fields: vec![(Some(variant), value)],
        },
        span,
    )
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
