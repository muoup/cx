use cx_hir::ast::{
    expression::HIRExprKind,
    pattern::{HIRBindingMode, HIRPattern},
};
use cx_hmir::{
    HMIRPattern,
    body::HMIRLocalID,
    expr::{HMIRExprID, op::HMIRTypeOp},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{body::BodyLowering, expr::lower_expr, ty::lower_type};

pub(crate) fn lower_pattern(
    context: &mut BodyLowering<'_>,
    pattern: &HIRPattern,
    span: &TokenRange,
) -> CXResult<HMIRPattern> {
    Ok(match pattern {
        HIRPattern::Wildcard => {
            let ty = lower_binding_type(context, HIRBindingMode::Reference, span);
            HMIRPattern::Binding(context.declare_local(None, ty, span))
        }
        HIRPattern::Binding { name, mode } => {
            let ty = lower_binding_type(context, *mode, span);
            HMIRPattern::Binding(context.declare_local(Some(name), ty, span))
        }
        HIRPattern::Integer(value) => HMIRPattern::Integer(*value),
        HIRPattern::Float(value) => HMIRPattern::Float(*value),
        HIRPattern::Value(value) => HMIRPattern::Value(lower_expr(context, value)?),
        HIRPattern::Variant {
            qualifier,
            name,
            inner,
        } => {
            let sum = match qualifier.as_deref().map(|qualifier| &qualifier.kind) {
                Some(HIRExprKind::Type(ty)) => Some(lower_type(context, ty)?),
                _ => None,
            };
            let inner = inner
                .as_deref()
                .map(|inner| lower_variant_payload(context, inner, span));
            HMIRPattern::Variant {
                sum,
                name: name.clone(),
                inner,
            }
        }
    })
}

fn lower_variant_payload(
    context: &mut BodyLowering<'_>,
    pattern: &HIRPattern,
    span: &TokenRange,
) -> HMIRLocalID {
    let (name, mode) = match pattern {
        HIRPattern::Binding { name, mode } => (Some(name), *mode),
        _ => (None, HIRBindingMode::Reference),
    };
    let ty = lower_binding_type(context, mode, span);
    context.declare_local(name, ty, span)
}

// 'auto x' takes the matched value, 'auto& x' refers to it where it is; a binding that is
// never named takes nothing
fn lower_binding_type(
    context: &mut BodyLowering<'_>,
    mode: HIRBindingMode,
    span: &TokenRange,
) -> HMIRExprID {
    let hole = context.hole(span);
    match mode {
        HIRBindingMode::Owned => hole,
        HIRBindingMode::Reference => context.type_op(HMIRTypeOp::Reference(hole), span),
        HIRBindingMode::ConstReference => {
            let inner = context.type_op(HMIRTypeOp::Const(hole), span);
            context.type_op(HMIRTypeOp::Reference(inner), span)
        }
    }
}
