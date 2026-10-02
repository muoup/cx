use cx_hir::ast::{
    expression::HIRExprKind,
    pattern::{HIRBindingMode, HIRPattern},
};
use cx_hmir::{HMIRExprID, HMIRLocalID, HMIRPattern, HMIRTypeOp};
use cx_tokens::TokenRange;

use crate::{body::BodyLowering, expr::lower_expr, ty::lower_type};

pub(crate) fn lower_pattern(
    cx: &mut BodyLowering<'_>,
    pattern: &HIRPattern,
    span: &TokenRange,
) -> HMIRPattern {
    match pattern {
        HIRPattern::Wildcard => {
            let ty = lower_binding_type(cx, HIRBindingMode::Reference, span);
            HMIRPattern::Binding(cx.declare_local(None, ty, span))
        }
        HIRPattern::Binding { name, mode } => {
            let ty = lower_binding_type(cx, *mode, span);
            HMIRPattern::Binding(cx.declare_local(Some(name), ty, span))
        }
        HIRPattern::Integer(value) => HMIRPattern::Integer(*value),
        HIRPattern::Float(value) => HMIRPattern::Float(*value),
        HIRPattern::Value(value) => HMIRPattern::Value(lower_expr(cx, value)),
        HIRPattern::Variant {
            qualifier,
            name,
            inner,
        } => {
            let sum = qualifier
                .as_deref()
                .and_then(|qualifier| match &qualifier.kind {
                    HIRExprKind::Type(ty) => Some(lower_type(cx, ty)),
                    _ => None,
                });
            let inner = inner
                .as_deref()
                .map(|inner| lower_variant_payload(cx, inner, span));
            HMIRPattern::Variant {
                sum,
                name: name.clone(),
                inner,
            }
        }
    }
}

fn lower_variant_payload(
    cx: &mut BodyLowering<'_>,
    pattern: &HIRPattern,
    span: &TokenRange,
) -> HMIRLocalID {
    let (name, mode) = match pattern {
        HIRPattern::Binding { name, mode } => (Some(name), *mode),
        _ => (None, HIRBindingMode::Reference),
    };
    let ty = lower_binding_type(cx, mode, span);
    cx.declare_local(name, ty, span)
}

// 'auto x' takes the matched value, 'auto& x' refers to it where it is; a binding that is
// never named takes nothing
fn lower_binding_type(
    cx: &mut BodyLowering<'_>,
    mode: HIRBindingMode,
    span: &TokenRange,
) -> HMIRExprID {
    let hole = cx.hole(span);
    match mode {
        HIRBindingMode::Owned => hole,
        HIRBindingMode::Reference | HIRBindingMode::ConstReference => {
            cx.type_op(HMIRTypeOp::Reference(hole), span)
        }
    }
}
