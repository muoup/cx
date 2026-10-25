use cx_hir::ast::{
    expression::HIRExprKind,
    pattern::{HIRBindingMode, HIRPattern},
};
use cx_hmir::{
    HMIRPattern,
    expr::HMIRExprID,
    ty::{HMIRExprTypeKind, aggregate::HMIRTypeSemantics},
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
        HIRPattern::Wildcard => HMIRPattern::Wildcard,
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
            let inner = match inner.as_deref() {
                Some(inner) => Some(Box::new(lower_pattern(context, inner, span)?)),
                None => None,
            };
            HMIRPattern::Variant {
                sum,
                name: name.clone(),
                inner,
            }
        }
    })
}

// 'auto x' takes the matched value, 'auto& x' refers to it where it is
fn lower_binding_type(
    context: &mut BodyLowering<'_>,
    mode: HIRBindingMode,
    span: &TokenRange,
) -> HMIRExprID {
    let hole = context.hole(span);
    let semantics = HMIRTypeSemantics::default();
    match mode {
        HIRBindingMode::Owned => hole,
        HIRBindingMode::Reference => {
            let kind = HMIRExprTypeKind::ReferenceTo {
                inner: hole,
                bitfield: None,
            };
            context.expr_type(kind, semantics, None, span)
        }
        HIRBindingMode::ConstReference => {
            let constant = HMIRTypeSemantics {
                constant: true,
                ..semantics
            };
            let inner = context.expr_type(HMIRExprTypeKind::Nominal(hole), constant, None, span);
            let kind = HMIRExprTypeKind::ReferenceTo {
                inner,
                bitfield: None,
            };
            context.expr_type(kind, semantics, None, span)
        }
    }
}
