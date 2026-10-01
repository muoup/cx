use cx_hir::ast::pattern::HIRPattern;
use cx_hmir::{HMIRLocalID, HMIRPattern};
use cx_tokens::TokenRange;

use crate::{
    body::{BodyLowering, Symbol},
    resolve::GlobalSymbol,
    ty::lower_constructor_sum,
};

pub(crate) fn lower_pattern(
    cx: &mut BodyLowering<'_>,
    pattern: &HIRPattern,
    span: &TokenRange,
) -> HMIRPattern {
    match pattern {
        HIRPattern::Binding(name) => {
            let ty = cx.hole(span);
            HMIRPattern::Binding(cx.declare_local(Some(name), ty, span))
        }
        HIRPattern::Integer(value) => HMIRPattern::Integer(*value),
        HIRPattern::Float(value) => HMIRPattern::Float(*value),
        HIRPattern::Variant {
            constructor,
            template_input,
            inner,
        } => {
            let (sum, index) = match cx.lookup(constructor, None) {
                Symbol::Global(GlobalSymbol::Constructor(data, _)) => (
                    lower_constructor_sum(cx, &data.union_type, template_input.as_ref(), span),
                    data.variant_index,
                ),
                _ => (cx.error(span), 0),
            };
            let inner = inner
                .as_deref()
                .map(|inner| lower_variant_payload(cx, inner, span));
            HMIRPattern::Variant { sum, index, inner }
        }
    }
}

fn lower_variant_payload(
    cx: &mut BodyLowering<'_>,
    pattern: &HIRPattern,
    span: &TokenRange,
) -> HMIRLocalID {
    let name = match pattern {
        HIRPattern::Binding(name) => Some(name),
        _ => None,
    };
    let ty = cx.hole(span);
    cx.declare_local(name, ty, span)
}
