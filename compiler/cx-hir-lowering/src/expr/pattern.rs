use cx_hir::ast::pattern::HIRPattern;
use cx_hmir::{HMIRLocalID, HMIRPattern};
use cx_tokens::TokenRange;

use crate::{
    body::{BodyLowering, Symbol},
    resolve::GlobalSymbol,
};

impl BodyLowering<'_> {
    pub(super) fn lower_pattern(&mut self, pattern: &HIRPattern, span: &TokenRange) -> HMIRPattern {
        match pattern {
            HIRPattern::Binding(name) => {
                let ty = self.hole(span);
                HMIRPattern::Binding(self.declare_local(Some(name), ty, span))
            }
            HIRPattern::Integer(value) => HMIRPattern::Integer(*value),
            HIRPattern::Float(value) => HMIRPattern::Float(*value),
            HIRPattern::Variant {
                constructor,
                template_input,
                inner,
            } => {
                let (sum, index) = match self.lookup(constructor, None) {
                    Symbol::Global(GlobalSymbol::Constructor(data, _)) => (
                        self.constructor_sum(&data.union_type, template_input.as_ref(), span),
                        data.variant_index,
                    ),
                    _ => (self.error(span), 0),
                };
                let inner = inner
                    .as_deref()
                    .map(|inner| self.variant_payload(inner, span));
                HMIRPattern::Variant { sum, index, inner }
            }
        }
    }

    fn variant_payload(&mut self, pattern: &HIRPattern, span: &TokenRange) -> HMIRLocalID {
        let name = match pattern {
            HIRPattern::Binding(name) => Some(name),
            _ => None,
        };
        let ty = self.hole(span);
        self.declare_local(name, ty, span)
    }
}
