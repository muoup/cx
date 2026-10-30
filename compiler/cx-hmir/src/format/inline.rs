use std::fmt;

use crate::expr::{
    HMIRExprID, HMIRExprKind,
    aggregate::{HMIRAggregateOp, HMIRPattern},
    op::HMIROp,
};

use super::{
    body::BodyPrinter,
    native::{Arg, render_args},
    ty::{write_def, write_generic_type, write_literal},
};

impl fmt::Display for HMIRExprID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "e{}", self.index())
    }
}

impl BodyPrinter<'_> {
    pub(super) fn inline(&self, id: HMIRExprID) -> String {
        match self.body().expr(id).kind() {
            HMIRExprKind::Literal(literal) => {
                fmt::from_fn(|f| write_literal(f, literal)).to_string()
            }
            HMIRExprKind::Local(local) => self.local(*local),
            HMIRExprKind::Def(def) => fmt::from_fn(|f| write_def(f, self.unit(), *def)).to_string(),
            HMIRExprKind::Name { name, tag: None } => name.to_string(),
            HMIRExprKind::Name {
                name,
                tag: Some(tag),
            } => format!("{} {name}", tag.prefix()),
            HMIRExprKind::Hole(hole) => hole.to_string(),
            HMIRExprKind::Type(ty) => fmt::from_fn(|f| {
                write_generic_type(
                    f,
                    ty,
                    &|f, inner| f.write_str(&self.inline(*inner)),
                    &|f, length| f.write_str(&self.inline(*length)),
                )
            })
            .to_string(),
            HMIRExprKind::ScopeAccess { base, member } => {
                format!("{}::{member}", self.inline(*base))
            }
            HMIRExprKind::Error(_) => "<error>".to_string(),
            HMIRExprKind::Comptime(inner) => format!("comptime({})", self.inline(*inner)),
            HMIRExprKind::Piped(value) => format!("piped({})", self.inline(*value)),
            HMIRExprKind::Splice { quote, args } => {
                let args = args.iter().map(|arg| self.inline(*arg)).collect::<Vec<_>>();
                format!("splice({})({})", self.inline(*quote), args.join(", "))
            }
            HMIRExprKind::Call {
                callee,
                args,
                named,
            } => {
                let args = args.iter().map(|arg| Arg::Value(*arg));
                let named = named
                    .iter()
                    .map(|(name, value)| Arg::Named(name.clone(), *value));
                let args = args.chain(named).collect::<Vec<_>>();
                let rendered = render_args(&args, &mut |id, _| self.inline(id));
                format!("{}({})", self.inline(*callee), rendered.join(", "))
            }
            HMIRExprKind::Intrinsic(intrinsic) => {
                let args = self.intrinsic_args(intrinsic);
                let rendered = render_args(&args, &mut |id, _| self.inline(id));
                format!("{}({})", intrinsic.path(), rendered.join(", "))
            }
            HMIRExprKind::Op(op) if self.is_place(op) => {
                self.place_with(id, &mut |id| self.inline(id))
            }
            HMIRExprKind::Op(op) => {
                let (head, args) = self.native_parts(op);
                let rendered = render_args(&args, &mut |id, _| self.inline(id));
                format!("{head}({})", rendered.join(", "))
            }
            _ => id.to_string(),
        }
    }

    pub(super) fn is_place(&self, op: &HMIROp) -> bool {
        matches!(
            op,
            HMIROp::AddressOf(_)
                | HMIROp::Dereference(_)
                | HMIROp::AggregateOp(
                    HMIRAggregateOp::Member { .. } | HMIRAggregateOp::Index { .. }
                )
        )
    }

    pub(super) fn place_with(
        &self,
        id: HMIRExprID,
        value: &mut dyn FnMut(HMIRExprID) -> String,
    ) -> String {
        let HMIRExprKind::Op(op) = self.body().expr(id).kind() else {
            return value(id);
        };
        match op {
            HMIROp::AggregateOp(HMIRAggregateOp::Member { base, name }) => {
                format!("{}.{name}", self.place_base(*base, value))
            }
            HMIROp::AggregateOp(HMIRAggregateOp::Index { base, index }) => {
                let base = self.place_base(*base, value);
                format!("{base}[{}]", value(*index))
            }
            HMIROp::Dereference(pointer) => format!("*{}", value(*pointer)),
            HMIROp::AddressOf(place) => format!("&{}", self.place_with(*place, value)),
            _ => value(id),
        }
    }

    fn place_base(&self, base: HMIRExprID, value: &mut dyn FnMut(HMIRExprID) -> String) -> String {
        let place = self.place_with(base, value);
        match self.body().expr(base).kind() {
            HMIRExprKind::Op(HMIROp::Dereference(_) | HMIROp::AddressOf(_)) => {
                format!("({place})")
            }
            _ => place,
        }
    }

    pub(super) fn pattern(&self, pattern: &HMIRPattern) -> String {
        match pattern {
            HMIRPattern::Wildcard => "_".to_string(),
            HMIRPattern::Binding(local) => self.local_decl(*local),
            HMIRPattern::Integer(value) => value.to_string(),
            HMIRPattern::Float(value) => value.to_string(),
            HMIRPattern::Value(value) => self.inline(*value),
            HMIRPattern::Variant { sum, name, inner } => {
                let sum = sum
                    .map(|sum| format!("{}::", self.inline(sum)))
                    .unwrap_or_default();
                let inner = inner
                    .as_ref()
                    .map(|inner| self.pattern(inner))
                    .unwrap_or_default();
                format!("{sum}{name}({inner})")
            }
        }
    }
}
