use std::fmt::{self, Formatter};

use cx_intrinsics::IntrinsicArg;

use crate::expr::{
    aggregate::HMIRPattern,
    kind::{HMIRExprID, HMIRExprKind, HMIRIntrinsic},
    native_op::{HMIRControlOp, HMIRNativeOp},
};

use super::{
    body::{BodyPrinter, indent},
    ty::{write_constant, write_def_ref},
};

impl BodyPrinter<'_> {
    pub(super) fn expr(&self, f: &mut Formatter<'_>, id: HMIRExprID, depth: usize) -> fmt::Result {
        match self.body().expr(id).kind() {
            HMIRExprKind::Constant(constant) => write_constant(f, self.unit(), constant),
            HMIRExprKind::Local(local) => self.local(f, *local),
            HMIRExprKind::Def(def) => write_def_ref(f, self.unit(), def),
            HMIRExprKind::Hole(hole) => write!(f, "{hole}"),
            HMIRExprKind::Error(_) => f.write_str("<error>"),

            HMIRExprKind::Comptime(inner) => {
                f.write_str("comptime(")?;
                self.expr(f, *inner, depth)?;
                f.write_str(")")
            }
            HMIRExprKind::Quote { params, body } => {
                f.write_str("quote")?;
                if !params.is_empty() {
                    f.write_str("(")?;
                    self.list(f, params, depth, |printer, f, param, _| {
                        printer.local_decl(f, param)
                    })?;
                    f.write_str(")")?;
                }
                f.write_str(" ")?;
                self.expr(f, *body, depth)
            }
            HMIRExprKind::Splice { quote, args } => {
                f.write_str("splice(")?;
                self.expr(f, *quote, depth)?;
                f.write_str(")")?;
                if !args.is_empty() {
                    f.write_str("(")?;
                    self.list(f, args, depth, Self::expr)?;
                    f.write_str(")")?;
                }
                Ok(())
            }

            HMIRExprKind::Intrinsic(intrinsic) => self.intrinsic(f, intrinsic, depth),
            HMIRExprKind::Native(op) => self.native(f, op, depth),

            HMIRExprKind::Let { local, initializer } => {
                f.write_str("local ")?;
                self.local_decl(f, *local)?;
                if let Some(initializer) = initializer {
                    f.write_str(" = ")?;
                    self.expr(f, *initializer, depth)?;
                }
                Ok(())
            }
            HMIRExprKind::Call { callee, args } => {
                self.expr(f, *callee, depth)?;
                f.write_str("(")?;
                self.list(f, args, depth, Self::expr)?;
                f.write_str(")")
            }

            HMIRExprKind::Block {
                kind,
                statements,
                tail,
            } => self.block(f, *kind, statements, *tail, depth),
            HMIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.condition(f, "if", *condition, depth)?;
                self.expr(f, *then_branch, depth)?;
                if let Some(else_branch) = else_branch {
                    f.write_str(" else ")?;
                    self.expr(f, *else_branch, depth)?;
                }
                Ok(())
            }
            HMIRExprKind::While {
                condition,
                body,
                pre_eval: true,
            } => {
                self.condition(f, "while", *condition, depth)?;
                self.expr(f, *body, depth)
            }
            HMIRExprKind::While {
                condition,
                body,
                pre_eval: false,
            } => {
                f.write_str("do ")?;
                self.expr(f, *body, depth)?;
                f.write_str(" ")?;
                self.condition(f, "while", *condition, depth)
            }
            HMIRExprKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                f.write_str("for (")?;
                self.statement(f, *init, depth)?;
                f.write_str("; ")?;
                self.statement(f, *condition, depth)?;
                f.write_str("; ")?;
                self.statement(f, *increment, depth)?;
                f.write_str(") ")?;
                self.expr(f, *body, depth)
            }
            HMIRExprKind::Switch { condition, body } => {
                self.condition(f, "switch", *condition, depth)?;
                self.expr(f, *body, depth)
            }
            HMIRExprKind::Case { value, body } => {
                match value {
                    Some(value) => {
                        f.write_str("case ")?;
                        self.expr(f, *value, depth)?;
                        f.write_str(": ")?;
                    }
                    None => f.write_str("default: ")?,
                }
                self.expr(f, *body, depth)
            }
            HMIRExprKind::Match {
                scrutinee,
                subject,
                arms,
            } => {
                self.condition(f, "match", *scrutinee, depth)?;
                f.write_str("as ")?;
                self.local(f, *subject)?;
                f.write_str(" {\n")?;
                for (pattern, body) in arms {
                    indent(f, depth + 1)?;
                    self.pattern(f, pattern, depth + 1)?;
                    f.write_str(" => ")?;
                    self.expr(f, *body, depth + 1)?;
                    f.write_str("\n")?;
                }
                indent(f, depth)?;
                f.write_str("}")
            }
            HMIRExprKind::Label { name, body } => {
                write!(f, "{name}: ")?;
                self.expr(f, *body, depth)
            }
        }
    }

    pub(super) fn is_structured(&self, id: HMIRExprID) -> bool {
        match self.body().expr(id).kind() {
            HMIRExprKind::Block { .. } | HMIRExprKind::Match { .. } => true,
            HMIRExprKind::If {
                then_branch,
                else_branch,
                ..
            } => self.is_structured(else_branch.unwrap_or(*then_branch)),
            HMIRExprKind::While {
                body,
                pre_eval: true,
                ..
            }
            | HMIRExprKind::For { body, .. }
            | HMIRExprKind::Switch { body, .. }
            | HMIRExprKind::Case { body, .. }
            | HMIRExprKind::Label { body, .. }
            | HMIRExprKind::Comptime(body)
            | HMIRExprKind::Native(HMIRNativeOp::Control(
                HMIRControlOp::Defer(body) | HMIRControlOp::Unsafe(body),
            )) => self.is_structured(*body),
            _ => false,
        }
    }

    pub(super) fn pattern(
        &self,
        f: &mut Formatter<'_>,
        pattern: &HMIRPattern,
        depth: usize,
    ) -> fmt::Result {
        match pattern {
            HMIRPattern::Binding(local) => self.local_decl(f, *local),
            HMIRPattern::Integer(value) => write!(f, "{value}"),
            HMIRPattern::Float(value) => write!(f, "{value}"),
            HMIRPattern::Value(value) => self.expr(f, *value, depth),
            HMIRPattern::Variant { sum, name, inner } => {
                if let Some(sum) = sum {
                    self.expr(f, *sum, depth)?;
                    f.write_str("::")?;
                }
                write!(f, "{name}")?;
                if let Some(inner) = inner {
                    f.write_str("(")?;
                    self.local_decl(f, *inner)?;
                    f.write_str(")")?;
                }
                Ok(())
            }
        }
    }

    fn intrinsic(
        &self,
        f: &mut Formatter<'_>,
        intrinsic: &HMIRIntrinsic,
        depth: usize,
    ) -> fmt::Result {
        write!(f, "{}(", intrinsic.path())?;
        for (index, arg) in intrinsic.args().into_iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            match arg {
                IntrinsicArg::Value(value) | IntrinsicArg::Type(value) => {
                    self.expr(f, *value, depth)?
                }
                IntrinsicArg::Index(value) => write!(f, "{value}")?,
                IntrinsicArg::Bool(value) => write!(f, "{value}")?,
                IntrinsicArg::String(value) => write!(f, "{value:?}")?,
            }
        }
        f.write_str(")")
    }
}
