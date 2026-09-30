use std::fmt::{self, Formatter};

use crate::{
    expr::{meta::HMIRMetaKind, type_op::HMIRTypeOp},
    ids::HMIRMetaID,
};

use super::{
    body::BodyPrinter,
    ty::{write_constant, write_def_ref},
};

impl BodyPrinter<'_> {
    pub(super) fn meta(&self, f: &mut Formatter<'_>, id: HMIRMetaID, depth: usize) -> fmt::Result {
        match self.body().meta(id).kind() {
            HMIRMetaKind::Constant(constant) => write_constant(f, self.unit(), constant),
            HMIRMetaKind::Local(local) => self.meta_local(f, *local),
            HMIRMetaKind::Def(def) => write_def_ref(f, self.unit(), def),
            HMIRMetaKind::Hole(hole) => write!(f, "{hole}"),

            HMIRMetaKind::Intrinsic(intrinsic) => {
                self.intrinsic(f, intrinsic, depth, |printer, f, value, depth| {
                    printer.meta(f, *value, depth)
                })
            }
            HMIRMetaKind::TypeOp(op) => self.type_op(f, op, depth),
            HMIRMetaKind::Binary { op, lhs, rhs } => {
                write!(f, "{}(", op.path())?;
                self.list(f, &[*lhs, *rhs], depth, Self::meta)?;
                f.write_str(")")
            }
            HMIRMetaKind::Unary { op, operand } => {
                write!(f, "{}(", op.path())?;
                self.meta(f, *operand, depth)?;
                f.write_str(")")
            }
            HMIRMetaKind::Coerce {
                mode,
                from,
                to,
                value,
            } => {
                write!(f, "{}<", mode.path())?;
                self.list(f, &[*from, *to], depth, Self::meta)?;
                f.write_str(">(")?;
                self.meta(f, *value, depth)?;
                f.write_str(")")
            }

            HMIRMetaKind::Call { callee, args } => {
                self.meta(f, *callee, depth)?;
                f.write_str("(")?;
                self.list(f, args, depth, Self::meta)?;
                f.write_str(")")
            }

            HMIRMetaKind::Let { local, initializer } => {
                f.write_str("local ")?;
                self.meta_local_decl(f, *local)?;
                if let Some(initializer) = initializer {
                    f.write_str(" = ")?;
                    self.meta(f, *initializer, depth)?;
                }
                Ok(())
            }
            HMIRMetaKind::Assign { local, value } => {
                self.meta_local(f, *local)?;
                f.write_str(" = ")?;
                self.meta(f, *value, depth)
            }

            HMIRMetaKind::Block { statements, tail } => self.block(
                f,
                statements,
                *tail,
                depth,
                Self::meta,
                Self::is_structured_meta,
            ),
            HMIRMetaKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                f.write_str("if (")?;
                self.meta(f, *condition, depth)?;
                f.write_str(") ")?;
                self.meta(f, *then_branch, depth)?;
                if let Some(else_branch) = else_branch {
                    f.write_str(" else ")?;
                    self.meta(f, *else_branch, depth)?;
                }
                Ok(())
            }
            HMIRMetaKind::While { condition, body } => {
                f.write_str("while (")?;
                self.meta(f, *condition, depth)?;
                f.write_str(") ")?;
                self.meta(f, *body, depth)
            }
            HMIRMetaKind::Break => f.write_str("break"),
            HMIRMetaKind::Continue => f.write_str("continue"),
            HMIRMetaKind::Return(value) => {
                f.write_str("return")?;
                if let Some(value) = value {
                    f.write_str(" ")?;
                    self.meta(f, *value, depth)?;
                }
                Ok(())
            }

            HMIRMetaKind::Quote { params, body } => {
                f.write_str("quote")?;
                if !params.is_empty() {
                    f.write_str("(")?;
                    self.list(f, params, depth, |printer, f, param, _| {
                        printer.obj_local_decl(f, param)
                    })?;
                    f.write_str(")")?;
                }
                f.write_str(" ")?;
                self.obj(f, *body, depth)
            }
            HMIRMetaKind::CompileError(message) => {
                f.write_str("@compile_error(")?;
                self.meta(f, *message, depth)?;
                f.write_str(")")
            }

            HMIRMetaKind::Error => f.write_str("<error>"),
        }
    }

    fn is_structured_meta(&self, id: HMIRMetaID) -> bool {
        matches!(
            self.body().meta(id).kind(),
            HMIRMetaKind::Block { .. } | HMIRMetaKind::If { .. } | HMIRMetaKind::While { .. }
        )
    }

    fn type_op(&self, f: &mut Formatter<'_>, op: &HMIRTypeOp, depth: usize) -> fmt::Result {
        match op {
            HMIRTypeOp::Pointer(inner) => {
                self.meta(f, *inner, depth)?;
                f.write_str("*")
            }
            HMIRTypeOp::Reference(inner) => {
                self.meta(f, *inner, depth)?;
                f.write_str("&")
            }
            HMIRTypeOp::Array { element, length } => {
                self.meta(f, *element, depth)?;
                f.write_str("[")?;
                if let Some(length) = length {
                    self.meta(f, *length, depth)?;
                }
                f.write_str("]")
            }
            HMIRTypeOp::Function {
                params,
                ret,
                variadic,
            } => {
                f.write_str("fn(")?;
                self.list(f, params, depth, Self::meta)?;
                if *variadic {
                    f.write_str(if params.is_empty() { "..." } else { ", ..." })?;
                }
                f.write_str(") -> ")?;
                self.meta(f, *ret, depth)
            }
            HMIRTypeOp::Expr { params, result } => {
                f.write_str("expr(")?;
                self.list(f, params, depth, Self::meta)?;
                f.write_str(") -> ")?;
                self.meta(f, *result, depth)
            }
            HMIRTypeOp::SizeOf(ty)
            | HMIRTypeOp::AlignOf(ty)
            | HMIRTypeOp::IsInt(ty)
            | HMIRTypeOp::IsFloat(ty)
            | HMIRTypeOp::IsPointer(ty)
            | HMIRTypeOp::IsSigned(ty) => {
                write!(f, "@{}(", op.path())?;
                self.meta(f, *ty, depth)?;
                f.write_str(")")
            }
            HMIRTypeOp::Equal(lhs, rhs) => {
                write!(f, "@{}(", op.path())?;
                self.list(f, &[*lhs, *rhs], depth, Self::meta)?;
                f.write_str(")")
            }
        }
    }
}
