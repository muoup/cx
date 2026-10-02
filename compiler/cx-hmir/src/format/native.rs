use std::fmt::{self, Formatter};

use crate::{
    expr::{
        aggregate::HMIRAggregateOp,
        kind::HMIRExprID,
        native_op::{HMIRControlOp, HMIRNativeOp, HMIROwnershipOp},
        type_op::HMIRTypeOp,
    },
    ty::nominal::{HMIRAggregateKind, HMIRMoveSemantics},
};

use super::body::{BodyPrinter, indent};

impl BodyPrinter<'_> {
    pub(super) fn native(
        &self,
        f: &mut Formatter<'_>,
        op: &HMIRNativeOp,
        depth: usize,
    ) -> fmt::Result {
        match op {
            HMIRNativeOp::BinOp { op, lhs, rhs } => self.call(f, op.path(), &[*lhs, *rhs], depth),
            HMIRNativeOp::UnOp { op, operand } => self.call(f, op.path(), &[*operand], depth),
            HMIRNativeOp::Coerce {
                mode,
                value,
                target,
            } => {
                write!(f, "{}<", mode.path())?;
                self.expr(f, *target, depth)?;
                f.write_str(">(")?;
                self.expr(f, *value, depth)?;
                f.write_str(")")
            }
            HMIRNativeOp::Assign { target, op, value } => {
                f.write_str("op.assign")?;
                if let Some(op) = op {
                    write!(f, "<{}>", op.path())?;
                }
                f.write_str("(")?;
                self.list(f, &[*target, *value], depth, Self::expr)?;
                f.write_str(")")
            }
            HMIRNativeOp::AddressOf(operand) => self.call(f, "op.address_of", &[*operand], depth),
            HMIRNativeOp::Dereference(operand) => {
                self.call(f, "op.dereference", &[*operand], depth)
            }
            HMIRNativeOp::Type(op) => self.type_op(f, op, depth),
            HMIRNativeOp::Control(op) => self.control(f, op, depth),
            HMIRNativeOp::OwnershipOp(op) => self.ownership(f, op, depth),
            HMIRNativeOp::AggregateOp(op) => self.aggregate(f, op, depth),
        }
    }

    fn call(
        &self,
        f: &mut Formatter<'_>,
        path: &str,
        args: &[HMIRExprID],
        depth: usize,
    ) -> fmt::Result {
        write!(f, "{path}(")?;
        self.list(f, args, depth, Self::expr)?;
        f.write_str(")")
    }

    fn control(&self, f: &mut Formatter<'_>, op: &HMIRControlOp, depth: usize) -> fmt::Result {
        f.write_str(op.path())?;
        match op {
            HMIRControlOp::Return(Some(operand))
            | HMIRControlOp::Yield(Some(operand))
            | HMIRControlOp::Defer(operand)
            | HMIRControlOp::Unsafe(operand) => {
                f.write_str(" ")?;
                self.expr(f, *operand, depth)
            }
            HMIRControlOp::Goto(label) => write!(f, " {label}"),
            HMIRControlOp::Return(None)
            | HMIRControlOp::Yield(None)
            | HMIRControlOp::Break
            | HMIRControlOp::Continue
            | HMIRControlOp::Unreachable => Ok(()),
        }
    }

    fn ownership(&self, f: &mut Formatter<'_>, op: &HMIROwnershipOp, depth: usize) -> fmt::Result {
        match op {
            HMIROwnershipOp::Allocate(operand)
            | HMIROwnershipOp::Adopt(operand)
            | HMIROwnershipOp::Leak(operand)
            | HMIROwnershipOp::Move(operand) => self.call(f, op.path(), &[*operand], depth),
        }
    }

    fn aggregate(&self, f: &mut Formatter<'_>, op: &HMIRAggregateOp, depth: usize) -> fmt::Result {
        match op {
            HMIRAggregateOp::Member { base, name } => {
                write!(f, "{}(", op.path())?;
                self.expr(f, *base, depth)?;
                write!(f, ", {name})")
            }
            HMIRAggregateOp::Index { base, index } => {
                self.call(f, op.path(), &[*base, *index], depth)
            }
            HMIRAggregateOp::Initialize { ty, fields } => {
                write!(f, "{}(", op.path())?;
                self.expr(f, *ty, depth)?;
                for (name, value) in fields {
                    f.write_str(", ")?;
                    if let Some(name) = name {
                        write!(f, ".{name} = ")?;
                    }
                    self.expr(f, *value, depth)?;
                }
                f.write_str(")")
            }
            HMIRAggregateOp::Is { value, pattern } => {
                write!(f, "{}(", op.path())?;
                self.expr(f, *value, depth)?;
                f.write_str(", ")?;
                self.pattern(f, pattern, depth)?;
                f.write_str(")")
            }
            HMIRAggregateOp::Unpack { value, bindings } => {
                write!(f, "{}(", op.path())?;
                self.expr(f, *value, depth)?;
                for (field, local) in bindings {
                    write!(f, ", .{field} = ")?;
                    self.local_decl(f, *local)?;
                }
                f.write_str(")")
            }
        }
    }

    fn type_op(&self, f: &mut Formatter<'_>, op: &HMIRTypeOp, depth: usize) -> fmt::Result {
        match op {
            HMIRTypeOp::Pointer(inner) => {
                self.expr(f, *inner, depth)?;
                f.write_str("*")
            }
            HMIRTypeOp::Reference(inner) => {
                self.expr(f, *inner, depth)?;
                f.write_str("&")
            }
            HMIRTypeOp::Array { element, length } => {
                self.expr(f, *element, depth)?;
                f.write_str("[")?;
                if let Some(length) = length {
                    self.expr(f, *length, depth)?;
                }
                f.write_str("]")
            }
            HMIRTypeOp::Function {
                params,
                ret,
                variadic,
            } => {
                f.write_str("fn(")?;
                self.list(f, params, depth, Self::expr)?;
                if *variadic {
                    f.write_str(if params.is_empty() { "..." } else { ", ..." })?;
                }
                f.write_str(") -> ")?;
                self.expr(f, *ret, depth)
            }
            HMIRTypeOp::Expr { params, result } => {
                f.write_str("expr(")?;
                self.list(f, params, depth, Self::expr)?;
                f.write_str(") -> ")?;
                self.expr(f, *result, depth)
            }
            HMIRTypeOp::Aggregate {
                kind,
                semantics,
                fields,
            } => {
                f.write_str(match kind {
                    HMIRAggregateKind::Struct => "struct",
                    HMIRAggregateKind::Union => "union",
                    HMIRAggregateKind::TaggedUnion => "tagged_union",
                })?;
                if let Some(semantics) = semantics_keyword(*semantics) {
                    write!(f, " {semantics}")?;
                }
                f.write_str(" {\n")?;
                for field in fields {
                    indent(f, depth + 1)?;
                    match field.name() {
                        Some(name) => write!(f, "{name}: ")?,
                        None => f.write_str("_: ")?,
                    }
                    self.expr(f, field.ty(), depth + 1)?;
                    if let Some(width) = field.bit_width() {
                        write!(f, " : {width}")?;
                    }
                    f.write_str(",\n")?;
                }
                indent(f, depth)?;
                f.write_str("}")
            }
            HMIRTypeOp::PointerInner(ty)
            | HMIRTypeOp::ReferenceInner(ty)
            | HMIRTypeOp::TypeOf(ty)
            | HMIRTypeOp::Decay(ty)
            | HMIRTypeOp::SizeOf(ty)
            | HMIRTypeOp::AlignOf(ty)
            | HMIRTypeOp::IsInt(ty)
            | HMIRTypeOp::IsFloat(ty)
            | HMIRTypeOp::IsPointer(ty)
            | HMIRTypeOp::IsSigned(ty) => self.call(f, op.path(), &[*ty], depth),
            HMIRTypeOp::Equal(lhs, rhs) => self.call(f, op.path(), &[*lhs, *rhs], depth),
        }
    }
}

fn semantics_keyword(semantics: HMIRMoveSemantics) -> Option<&'static str> {
    match semantics {
        HMIRMoveSemantics::POD => None,
        HMIRMoveSemantics::Nocopy => Some("nocopy"),
        HMIRMoveSemantics::Nodrop => Some("nodrop"),
    }
}
