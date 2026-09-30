use std::fmt::{self, Formatter};

use crate::{
    expr::{aggregate::HMIRInitializer, obj::HMIRObjKind, pattern::HMIRPattern},
    ids::{HMIRMetaID, HMIRObjID},
};

use super::{
    body::{BodyPrinter, indent},
    ty::{write_constant, write_def_ref},
};

impl BodyPrinter<'_> {
    pub(super) fn obj(&self, f: &mut Formatter<'_>, id: HMIRObjID, depth: usize) -> fmt::Result {
        match self.body().obj(id).kind() {
            HMIRObjKind::Constant(constant) => write_constant(f, self.unit(), constant),
            HMIRObjKind::Local(local) => self.obj_local(f, *local),
            HMIRObjKind::Global(def) => write_def_ref(f, self.unit(), def),
            HMIRObjKind::FunctionAddress { function, statics } => {
                f.write_str("&")?;
                self.meta(f, *function, depth)?;
                self.statics(f, statics, depth)
            }
            HMIRObjKind::Lift(value) => {
                f.write_str("lift(")?;
                self.meta(f, *value, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::Splice { quote, args } => {
                f.write_str("splice(")?;
                self.meta(f, *quote, depth)?;
                f.write_str(")")?;
                if !args.is_empty() {
                    f.write_str("(")?;
                    self.list(f, args, depth, Self::obj)?;
                    f.write_str(")")?;
                }
                Ok(())
            }

            HMIRObjKind::Intrinsic(intrinsic) => {
                self.intrinsic(f, intrinsic, depth, |printer, f, value, depth| {
                    printer.obj(f, *value, depth)
                })
            }
            HMIRObjKind::Binary { op, lhs, rhs } => {
                write!(f, "{}(", op.path())?;
                self.list(f, &[*lhs, *rhs], depth, Self::obj)?;
                f.write_str(")")
            }
            HMIRObjKind::Unary { op, operand } => {
                write!(f, "{}(", op.path())?;
                self.obj(f, *operand, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::Increment {
                target,
                amount,
                postfix,
            } => {
                f.write_str(if *postfix { "op.post_inc(" } else { "op.pre_inc(" })?;
                self.obj(f, *target, depth)?;
                write!(f, ", {amount})")
            }
            HMIRObjKind::Coerce {
                mode,
                from,
                to,
                value,
            } => {
                write!(f, "{}<", mode.path())?;
                self.list(f, &[*from, *to], depth, Self::meta)?;
                f.write_str(">(")?;
                self.obj(f, *value, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::Retype { value, to } => {
                f.write_str("retype<")?;
                self.meta(f, *to, depth)?;
                f.write_str(">(")?;
                self.obj(f, *value, depth)?;
                f.write_str(")")
            }

            HMIRObjKind::Let { local, initializer } => {
                f.write_str("local ")?;
                self.obj_local_decl(f, *local)?;
                if let Some(initializer) = initializer {
                    f.write_str(" = ")?;
                    self.obj(f, *initializer, depth)?;
                }
                Ok(())
            }
            HMIRObjKind::Adopt { local, value } => {
                f.write_str("adopt ")?;
                self.obj_local_decl(f, *local)?;
                f.write_str(" = ")?;
                self.obj(f, *value, depth)
            }
            HMIRObjKind::StaticLet { local, initializer } => {
                f.write_str("static ")?;
                self.meta_local_decl(f, *local)?;
                f.write_str(" = ")?;
                self.meta(f, *initializer, depth)
            }
            HMIRObjKind::Load(place) => {
                f.write_str("load(")?;
                self.obj(f, *place, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::Move(local) => {
                f.write_str("move ")?;
                self.obj_local(f, *local)
            }
            HMIRObjKind::Assign { target, value } => {
                self.obj(f, *target, depth)?;
                f.write_str(" = ")?;
                self.obj(f, *value, depth)
            }
            HMIRObjKind::AddressOf(place) => {
                f.write_str("&")?;
                self.obj(f, *place, depth)
            }
            HMIRObjKind::Dereference(pointer) => {
                f.write_str("*")?;
                self.obj(f, *pointer, depth)
            }

            HMIRObjKind::Member { base, name } => {
                f.write_str("op.member(")?;
                self.obj(f, *base, depth)?;
                write!(f, ", {name})")
            }
            HMIRObjKind::Field {
                base,
                index,
                aggregate,
            } => {
                self.obj(f, *base, depth)?;
                write!(f, ".{index}<")?;
                self.meta(f, *aggregate, depth)?;
                f.write_str(">")
            }
            HMIRObjKind::Index {
                base,
                index,
                element,
            } => {
                self.obj(f, *base, depth)?;
                f.write_str("[")?;
                self.obj(f, *index, depth)?;
                f.write_str("]<")?;
                self.meta(f, *element, depth)?;
                f.write_str(">")
            }
            HMIRObjKind::Initialize { ty, initializer } => {
                f.write_str("init ")?;
                self.meta(f, *ty, depth)?;
                f.write_str(" ")?;
                self.initializer(f, initializer, depth)
            }
            HMIRObjKind::Tag { value, sum } => {
                f.write_str("tag<")?;
                self.meta(f, *sum, depth)?;
                f.write_str(">(")?;
                self.obj(f, *value, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::SetVariant {
                target,
                index,
                value,
                sum,
            } => {
                f.write_str("set_variant<")?;
                self.meta(f, *sum, depth)?;
                f.write_str(">(")?;
                self.obj(f, *target, depth)?;
                write!(f, ", {index}, ")?;
                self.obj(f, *value, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::Is { value, pattern } => {
                self.obj(f, *value, depth)?;
                f.write_str(" is ")?;
                self.pattern(f, pattern, depth)
            }
            HMIRObjKind::Unpack { value, bindings } => {
                f.write_str("unpack ")?;
                self.obj(f, *value, depth)?;
                f.write_str(" { ")?;
                for (index, (field, local)) in bindings.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, ".{field} => ")?;
                    self.obj_local_decl(f, *local)?;
                }
                f.write_str(" }")
            }

            HMIRObjKind::Call {
                callee,
                statics,
                args,
            } => {
                self.meta(f, *callee, depth)?;
                self.statics(f, statics, depth)?;
                f.write_str("(")?;
                self.list(f, args, depth, Self::obj)?;
                f.write_str(")")
            }
            HMIRObjKind::CallIndirect { callee, args } => {
                f.write_str("call_indirect ")?;
                self.obj(f, *callee, depth)?;
                f.write_str("(")?;
                self.list(f, args, depth, Self::obj)?;
                f.write_str(")")
            }

            HMIRObjKind::Block { statements, tail } => self.block(
                f,
                statements,
                *tail,
                depth,
                Self::obj,
                Self::is_structured_obj,
            ),
            HMIRObjKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                f.write_str("if (")?;
                self.obj(f, *condition, depth)?;
                f.write_str(") ")?;
                self.branches(f, *then_branch, *else_branch, depth)
            }
            HMIRObjKind::StaticIf {
                condition,
                then_branch,
                else_branch,
            } => {
                f.write_str("static if (")?;
                self.meta(f, *condition, depth)?;
                f.write_str(") ")?;
                self.branches(f, *then_branch, *else_branch, depth)
            }
            HMIRObjKind::While {
                condition,
                body,
                pre_eval: true,
            } => {
                f.write_str("while (")?;
                self.obj(f, *condition, depth)?;
                f.write_str(") ")?;
                self.obj(f, *body, depth)
            }
            HMIRObjKind::While {
                condition,
                body,
                pre_eval: false,
            } => {
                f.write_str("do ")?;
                self.obj(f, *body, depth)?;
                f.write_str(" while (")?;
                self.obj(f, *condition, depth)?;
                f.write_str(")")
            }
            HMIRObjKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                f.write_str("for (")?;
                self.obj(f, *init, depth)?;
                f.write_str("; ")?;
                self.obj(f, *condition, depth)?;
                f.write_str("; ")?;
                self.obj(f, *increment, depth)?;
                f.write_str(") ")?;
                self.obj(f, *body, depth)
            }
            HMIRObjKind::Switch {
                condition,
                cases,
                default,
            } => {
                f.write_str("switch (")?;
                self.obj(f, *condition, depth)?;
                f.write_str(") {\n")?;
                for (value, body) in cases {
                    indent(f, depth + 1)?;
                    f.write_str("case ")?;
                    self.meta(f, *value, depth + 1)?;
                    f.write_str(": ")?;
                    self.obj(f, *body, depth + 1)?;
                    f.write_str("\n")?;
                }
                if let Some(default) = default {
                    indent(f, depth + 1)?;
                    f.write_str("default: ")?;
                    self.obj(f, *default, depth + 1)?;
                    f.write_str("\n")?;
                }
                indent(f, depth)?;
                f.write_str("}")
            }
            HMIRObjKind::Match {
                scrutinee,
                subject,
                arms,
            } => {
                f.write_str("match (")?;
                self.obj(f, *scrutinee, depth)?;
                f.write_str(") as ")?;
                self.obj_local(f, *subject)?;
                f.write_str(" {\n")?;
                for (pattern, body) in arms {
                    indent(f, depth + 1)?;
                    self.pattern(f, pattern, depth + 1)?;
                    f.write_str(" => ")?;
                    self.obj(f, *body, depth + 1)?;
                    f.write_str("\n")?;
                }
                indent(f, depth)?;
                f.write_str("}")
            }
            HMIRObjKind::Break => f.write_str("break"),
            HMIRObjKind::Continue => f.write_str("continue"),
            HMIRObjKind::Goto(label) => write!(f, "goto {label}"),
            HMIRObjKind::Label { name, body } => {
                write!(f, "{name}: ")?;
                self.obj(f, *body, depth)
            }
            HMIRObjKind::Return(value) => self.keyword_value(f, "return", *value, depth),
            HMIRObjKind::Yield(value) => self.keyword_value(f, "yield", *value, depth),
            HMIRObjKind::Unreachable => f.write_str("unreachable"),

            HMIRObjKind::Defer(body) => {
                f.write_str("defer ")?;
                self.obj(f, *body, depth)
            }
            HMIRObjKind::Leak(value) => {
                f.write_str("leak ")?;
                self.obj(f, *value, depth)
            }
            HMIRObjKind::Unsafe(body) => {
                f.write_str("unsafe ")?;
                self.obj(f, *body, depth)
            }

            HMIRObjKind::Error => f.write_str("<error>"),
        }
    }

    fn is_structured_obj(&self, id: HMIRObjID) -> bool {
        match self.body().obj(id).kind() {
            HMIRObjKind::Block { .. }
            | HMIRObjKind::If { .. }
            | HMIRObjKind::StaticIf { .. }
            | HMIRObjKind::For { .. }
            | HMIRObjKind::Switch { .. }
            | HMIRObjKind::Match { .. }
            | HMIRObjKind::While { pre_eval: true, .. } => true,
            HMIRObjKind::Label { body, .. } => self.is_structured_obj(*body),
            _ => false,
        }
    }

    fn statics(
        &self,
        f: &mut Formatter<'_>,
        statics: &[HMIRMetaID],
        depth: usize,
    ) -> fmt::Result {
        if statics.is_empty() {
            return Ok(());
        }
        f.write_str("[")?;
        self.list(f, statics, depth, Self::meta)?;
        f.write_str("]")
    }

    fn branches(
        &self,
        f: &mut Formatter<'_>,
        then_branch: HMIRObjID,
        else_branch: Option<HMIRObjID>,
        depth: usize,
    ) -> fmt::Result {
        self.obj(f, then_branch, depth)?;
        if let Some(else_branch) = else_branch {
            f.write_str(" else ")?;
            self.obj(f, else_branch, depth)?;
        }
        Ok(())
    }

    fn keyword_value(
        &self,
        f: &mut Formatter<'_>,
        keyword: &str,
        value: Option<HMIRObjID>,
        depth: usize,
    ) -> fmt::Result {
        f.write_str(keyword)?;
        if let Some(value) = value {
            f.write_str(" ")?;
            self.obj(f, value, depth)?;
        }
        Ok(())
    }

    fn initializer(
        &self,
        f: &mut Formatter<'_>,
        initializer: &HMIRInitializer,
        depth: usize,
    ) -> fmt::Result {
        match initializer {
            HMIRInitializer::Struct(fields) => {
                f.write_str("{ ")?;
                for (index, (field, value)) in fields.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, ".{field} = ")?;
                    self.obj(f, *value, depth)?;
                }
                f.write_str(" }")
            }
            HMIRInitializer::Array(elements) => {
                f.write_str("[")?;
                self.list(f, elements, depth, Self::obj)?;
                f.write_str("]")
            }
            HMIRInitializer::Variant { index, value } => {
                write!(f, "variant {index}(")?;
                self.obj(f, *value, depth)?;
                f.write_str(")")
            }
            HMIRInitializer::Designated(fields) => {
                f.write_str("{ ")?;
                for (index, (name, value)) in fields.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    if let Some(name) = name {
                        write!(f, ".{name} = ")?;
                    }
                    self.obj(f, *value, depth)?;
                }
                f.write_str(" }")
            }
        }
    }

    fn pattern(&self, f: &mut Formatter<'_>, pattern: &HMIRPattern, depth: usize) -> fmt::Result {
        match pattern {
            HMIRPattern::Binding(local) => self.obj_local_decl(f, *local),
            HMIRPattern::Integer(value) => write!(f, "{value}"),
            HMIRPattern::Float(value) => write!(f, "{value}"),
            HMIRPattern::Variant { sum, index, inner } => {
                self.meta(f, *sum, depth)?;
                write!(f, "::variant {index}")?;
                if let Some(inner) = inner {
                    f.write_str("(")?;
                    self.obj_local_decl(f, *inner)?;
                    f.write_str(")")?;
                }
                Ok(())
            }
        }
    }
}
