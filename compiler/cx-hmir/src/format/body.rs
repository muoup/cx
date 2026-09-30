use std::fmt::{self, Formatter};

use crate::{
    binding::HMIRLocalID,
    body::HMIRBody,
    expr::kind::{HMIRExprID, HMIRExprKind},
    unit::HMIRUnit,
};

pub(super) struct BodyPrinter<'a> {
    unit: &'a HMIRUnit,
    body: &'a HMIRBody,
}

pub(super) fn indent(f: &mut Formatter<'_>, depth: usize) -> fmt::Result {
    for _ in 0..depth {
        f.write_str("    ")?;
    }
    Ok(())
}

impl<'a> BodyPrinter<'a> {
    pub(super) fn new(unit: &'a HMIRUnit, body: &'a HMIRBody) -> Self {
        Self { unit, body }
    }

    pub(super) fn unit(&self) -> &'a HMIRUnit {
        self.unit
    }

    pub(super) fn body(&self) -> &'a HMIRBody {
        self.body
    }

    pub(super) fn local(&self, f: &mut Formatter<'_>, id: HMIRLocalID) -> fmt::Result {
        let local = self.body.local(id);
        f.write_str(if local.is_comptime() { "$" } else { "%" })?;
        match local.name() {
            Some(name) => write!(f, "{name}"),
            None => write!(f, "{}", id.index()),
        }
    }

    pub(super) fn local_decl(&self, f: &mut Formatter<'_>, id: HMIRLocalID) -> fmt::Result {
        self.local(f, id)?;
        f.write_str(": ")?;
        self.expr(f, self.body.local(id).ty(), 0)
    }

    pub(super) fn param_decl(&self, f: &mut Formatter<'_>, id: HMIRLocalID) -> fmt::Result {
        if self.body.local(id).is_comptime() {
            f.write_str("comptime ")?;
        }
        self.local_decl(f, id)
    }

    pub(super) fn list<I: Copy>(
        &self,
        f: &mut Formatter<'_>,
        items: &[I],
        depth: usize,
        write: impl Fn(&Self, &mut Formatter<'_>, I, usize) -> fmt::Result,
    ) -> fmt::Result {
        for (index, item) in items.iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            write(self, f, *item, depth)?;
        }
        Ok(())
    }

    pub(super) fn block(
        &self,
        f: &mut Formatter<'_>,
        statements: &[HMIRExprID],
        tail: Option<HMIRExprID>,
        depth: usize,
    ) -> fmt::Result {
        f.write_str("{\n")?;
        for statement in statements {
            indent(f, depth + 1)?;
            self.statement(f, *statement, depth + 1)?;
            if !self.is_structured(*statement) {
                f.write_str(";")?;
            }
            f.write_str("\n")?;
        }
        if let Some(tail) = tail {
            indent(f, depth + 1)?;
            self.statement(f, tail, depth + 1)?;
            f.write_str("\n")?;
        }
        indent(f, depth)?;
        f.write_str("}")
    }

    pub(super) fn statement(
        &self,
        f: &mut Formatter<'_>,
        id: HMIRExprID,
        depth: usize,
    ) -> fmt::Result {
        match self.body.expr(id).kind() {
            HMIRExprKind::Comptime(inner) => {
                f.write_str("comptime ")?;
                self.expr(f, *inner, depth)
            }
            _ => self.expr(f, id, depth),
        }
    }

    pub(super) fn condition(
        &self,
        f: &mut Formatter<'_>,
        keyword: &str,
        condition: HMIRExprID,
        depth: usize,
    ) -> fmt::Result {
        match self.body.expr(condition).kind() {
            HMIRExprKind::Comptime(inner) => {
                write!(f, "comptime {keyword} (")?;
                self.expr(f, *inner, depth)?;
            }
            _ => {
                write!(f, "{keyword} (")?;
                self.expr(f, condition, depth)?;
            }
        }
        f.write_str(") ")
    }
}
