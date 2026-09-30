use std::fmt::{self, Formatter};

use cx_intrinsics::{Intrinsic, IntrinsicArg};

use crate::{
    body::HMIRBody,
    ids::{HMIRMetaID, HMIRMetaLocalID, HMIRObjLocalID},
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

    pub(super) fn meta_local(&self, f: &mut Formatter<'_>, id: HMIRMetaLocalID) -> fmt::Result {
        match self.body.meta_local(id).name() {
            Some(name) => write!(f, "${name}"),
            None => write!(f, "{id}"),
        }
    }

    pub(super) fn obj_local(&self, f: &mut Formatter<'_>, id: HMIRObjLocalID) -> fmt::Result {
        match self.body.obj_local(id).name() {
            Some(name) => write!(f, "%{name}"),
            None => write!(f, "{id}"),
        }
    }

    pub(super) fn meta_local_decl(
        &self,
        f: &mut Formatter<'_>,
        id: HMIRMetaLocalID,
    ) -> fmt::Result {
        self.meta_local(f, id)?;
        f.write_str(": ")?;
        self.meta(f, self.body.meta_local(id).ty(), 0)
    }

    pub(super) fn obj_local_decl(&self, f: &mut Formatter<'_>, id: HMIRObjLocalID) -> fmt::Result {
        self.obj_local(f, id)?;
        f.write_str(": ")?;
        self.meta(f, self.body.obj_local(id).ty(), 0)
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

    pub(super) fn block<I: Copy>(
        &self,
        f: &mut Formatter<'_>,
        statements: &[I],
        tail: Option<I>,
        depth: usize,
        write: impl Fn(&Self, &mut Formatter<'_>, I, usize) -> fmt::Result,
        is_structured: impl Fn(&Self, I) -> bool,
    ) -> fmt::Result {
        f.write_str("{\n")?;
        for statement in statements {
            indent(f, depth + 1)?;
            write(self, f, *statement, depth + 1)?;
            if !is_structured(self, *statement) {
                f.write_str(";")?;
            }
            f.write_str("\n")?;
        }
        if let Some(tail) = tail {
            indent(f, depth + 1)?;
            write(self, f, tail, depth + 1)?;
            f.write_str("\n")?;
        }
        indent(f, depth)?;
        f.write_str("}")
    }

    pub(super) fn intrinsic<V>(
        &self,
        f: &mut Formatter<'_>,
        intrinsic: &Intrinsic<V, HMIRMetaID>,
        depth: usize,
        value: impl Fn(&Self, &mut Formatter<'_>, &V, usize) -> fmt::Result,
    ) -> fmt::Result {
        write!(f, "@intrinsic.{}(", intrinsic.path())?;
        for (index, arg) in intrinsic.args().into_iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            match arg {
                IntrinsicArg::Value(operand) => value(self, f, operand, depth)?,
                IntrinsicArg::Type(ty) => self.meta(f, *ty, depth)?,
                IntrinsicArg::Flag(flag) => write!(f, "{flag}")?,
                IntrinsicArg::Message(message) => write!(f, "{message:?}")?,
            }
        }
        f.write_str(")")
    }
}
