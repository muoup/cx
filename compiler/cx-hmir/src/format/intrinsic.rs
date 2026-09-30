use std::fmt::{self, Formatter};

use cx_intrinsics::{Intrinsic, IntrinsicArg};
use cx_util::identifier::CXIdent;

use crate::{
    binding::HMIRObjLocalID,
    expr::{
        aggregate::HMIRPattern,
        intrinsic::{
            HMIRControlIntrinsic, HMIRMemoryIntrinsic, HMIRMetaIntrinsic,
            HMIRObjIntrinsic, HMIRVariantIntrinsic,
        },
        meta::HMIRMetaID,
        obj::HMIRObjID,
    },
};

use super::body::BodyPrinter;

enum Arg<'a> {
    Obj(HMIRObjID),
    Meta(HMIRMetaID),
    Index(usize),
    Flag(bool),
    Message(&'a str),
    Name(&'a CXIdent),
    Pattern(&'a HMIRPattern),
    Binding(usize, HMIRObjLocalID),
}

impl BodyPrinter<'_> {
    pub(super) fn obj_intrinsic(
        &self,
        f: &mut Formatter<'_>,
        intrinsic: &HMIRObjIntrinsic,
        depth: usize,
    ) -> fmt::Result {
        let args = match intrinsic {
            HMIRObjIntrinsic::TypedOp(native) => native_args(native, |value| Arg::Obj(*value)),
            HMIRObjIntrinsic::Control(control) => match control {
                HMIRControlIntrinsic::Defer(body)
                | HMIRControlIntrinsic::Leak(body)
                | HMIRControlIntrinsic::Unsafe(body) => vec![Arg::Obj(*body)],
                HMIRControlIntrinsic::Unreachable => vec![],
            },
        };
        self.call(f, intrinsic.path(), &args, depth)
    }

    pub(super) fn meta_intrinsic(
        &self,
        f: &mut Formatter<'_>,
        intrinsic: &HMIRMetaIntrinsic,
        depth: usize,
    ) -> fmt::Result {
        let args = match intrinsic {
            HMIRMetaIntrinsic::Native(native) => native_args(native, |value| Arg::Meta(*value)),
            HMIRMetaIntrinsic::Break | HMIRMetaIntrinsic::Continue => vec![],
            HMIRMetaIntrinsic::Label(name) => vec![Arg::Name(name)],
            HMIRMetaIntrinsic::Branch(target) => vec![Arg::Meta(*target)],
            HMIRMetaIntrinsic::CompileError(message) => vec![Arg::Meta(*message)],
        };
        self.call(f, intrinsic.path(), &args, depth)
    }

    fn call(
        &self,
        f: &mut Formatter<'_>,
        path: &str,
        args: &[Arg<'_>],
        depth: usize,
    ) -> fmt::Result {
        f.write_str(path)?;
        if args.is_empty() {
            return Ok(());
        }
        f.write_str("(")?;
        for (index, arg) in args.iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            match arg {
                Arg::Obj(value) => self.obj(f, *value, depth)?,
                Arg::Meta(value) => self.meta(f, *value, depth)?,
                Arg::Index(value) => write!(f, "{value}")?,
                Arg::Flag(flag) => write!(f, "{flag}")?,
                Arg::Message(message) => write!(f, "{message:?}")?,
                Arg::Name(name) => write!(f, "{name}")?,
                Arg::Pattern(pattern) => self.pattern(f, pattern, depth)?,
                Arg::Binding(field, local) => {
                    write!(f, "{field} => ")?;
                    self.obj_local_decl(f, *local)?;
                }
            }
        }
        f.write_str(")")
    }
}

fn native_args<'a, V>(
    intrinsic: &'a Intrinsic<V, HMIRMetaID>,
    value: impl Fn(&V) -> Arg<'a>,
) -> Vec<Arg<'a>> {
    intrinsic
        .args()
        .into_iter()
        .map(|arg| match arg {
            IntrinsicArg::Value(operand) => value(operand),
            IntrinsicArg::Type(ty) => Arg::Meta(*ty),
            IntrinsicArg::Bool(flag) => Arg::Flag(flag),
            IntrinsicArg::String(message) => Arg::Message(message),
        })
        .collect()
}
