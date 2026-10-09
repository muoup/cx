use std::fmt::{self, Formatter};

use crate::{
    expr::constant::HMIRConstant,
    ty::{HMIRTypeID, HMIRTypeKind},
    unit::{HMIRUnit, def::HMIRDefRef},
};

pub(super) fn write_def_ref(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDefRef,
) -> fmt::Result {
    match def {
        HMIRDefRef::Local(id) => write!(f, "@{}", unit.resolve_def(*id).name()),
        HMIRDefRef::External(name) => write!(f, "@{name}"),
        HMIRDefRef::Candidates(candidates) => {
            for (index, candidate) in candidates.iter().enumerate() {
                if index > 0 {
                    f.write_str(" | ")?;
                }
                write_def_ref(f, unit, candidate)?;
            }
            Ok(())
        }
    }
}

pub(super) fn write_constant(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    constant: &HMIRConstant,
) -> fmt::Result {
    match constant {
        HMIRConstant::Unit => f.write_str("()"),
        HMIRConstant::Bool(value) => write!(f, "{value}"),
        HMIRConstant::Int { value, ty } => {
            write!(f, "{value}:")?;
            write_type(f, unit, *ty)
        }
        HMIRConstant::Float { value, ty } => {
            write!(f, "{value}:")?;
            write_type(f, unit, *ty)
        }
        HMIRConstant::Str(value) => write!(f, "{value:?}"),
        HMIRConstant::Null(ty) => {
            f.write_str("null:")?;
            write_type(f, unit, *ty)
        }
        HMIRConstant::Type(ty) => write_type(f, unit, *ty),
    }
}

pub(super) fn write_type(f: &mut Formatter<'_>, unit: &HMIRUnit, id: HMIRTypeID) -> fmt::Result {
    match unit.resolve_type(id).kind() {
        HMIRTypeKind::Void => f.write_str("void"),
        HMIRTypeKind::Unreachable => f.write_str("never"),
        HMIRTypeKind::Type => f.write_str("@type"),
        HMIRTypeKind::Str => f.write_str("str"),
        HMIRTypeKind::Int(ty) => {
            write!(
                f,
                "{}{}",
                if ty.signed() { "i" } else { "u" },
                ty.width().bytes() * 8
            )
        }
        HMIRTypeKind::Float(ty) => write!(f, "f{}", ty.width().bytes() * 8),
        HMIRTypeKind::PointerTo(inner) => {
            write_type(f, unit, *inner)?;
            f.write_str("*")
        }
        HMIRTypeKind::ReferenceTo(inner) => {
            write_type(f, unit, *inner)?;
            f.write_str("&")
        }
        HMIRTypeKind::Array { element, length } => {
            write_type(f, unit, *element)?;
            match length {
                Some(length) => write!(f, "[{length}]"),
                None => f.write_str("[]"),
            }
        }
        HMIRTypeKind::Function(signature) => {
            f.write_str("fn(")?;
            write_type_list(f, unit, signature.params())?;
            if signature.is_variadic() {
                f.write_str(if signature.params().is_empty() {
                    "..."
                } else {
                    ", ..."
                })?;
            }
            f.write_str(") -> ")?;
            write_type(f, unit, signature.return_type())
        }
        HMIRTypeKind::StagedExpr { params, result } => {
            f.write_str("expr(")?;
            write_type_list(f, unit, params)?;
            f.write_str(") -> ")?;
            write_type(f, unit, *result)
        }
        HMIRTypeKind::Opaque { size, alignment } => write!(f, "opaque({size}, {alignment})"),
    }
}

fn write_type_list(f: &mut Formatter<'_>, unit: &HMIRUnit, types: &[HMIRTypeID]) -> fmt::Result {
    for (index, ty) in types.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        write_type(f, unit, *ty)?;
    }
    Ok(())
}
