use std::fmt::{self, Formatter};

use crate::{
    def::HMIRDefRef,
    expr::HMIRConstant,
    ty::desc::{HMIRTypeDesc, HMIRTypeID},
    unit::HMIRUnit,
};

pub(super) fn write_def_ref(
    f: &mut Formatter<'_>,
    unit: &HMIRUnit,
    def: &HMIRDefRef,
) -> fmt::Result {
    match def {
        HMIRDefRef::Local(id) => write!(f, "@{}", unit.def(*id).name()),
        HMIRDefRef::External(name) => write!(f, "@{name}"),
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
    match unit.types().get(id) {
        HMIRTypeDesc::Void => f.write_str("void"),
        HMIRTypeDesc::Unreachable => f.write_str("never"),
        HMIRTypeDesc::Type => f.write_str("@type"),
        HMIRTypeDesc::Str => f.write_str("str"),
        HMIRTypeDesc::Int { width, signed } => {
            write!(f, "{}{}", if *signed { "i" } else { "u" }, width.bits())
        }
        HMIRTypeDesc::Float { width } => write!(f, "f{}", width.bits()),
        HMIRTypeDesc::Pointer(inner) => {
            write_type(f, unit, *inner)?;
            f.write_str("*")
        }
        HMIRTypeDesc::Reference(inner) => {
            write_type(f, unit, *inner)?;
            f.write_str("&")
        }
        HMIRTypeDesc::Array { element, length } => {
            write_type(f, unit, *element)?;
            match length {
                Some(length) => write!(f, "[{length}]"),
                None => f.write_str("[]"),
            }
        }
        HMIRTypeDesc::Function(signature) => {
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
            write_type(f, unit, signature.ret())
        }
        HMIRTypeDesc::Expr { params, result } => {
            f.write_str("expr(")?;
            write_type_list(f, unit, params)?;
            f.write_str(") -> ")?;
            write_type(f, unit, *result)
        }
        HMIRTypeDesc::Nominal(id) => {
            let nominal = unit.types().nominal(*id);
            write_def_ref(f, unit, nominal.def())?;
            if nominal.args().is_empty() {
                return Ok(());
            }
            f.write_str("(")?;
            for (index, arg) in nominal.args().iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                write_constant(f, unit, arg)?;
            }
            f.write_str(")")
        }
        HMIRTypeDesc::Opaque { size, alignment } => write!(f, "opaque({size}, {alignment})"),
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
