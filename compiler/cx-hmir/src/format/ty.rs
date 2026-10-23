use std::fmt::{self, Formatter};

use crate::{
    expr::literal::HMIRLiteral,
    ty::{
        HMIRGenericType, HMIRGenericTypeKind,
        aggregate::HMIRFieldDef,
        intrinsic::{HMIRFloatType, HMIRIntType},
    },
    unit::{HMIRUnit, def::HMIRDefID},
};

pub(super) fn write_def(f: &mut Formatter<'_>, unit: &HMIRUnit, def: HMIRDefID) -> fmt::Result {
    match unit.resolve_def(def) {
        Some(def) => write!(f, "@{}", def.name()),
        None => write!(f, "@{def}"),
    }
}

pub(super) fn write_literal(f: &mut Formatter<'_>, literal: &HMIRLiteral) -> fmt::Result {
    match literal {
        HMIRLiteral::Unit => f.write_str("()"),
        HMIRLiteral::Int { value, ty } => {
            write!(f, "{value}:")?;
            write_int_type(f, ty)
        }
        HMIRLiteral::Float { value, ty } => {
            write!(f, "{value}:")?;
            write_float_type(f, ty)
        }
        HMIRLiteral::Str(value) => write!(f, "{value:?}"),
    }
}

fn write_int_type(f: &mut Formatter<'_>, ty: &HMIRIntType) -> fmt::Result {
    write!(
        f,
        "{}{}",
        if ty.signed() { "i" } else { "u" },
        ty.width().bytes() * 8
    )
}

fn write_float_type(f: &mut Formatter<'_>, ty: &HMIRFloatType) -> fmt::Result {
    write!(f, "f{}", ty.width().bytes() * 8)
}

pub(super) fn write_generic_type<T, L, M>(
    f: &mut Formatter<'_>,
    ty: &HMIRGenericType<T, L, M>,
    write_inner: &impl Fn(&mut Formatter<'_>, &T) -> fmt::Result,
    write_length: &impl Fn(&mut Formatter<'_>, &L) -> fmt::Result,
) -> fmt::Result {
    match ty.kind() {
        HMIRGenericTypeKind::Void => f.write_str("void"),
        HMIRGenericTypeKind::Unreachable => f.write_str("never"),
        HMIRGenericTypeKind::Type => f.write_str("@type"),
        HMIRGenericTypeKind::Str => f.write_str("str"),
        HMIRGenericTypeKind::Int(ty) => write_int_type(f, ty),
        HMIRGenericTypeKind::Float(ty) => write_float_type(f, ty),
        HMIRGenericTypeKind::PointerTo(inner) => {
            write_inner(f, inner)?;
            f.write_str("*")
        }
        HMIRGenericTypeKind::ReferenceTo(inner) => {
            write_inner(f, inner)?;
            f.write_str("&")
        }
        HMIRGenericTypeKind::Array { element, length } => {
            write_inner(f, element)?;
            f.write_str("[")?;
            if let Some(length) = length {
                write_length(f, length)?;
            }
            f.write_str("]")
        }
        HMIRGenericTypeKind::Function(signature) => {
            f.write_str("fn(")?;
            for (index, param) in signature.params().iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                if param.comptime() {
                    f.write_str("comptime ")?;
                }
                write_inner(f, param.ty())?;
            }
            if signature.is_variadic() {
                f.write_str(if signature.params().is_empty() {
                    "..."
                } else {
                    ", ..."
                })?;
            }
            f.write_str(") -> ")?;
            write_inner(f, signature.return_type())
        }
        HMIRGenericTypeKind::Struct { fields } => write_fields(f, "struct", fields, write_inner),
        HMIRGenericTypeKind::Union { fields } => write_fields(f, "union", fields, write_inner),
        HMIRGenericTypeKind::TaggedUnion { fields } => {
            write_fields(f, "enum union", fields, write_inner)
        }
        HMIRGenericTypeKind::StagedExpr { params, result } => {
            f.write_str("expr(")?;
            for (index, param) in params.iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                write_inner(f, param)?;
            }
            f.write_str(") -> ")?;
            write_inner(f, result)
        }
        HMIRGenericTypeKind::Opaque { size, alignment } => {
            write!(f, "opaque({size}, {alignment})")
        }
        HMIRGenericTypeKind::Nominal(inner) => {
            if ty.semantics().constant {
                f.write_str("const ")?;
            }
            write_inner(f, inner)
        }
    }
}

fn write_fields<T>(
    f: &mut Formatter<'_>,
    keyword: &str,
    fields: &[HMIRFieldDef<T>],
    write_inner: &impl Fn(&mut Formatter<'_>, &T) -> fmt::Result,
) -> fmt::Result {
    write!(f, "{keyword} {{")?;
    for (index, field) in fields.iter().enumerate() {
        f.write_str(if index == 0 { " " } else { ", " })?;
        match field.name() {
            Some(name) => write!(f, "{name}: ")?,
            None => f.write_str("_: ")?,
        }
        write_inner(f, field.ty())?;
        if let Some(width) = field.bit_width() {
            write!(f, " : {width}")?;
        }
    }
    f.write_str(" }")
}
