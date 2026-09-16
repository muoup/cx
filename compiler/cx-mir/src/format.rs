use std::fmt::{self, Display, Formatter};

mod contextual;

pub use contextual::MIRDisplay;

use crate::{
    MIRLayoutError, MIRTypeID,
    global::{MIRFnSignature, MIRFunctionID, MIRGlobalID, MIRGlobalState},
    instruction::{
        MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRPlaceID, MIRRegister, MIRValue,
    },
    layout_error,
    op::{MIRBinaryOp, MIRCoercion, MIRUnaryOp},
    ty::MIRIntType,
    unit::MIRUnit,
};

impl Display for MIRPlaceID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%p{}", self.index())
    }
}

impl Display for MIRRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%r{}", self.index())
    }
}

impl Display for MIRBasicBlockID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.index())
    }
}

impl Display for MIRBlockTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.block, f)?;
        if self.args.is_empty() {
            return Ok(());
        }
        f.write_str("(")?;
        write_values(f, &self.args)?;
        f.write_str(")")
    }
}

impl Display for MIRFunctionID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "@f{}", self.index())
    }
}

impl Display for MIRGlobalID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "@g{}", self.index())
    }
}

fn int_width(ty: MIRIntType) -> u16 {
    match ty {
        MIRIntType::I1 => 1,
        MIRIntType::I8 => 8,
        MIRIntType::I16 => 16,
        MIRIntType::I32 => 32,
        MIRIntType::I64 => 64,
        MIRIntType::I128 => 128,
    }
}

impl Display for MIRConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("()"),
            Self::Integer { value, ty, signed } => write!(
                f,
                "{value}:{}{}",
                if *signed { "i" } else { "u" },
                int_width(*ty)
            ),
            Self::Float { value, ty } => write!(f, "{value}:{ty:?}"),
            Self::Nullptr { .. } => write!(f, "null"),
            Self::Aggregate { fields, .. } => {
                f.write_str("{")?;
                for (index, value) in fields.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}: {}", value.0, value.1)?;
                }
                f.write_str("}")
            }
            Self::Global { global, offset, .. } => {
                write!(f, "global {global}")?;

                if *offset != 0 {
                    write!(f, " + {offset}")?;
                }

                Ok(())
            }
            Self::Function(function) => write!(f, "fn {function}"),
            Self::Undefined => f.write_str("undefined"),
        }
    }
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register(value) => Display::fmt(value, f),
            Self::Place(value) => Display::fmt(value, f),
            Self::Global(value) => Display::fmt(value, f),
            Self::Constant(value) => Display::fmt(value, f),
        }
    }
}

impl Display for MIRBinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Integer { ty, signed, op } => write!(
                f,
                "{op:?}.{}{}",
                if *signed { "i" } else { "u" },
                int_width(*ty)
            ),
            Self::Float { ty, op } => write!(f, "{op:?}.{ty:?}"),
            Self::PointerOffset { op, pointee } => write!(f, "ptr_{op:?}.{pointee}"),
            Self::Pointer(op) => write!(f, "ptr_{op:?}"),
        }
    }
}

impl Display for MIRUnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::IntegerNeg { ty, signed } => write!(
                f,
                "neg.{}{}",
                if *signed { "i" } else { "u" },
                int_width(*ty)
            ),
            Self::FloatNeg(ty) => write!(f, "fneg.{ty:?}"),
            Self::BitNot(ty) => write!(f, "bit_not.i{}", int_width(*ty)),
            Self::LogicalNot => f.write_str("logical_not"),
            Self::Increment { amount, post } => write!(
                f,
                "{}increment({amount})",
                if *post { "post_" } else { "pre_" }
            ),
        }
    }
}

impl Display for MIRCoercion {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

fn write_values(f: &mut Formatter<'_>, values: &[MIRValue]) -> fmt::Result {
    for (index, value) in values.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        Display::fmt(value, f)?;
    }
    Ok(())
}

impl Display for MIRFnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn (")?;
        for (i, param) in self.params.iter().enumerate() {
            if i != 0 {
                f.write_str(", ")?;
            }
            if let Some(name) = &param.name {
                write!(f, "{name}: ")?;
            }
            Display::fmt(&param.ty, f)?;
        }
        if self.variadic {
            if !self.params.is_empty() {
                f.write_str(", ")?;
            }
            f.write_str("...")?;
        }
        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for MIRGlobalState {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::External => f.write_str("external"),
            Self::ZeroInitialized => f.write_str("zero"),
            Self::Initialized(value) => Display::fmt(value, f),
        }
    }
}

impl Display for MIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.display_pretty(), f)
    }
}

impl fmt::Display for MIRTypeID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "t{}", self.index())
    }
}

impl std::fmt::Display for MIRLayoutError {
    fn fmt(&self, output: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        output.write_str(&layout_error(self.clone()).message())
    }
}
