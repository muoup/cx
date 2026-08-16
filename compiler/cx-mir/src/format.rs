use std::fmt::{self, Display, Formatter};

mod contextual;
pub(crate) use contextual::TypePrinter;
pub use contextual::MIRDisplay;

use crate::{
    MIRLayoutError, MIRTypeID,
    expr::{
        MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRParameterID, MIRPlace, MIRPlaceID, MIRRegister,
        MIRValue,
    },
    global::{
        MIRFnSignature, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    },
    op::{MIRBinaryOp, MIRCoercion, MIRUnaryOp},
    ty::MIRIntType,
    unit::MIRUnit,
};

impl Display for MIRPlaceID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%p{}", self.index())
    }
}

impl Display for MIRParameterID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%arg{}", self.index())
    }
}

impl Display for MIRPlace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::FunctionLocal(id) => Display::fmt(id, f),
            Self::Parameter(id) => Display::fmt(id, f),
            Self::Global(id) => Display::fmt(id, f),
        }
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
            Self::Bool(value) => Display::fmt(value, f),
            Self::String(value) => write!(f, "{:?}", value),
            Self::Integer { value, ty, signed } => write!(
                f,
                "{value}:{}{}",
                if *signed { "i" } else { "u" },
                int_width(*ty)
            ),
            Self::Float { value, ty } => write!(f, "{value}:{ty:?}"),
            Self::Null { ty } => write!(f, "null:{ty}"),
            Self::Function(function) => write!(f, "fn {function}"),
            Self::Undefined => f.write_str("undef"),
        }
    }
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Register(value) => Display::fmt(value, f),
            Self::Place(value) => Display::fmt(value, f),
            Self::Copy(place) => write!(f, "copy {place}"),
            Self::Move(place) => write!(f, "move {place}"),
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
        write!(f, "fn {}(", self.display_name())?;
        f.write_str("(")?;
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
        write!(f, ") -> {} /* {} * /", self.return_type, self.symbol_name)
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

impl Display for MIRGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "global {} {}: {} [{:?}, {}]",
            self.id,
            self.name,
            self.ty,
            self.linkage,
            if self.is_mutable {
                "mutable"
            } else {
                "readonly"
            }
        )?;
        write!(f, " = {}", self.state)
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

impl fmt::Display for MIRLayoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidType(id) => write!(f, "invalid MIR type {id}"),
            Self::DuplicateType(id) => write!(f, "MIR type {id} was defined more than once"),
            Self::RecursiveType(id) => {
                write!(f, "cannot compute layout of recursive MIR type {id}")
            }
            Self::InvalidBitfieldWidth {
                width,
                storage_bits,
            } => write!(
                f,
                "invalid bitfield width: {width} exceeds storage size of {storage_bits} bits"
            ),
            Self::InvalidAlignment(alignment) => write!(f, "invalid type alignment {alignment}"),
            Self::InvalidField { ty, field } => {
                write!(f, "MIR type {ty} has no field at index {field}")
            }
            Self::SizeOverflow => f.write_str("MIR type layout size overflowed usize"),
        }
    }
}
