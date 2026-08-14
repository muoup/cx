use std::fmt::{self, Display, Formatter};

mod contextual;
pub use contextual::MIRDisplay;

use crate::{
    MIRLayoutError, MIRTypeID,
    expr::{
        MIRAggregateOp, MIRBasicBlock, MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRInstr,
        MIRInstrKind, MIRParameterID, MIRPlace, MIRPlaceAggregateOp, MIRPlaceID, MIRRegister,
        MIRValue, MIRValueAggregateOp,
    },
    global::{
        MIRFnSignature, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
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

impl Display for MIRInstr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.kind, f)
    }
}

impl Display for MIRInstrKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ScopeEnter { scope } => write!(f, "scope.enter {scope:?}"),
            Self::ScopeExit { scope } => write!(f, "scope.exit {scope:?}"),
            Self::Initialize { place } => write!(f, "initialize {place}"),
            Self::Leak { place } => write!(f, "leak {place}"),
            Self::Create { out, ty } => write!(f, "create {out}: {ty}"),
            Self::Assign { dest, value, ty } => write!(f, "{dest} = {value}: {ty}"),
            Self::AddressOf { out, place } => write!(f, "{out} = address_of {place}"),
            Self::Dereference {
                out,
                pointer,
                pointee_type,
            } => write!(f, "{out} = dereference {pointer}: {pointee_type}"),
            Self::AggregateOp(operation) => match operation {
                MIRAggregateOp::Place {
                    out,
                    op:
                        MIRPlaceAggregateOp::Field {
                            base,
                            field,
                            aggregate_type,
                        },
                } => write!(
                    f,
                    "{out} = aggregate.field {base}.{field}: {aggregate_type}"
                ),
                MIRAggregateOp::Place {
                    out,
                    op:
                        MIRPlaceAggregateOp::Index {
                            base,
                            index,
                            element_type,
                        },
                } => write!(f, "{out} = aggregate.index {base}[{index}]: {element_type}"),
                MIRAggregateOp::Place {
                    out,
                    op:
                        MIRPlaceAggregateOp::Variant {
                            base,
                            variant,
                            sum_type,
                        },
                } => write!(
                    f,
                    "{out} = aggregate.variant.place {base}.{variant}: {sum_type}"
                ),
                MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Discriminant { value, sum_type },
                } => write!(f, "{out} = aggregate.discriminant {value}: {sum_type}"),
                MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Construct { ty, fields },
                } => {
                    write!(f, "{out} = aggregate.construct {ty} ")?;
                    write_fields(f, fields)
                }
                MIRAggregateOp::Value {
                    out,
                    op:
                        MIRValueAggregateOp::Variant {
                            variant,
                            value,
                            sum_type,
                        },
                } => write!(f, "{out} = aggregate.variant {sum_type}.{variant}({value})"),
            },
            Self::Call { out, callee, args } => {
                write_optional_register(f, *out)?;
                write!(f, "call {callee}(")?;
                write_values(f, args)?;
                f.write_str(")")
            }
            Self::Emit { value } => write!(f, "emit {value}"),
            Self::BinOp { out, op, lhs, rhs } => write!(f, "{out} = {op} {lhs}, {rhs}"),
            Self::UnOp { out, op, operand } => write!(f, "{out} = {op} {operand}"),
            Self::Coerce {
                out,
                operand,
                coercion,
                to_type,
            } => write!(f, "{out} = coerce {operand} via {coercion} to {to_type}"),
            Self::Assert { condition, message } => {
                write!(f, "assert {condition}")?;
                if let Some(message) = message {
                    write!(f, ", {message:?}")?;
                }
                Ok(())
            }
            Self::Assume { condition } => write!(f, "assume {condition}"),
            Self::Return { value } => {
                f.write_str("return")?;
                if let Some(value) = value {
                    write!(f, " {value}")?;
                }
                Ok(())
            }
            Self::Jump { target } => write!(f, "jump {target}"),
            Self::Branch {
                cond,
                true_target,
                false_target,
            } => write!(f, "branch {cond}, {true_target}, {false_target}"),
            Self::IntSwitch {
                value,
                cases,
                default,
            } => {
                write!(f, "switch {value} [")?;
                for (index, (case, target)) in cases.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{case} => {target}")?;
                }
                if let Some(default) = default {
                    if !cases.is_empty() {
                        f.write_str(", ")?;
                    }
                    write!(f, "_ => {default}")?;
                }
                f.write_str("]")
            }
            Self::VariantSwitch {
                subject,
                sum_type,
                cases,
                default,
                ..
            } => {
                write!(f, "variant_switch {subject}: {sum_type} [")?;
                for (index, (variant, target)) in cases.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{variant} => {target}")?;
                }
                if let Some(default) = default {
                    if !cases.is_empty() {
                        f.write_str(", ")?;
                    }
                    write!(f, "_ => {default}")?;
                }
                f.write_str("]")
            }
            Self::Unreachable => f.write_str("unreachable"),
        }
    }
}

fn write_fields(f: &mut Formatter<'_>, fields: &[(usize, MIRValue)]) -> fmt::Result {
    f.write_str("{")?;
    for (index, (field, value)) in fields.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        write!(f, "{field}: {value}")?;
    }
    f.write_str("}")
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

fn write_optional_register(f: &mut Formatter<'_>, out: Option<MIRRegister>) -> fmt::Result {
    if let Some(out) = out {
        write!(f, "{out} = ")?;
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

impl Display for MIRBasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
        if !self.params.is_empty() {
            f.write_str("(")?;
            for (index, param) in self.params.iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                Display::fmt(param, f)?;
            }
            f.write_str(")")?;
        }
        if let Some(name) = &self.debug_name {
            write!(f, " ({name})")?;
        }
        f.write_str(":\n")?;
        for instruction in &self.instrs {
            writeln!(f, "    {instruction}")?;
        }
        Ok(())
    }
}

impl Display for MIRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn {} {} [{:?}]",
            self.id, self.prototype.signature, self.prototype.linkage
        )?;
        if self.is_declaration() {
            return f.write_str(";\n");
        }
        f.write_str(" {\n")?;
        if let Some(entry) = self.entry {
            writeln!(f, "  entry {entry}")?;
        }
        for place in &self.places {
            write!(f, "  place {}: {}", place.id, place.ty)?;
            if let Some(name) = &place.debug_name {
                write!(f, " ({name})")?;
            }
            f.write_str("\n")?;
        }
        for register in &self.registers {
            write!(f, "  register {}: {}", register.id, register.ty)?;
            if let Some(name) = &register.debug_name {
                write!(f, " ({name})")?;
            }
            f.write_str("\n")?;
        }
        for block in &self.blocks {
            write!(f, "  {block}")?;
        }
        f.write_str("}\n")
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
