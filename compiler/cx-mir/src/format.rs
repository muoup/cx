use std::fmt::{self, Display, Formatter};

use cx_thir::thir::r#type::{THIRField, THIRIntType, THIRTypeKind};

use crate::{
    expr::{
        MIRAggregateKind, MIRBasicBlock, MIRBasicBlockID, MIRConstant, MIRDestination, MIRInstr,
        MIRInstrKind, MIROperand, MIRPlace, MIRRegister,
    },
    global::{MIRFnSignature, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalVariable},
    module::MIRUnit,
    op::{MIRBinaryOp, MIRCoercion, MIRUnaryOp},
    ty::MIRType,
};

impl Display for MIRPlace {
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

impl Display for MIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(name) = self.0.strong_identifier.as_deref() {
            return f.write_str(name);
        }
        match &self.0.kind {
            THIRTypeKind::Unit => f.write_str("unit"),
            THIRTypeKind::Integer { _type, signed } => {
                write!(
                    f,
                    "{}{}",
                    if *signed { "i" } else { "u" },
                    int_width(*_type)
                )
            }
            THIRTypeKind::Float { _type } => write!(f, "{_type:?}"),
            THIRTypeKind::Structured { fields } => {
                f.write_str("struct{")?;
                write_thir_fields(f, fields)?;
                f.write_str("}")
            }
            THIRTypeKind::Union { variants } => {
                f.write_str("union{")?;
                write_thir_fields(f, variants)?;
                f.write_str("}")
            }
            THIRTypeKind::TaggedUnion { variants } => {
                f.write_str("sum{")?;
                write_thir_fields(f, variants)?;
                f.write_str("}")
            }
            THIRTypeKind::PointerTo { inner_type } => write!(f, "ptr<t{}>", inner_type.0),
            THIRTypeKind::MemoryReference {
                inner_type,
                bitfield,
            } => {
                write!(f, "ref<t{}", inner_type.0)?;
                if let Some(bitfield) = bitfield {
                    write!(
                        f,
                        ", bitfield(t{}, {}..{}, signed={})",
                        bitfield.storage_type.0,
                        bitfield.bit_offset,
                        bitfield.bit_offset + bitfield.bit_width,
                        bitfield.signed
                    )?;
                }
                f.write_str(">")
            }
            THIRTypeKind::Array { length, inner_type } => {
                write!(f, "[t{}; {length}]", inner_type.0)
            }
            THIRTypeKind::Function { .. } => f.write_str("fn-type"),
            THIRTypeKind::Opaque { size, alignment } => {
                write!(f, "opaque(size={size}, align={alignment})")
            }
            THIRTypeKind::Undefined => f.write_str("undefined"),
            THIRTypeKind::Str => f.write_str("str"),
        }
    }
}

fn int_width(ty: THIRIntType) -> u16 {
    match ty {
        THIRIntType::I1 => 1,
        THIRIntType::I8 => 8,
        THIRIntType::I16 => 16,
        THIRIntType::I32 => 32,
        THIRIntType::I64 => 64,
        THIRIntType::I128 => 128,
    }
}

fn write_thir_fields(f: &mut Formatter<'_>, fields: &[THIRField]) -> fmt::Result {
    for (index, field) in fields.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        match field {
            THIRField::Standard { name, type_id } => write!(f, "{name}: t{}", type_id.0)?,
            THIRField::Bitfield {
                name,
                integer_type_id,
                width,
            } => write!(
                f,
                "{}: t{}:{width}",
                name.as_deref().unwrap_or("_"),
                integer_type_id.0
            )?,
        }
    }
    Ok(())
}

impl Display for MIRConstant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("()"),
            Self::Bool(value) => Display::fmt(value, f),
            Self::Integer { value, ty, signed } => write!(
                f,
                "{value}:{}{}",
                if *signed { "i" } else { "u" },
                int_width(*ty)
            ),
            Self::Float { value, ty } => write!(f, "{value}:{ty:?}"),
            Self::String(value) => write!(f, "{value:?}"),
            Self::Null => f.write_str("null"),
            Self::Undefined => f.write_str("undef"),
        }
    }
}

impl Display for MIROperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Place(value) => Display::fmt(value, f),
            Self::Parameter(index) => write!(f, "arg{index}"),
            Self::Register(value) => Display::fmt(value, f),
            Self::Constant(value) => Display::fmt(value, f),
            Self::Function(value) => write!(f, "fn {value}"),
            Self::Global(value) => write!(f, "global {value}"),
        }
    }
}

impl Display for MIRDestination {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Place(value) => Display::fmt(value, f),
            Self::Register(value) => Display::fmt(value, f),
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
            Self::LivenessStart(p) => write!(f, "storage.live {p}"),
            Self::LivenessEnd(p) => write!(f, "storage.dead {p}"),
            Self::LeakPlace(p) => write!(f, "storage.leak {p}"),
            Self::CreatePlace { out, ty } => write!(f, "create {out}: {ty}"),
            Self::CopyInto { dest, src, ty } => write!(f, "copy {src} -> {dest}: {ty}"),
            Self::MoveInto { dest, src, ty } => write!(f, "move {src} -> {dest}: {ty}"),
            Self::Assign { out, value } => write!(f, "{out} = {value}"),
            Self::Load { out, source } => write!(f, "{out} = load {source}"),
            Self::Store { dest, value } => write!(f, "store {value} -> {dest}"),
            Self::AddressOf { out, place } => write!(f, "{out} = address_of {place}"),
            Self::Dereference {
                out,
                pointer,
                pointee_type,
            } => write!(f, "{out} = deref {pointer}: {pointee_type}"),
            Self::MemberAccess {
                out,
                base,
                member_index,
                aggregate_type,
            } => write!(f, "{out} = member {base}.{member_index}: {aggregate_type}"),
            Self::ArrayAccess {
                out,
                base,
                index,
                element_type,
            } => write!(f, "{out} = index {base}[{index}]: {element_type}"),
            Self::SumTag {
                out,
                base,
                sum_type,
            } => write!(f, "{out} = sum.tag {base}: {sum_type}"),
            Self::SumVariant {
                out,
                base,
                variant_index,
                sum_type,
            } => write!(f, "{out} = sum.variant {base}.{variant_index}: {sum_type}"),
            Self::ConstructAggregate {
                out,
                kind,
                ty,
                fields,
            } => {
                write!(f, "{out} = {} {ty} ", aggregate_name(*kind))?;
                write_fields(f, fields)
            }
            Self::UpdateAggregate {
                out,
                base,
                ty,
                fields,
            } => {
                write!(f, "{out} = update {base}: {ty} ")?;
                write_fields(f, fields)
            }
            Self::ConstructSum {
                out,
                variant_index,
                value,
                sum_type,
            } => write!(f, "{out} = sum {sum_type} variant {variant_index}({value})"),
            Self::SetSumVariant {
                target,
                variant_index,
                value,
                sum_type,
            } => write!(
                f,
                "sum.set {target}: {sum_type} variant {variant_index}({value})"
            ),
            Self::DirectCall {
                out,
                function,
                args,
            } => {
                write_optional_destination(f, *out)?;
                write!(f, "call {function}(")?;
                write_operands(f, args)?;
                f.write_str(")")
            }
            Self::IndirectCall { out, callee, args } => {
                write_optional_destination(f, *out)?;
                write!(f, "call_indirect {callee}(")?;
                write_operands(f, args)?;
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
            Self::Phi { out, incoming } => {
                write!(f, "{out} = phi ")?;
                for (i, (b, v)) in incoming.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "[{b}: {v}]")?;
                }
                Ok(())
            }
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
                for (i, (case, target)) in cases.iter().enumerate() {
                    if i != 0 {
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
            Self::Unreachable => f.write_str("unreachable"),
        }
    }
}

fn aggregate_name(kind: MIRAggregateKind) -> &'static str {
    match kind {
        MIRAggregateKind::Array => "array",
        MIRAggregateKind::Struct => "struct",
    }
}
fn write_fields(f: &mut Formatter<'_>, fields: &[(usize, MIROperand)]) -> fmt::Result {
    f.write_str("{")?;
    for (i, (index, value)) in fields.iter().enumerate() {
        if i != 0 {
            f.write_str(", ")?;
        }
        write!(f, "{index}: {value}")?;
    }
    f.write_str("}")
}
fn write_operands(f: &mut Formatter<'_>, values: &[MIROperand]) -> fmt::Result {
    for (i, value) in values.iter().enumerate() {
        if i != 0 {
            f.write_str(", ")?;
        }
        Display::fmt(value, f)?;
    }
    Ok(())
}
fn write_optional_destination(f: &mut Formatter<'_>, out: Option<MIRDestination>) -> fmt::Result {
    if let Some(out) = out {
        write!(f, "{out} = ")?;
    }
    Ok(())
}

impl Display for MIRFnSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
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
        f.write_str(") -> ")?;
        match &self.return_type {
            Some(ty) => Display::fmt(ty, f),
            None => f.write_str("unit"),
        }
    }
}

impl Display for MIRGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "global {} {}: {} [{:?}]",
            self.id, self.name, self.ty, self.linkage
        )?;
        if self.is_definition {
            f.write_str(" = ")?;
            match &self.initializer {
                Some(v) => Display::fmt(v, f),
                None => f.write_str("zero"),
            }
        } else {
            f.write_str(" declaration")
        }
    }
}

impl Display for MIRBasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)?;
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
        f.write_str("mir v0 {\n")?;
        for global in &self.globals {
            writeln!(f, "  {global}")?;
        }
        for function in &self.functions {
            for line in function.to_string().lines() {
                writeln!(f, "  {line}")?;
            }
        }
        f.write_str("}\n")
    }
}
