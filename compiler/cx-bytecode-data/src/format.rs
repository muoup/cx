use crate::bc_type::{BCFunctionPrototype, BCType, FloatType, IntegerType};
use crate::compilation_unit::{BCBlock, BCFunction, BCUnit};
use crate::instruction::{
    BCAddress, BCFloatBinOp, BCFloatUnOp, BCInstruction, BCIntegerBinOp, BCIntegerUnOp, BCValue,
};
use std::fmt::{Display, Formatter};

impl Display for BCUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Bytecode Unit:")?;

        for func in self.function_prototypes.iter() {
            writeln!(f, "extern {};", func)?;
        }

        Ok(())
    }
}

impl Display for BCFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn (")?;
        for (i, (p_name, p_type)) in self
            .params
            .iter()
            .zip(self.prototype.parameter_types.iter())
            .enumerate()
        {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{p_name}: {p_type}")?;
        }
        writeln!(f, ") -> {}:", self.prototype.return_type)?;

        for block in &self.blocks {
            writeln!(f, "{block}")?;
        }

        Ok(())
    }
}

impl Display for BCBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for instruction in &self.instructions {
            writeln!(f, "\t{instruction}")?;
        }
        Ok(())
    }
}

impl Display for BCFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;

        for (i, arg) in self.parameter_types.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }

        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for BCInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCInstruction::Allocate {
                result: value,
                type_,
                alignment,
            } => {
                write!(f, "{value} = allocate {type_} align {alignment}")
            }
            BCInstruction::Store {
                value,
                destination,
                store_type,
            } => {
                write!(f, "store {store_type} {destination}, {value}")
            }
            BCInstruction::Load {
                result: destination,
                source,
                load_type,
            } => {
                write!(f, "{destination} = load {load_type} {source}")
            }
            BCInstruction::Return { value } => {
                write!(f, "return")?;

                if let Some(value) = value {
                    write!(f, " {value}")?;
                }

                Ok(())
            }
            BCInstruction::IntBinOp {
                result: destination,
                left,
                right,
                op,
            } => {
                write!(f, "{destination} = {left} {op} {right}")
            }
            BCInstruction::FloatBinOp {
                result: destination,
                left,
                right,
                op,
            } => {
                write!(f, "{destination} = {left} {op} {right}")
            }
            BCInstruction::IntUnOp {
                result: destination,
                value,
                op,
            } => {
                write!(f, "{destination} = {op} {value}")
            }
            BCInstruction::FloatUnOp {
                result: destination,
                value,
                op,
            } => {
                write!(f, "{destination} = {op} {value}")
            }
            BCInstruction::PointerBinOp {
                result: destination,
                ptr_type,
                left,
                right,
                op,
            } => {
                write!(f, "{destination} = ({ptr_type}) {left} {op} {right}")
            }
            BCInstruction::CallDirect {
                result: destination,
                function,
                arguments,
            } => {
                if let Some(destination) = destination {
                    write!(f, "{destination} = ")?;
                }
                write!(f, "call ({}) @{}(", function, function.name)?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            BCInstruction::CallIndirect {
                result: destination,
                prototype,
                function_pointer,
                arguments,
            } => {
                if let Some(destination) = destination {
                    write!(f, "{destination} = ")?;
                }
                write!(f, "call ({prototype}) *{function_pointer}(")?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            BCInstruction::Branch {
                condition,
                true_target,
                false_target,
            } => {
                write!(f, "branch {condition} ? {true_target} : {false_target}")
            }
            BCInstruction::Jump { target } => write!(f, "jump {target}"),

            BCInstruction::GetElementPtr {
                result: destination,
                base,
                index,
                offset,
                structure_type,
            } => write!(
                f,
                "{destination} = getelementptr {structure_type} {base}, {index}, {offset}"
            ),

            BCInstruction::GetFunctionPtr { result, function } => {
                write!(f, "{result} = getfuncptr @{}", function)
            }

            BCInstruction::ValueCoercion {
                result: destination,
                value,
                coercion,
            } => write!(f, "{destination} = coerce {value} ({coercion:?})"),

            BCInstruction::Memset {
                result: destination,
                value,
                _type,
            } => write!(f, "memset {destination}, {value}, {_type}"),

            BCInstruction::Phi {
                result: destination,
                predecessors: sources,
            } => {
                write!(f, "{destination} = phi {{ ")?;
                for (i, (block, value)) in sources.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{block}: {value}")?;
                }
                write!(f, " }}")
            }

            BCInstruction::JumpTable {
                index,
                targets,
                default_target,
            } => {
                write!(f, "jumptable {index} [ ")?;
                for (i, target) in targets.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{target}")?;
                }
                write!(f, " ] default {default_target}")
            }

            BCInstruction::Alias {
                result: destination,
                source,
            } => write!(f, "{destination} = {source}"),
        }
    }
}

impl Display for BCAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCAddress::Local(ident) => write!(f, "%{}", ident),
            BCAddress::Global(ident) => write!(f, "@{}", ident),
        }
    }
}

impl Display for BCValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCValue::Integer { value, .. } => write!(f, "{value}"),
            BCValue::Float { value, .. } => write!(f, "{value}"),
            BCValue::Address(address) => write!(f, "{address}"),
        }
    }
}

impl Display for BCIntegerBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BCIntegerBinOp::ADD => "+",
                BCIntegerBinOp::SUB => "-",
                BCIntegerBinOp::MUL => "*",
                BCIntegerBinOp::DIV => "/",
                BCIntegerBinOp::IMUL => "i*",
                BCIntegerBinOp::IDIV => "i/",
                BCIntegerBinOp::EQ => "==",
                BCIntegerBinOp::NE => "!=",
                BCIntegerBinOp::LT => "<",
                BCIntegerBinOp::GT => ">",
                BCIntegerBinOp::LE => "<=",
                BCIntegerBinOp::GE => ">=",
                BCIntegerBinOp::ULT => "(u)<",
                BCIntegerBinOp::UGT => "(u)>",
                BCIntegerBinOp::ULE => "(u)<=",
                BCIntegerBinOp::UGE => "(u)>=",
                BCIntegerBinOp::BAND => "&",
                BCIntegerBinOp::BOR => "|",
                BCIntegerBinOp::BXOR => "^",
            }
        )
    }
}

impl Display for BCFloatBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BCFloatBinOp::ADD => "+.",
                BCFloatBinOp::SUB => "-.",
                BCFloatBinOp::MUL => "*.",
                BCFloatBinOp::DIV => "/.",
                BCFloatBinOp::EQ => "==.",
                BCFloatBinOp::NE => "!=.",
                BCFloatBinOp::LT => "<.",
                BCFloatBinOp::GT => ">.",
                BCFloatBinOp::LE => "<=.",
                BCFloatBinOp::GE => ">=.",
            }
        )
    }
}

impl Display for BCIntegerUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BCIntegerUnOp::NEG => "-",
                BCIntegerUnOp::BNOT => "~",
            }
        )
    }
}

impl Display for BCFloatUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BCFloatUnOp::NEG => "-",
            }
        )
    }
}

impl Display for BCType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCType::Pointer {
                dereferenceable,
                nonnull: nullable,
            } => {
                write!(f, "ptr")?;
                if *nullable {
                    write!(f, " nonnull")?;
                }
                if *dereferenceable > 0 {
                    write!(f, " (deref: {dereferenceable})")?;
                }
                Ok(())
            }
            BCType::Structured { name, fields } => {
                write!(f, "struct {name} {{ ")?;
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{field_name}: {field_type}")?;
                }
                write!(f, " }}")
            }
            BCType::Array { element, size } => {
                write!(f, "[{element}; {size}]")
            }
            BCType::Integer(it) => write!(f, "{it}"),
            BCType::Float(ft) => write!(f, "{ft}"),
            BCType::Opaque { size } => write!(f, "opaque<{size}>"),
            BCType::Unit => write!(f, "()"),
            BCType::Bool => write!(f, "bool"),
        }
    }
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntegerType::I8 => "i8",
                IntegerType::I16 => "i16",
                IntegerType::I32 => "i32",
                IntegerType::I64 => "i64",
                IntegerType::I128 => "i128",
            }
        )
    }
}

impl Display for FloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                FloatType::F32 => "f32",
                FloatType::F64 => "f64",
            }
        )
    }
}
