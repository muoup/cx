use crate::types::{BCFloatType, BCIntegerType, BCTypeKind, BCType};
use crate::{
    MIRFloatBinOp, MIRFloatUnOp, MIRIntUnOp, MIRPtrBinOp, BlockID, MIRBlock, MIRFunction,
    MIRFunctionPrototype, BCGlobalType, BCInstruction, BCInstructionKind, MIRIntBinOp, MIRUnit,
    BCValue,
};
use std::fmt::{Display, Formatter};

impl Display for MIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Bytecode Program:")?;

        for global in self.global_vars.iter() {
            writeln!(f, "{} :: {}", global.name, global._type)?;
        }

        for func in self.fn_defs.iter() {
            writeln!(f, "{func}")?;
        }

        Ok(())
    }
}

impl Display for MIRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.prototype)?;

        for (i, block) in self.blocks.iter().enumerate() {
            write!(f, "block{i}")?;
            if !block.debug_name.is_empty() {
                write!(f, "  // {}", block.debug_name)?;
            }
            writeln!(f, ":")?;
            writeln!(f, "{block}")?;
        }

        for (i, block) in self.defer_blocks.iter().enumerate() {
            write!(f, "defer_block{i}")?;
            if !block.debug_name.is_empty() {
                write!(f, "  // {}", block.debug_name)?;
            }
            writeln!(f, ":")?;
            writeln!(f, "{block}")?;
        }

        Ok(())
    }
}

impl Display for MIRBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, instruction) in self.body.iter().enumerate() {
            write!(f, "\t")?;

            if !instruction.value_type.is_void() {
                write!(f, "_{i} = ")?;
            }

            writeln!(f, "{instruction}")?;
        }

        Ok(())
    }
}

impl Display for MIRFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;

        for (i, arg) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg._type)?;
        }

        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for BCInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)?;

        if !self.value_type.is_void() {
            write!(f, "\n\t\t ({})", self.value_type)?;
        }

        Ok(())
    }
}

impl Display for BCGlobalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCGlobalType::StringLiteral(s) => {
                write!(f, "string_literal \"{}\"", s.replace('\n', "\\n"))
            }
            BCGlobalType::Variable {
                _type,
                initial_value,
            } => {
                if let Some(initial_value) = initial_value {
                    write!(f, "variable {_type} = {initial_value}")
                } else {
                    write!(f, "variable {_type}")
                }
            }
        }
    }
}

impl Display for BCValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCValue::NULL => write!(f, "null"),
            BCValue::ParameterRef(index) => write!(f, "p{index}"),
            BCValue::IntImmediate { val, .. } => write!(f, "{val}"),
            BCValue::FloatImmediate { val, .. } => write!(f, "{val}"),
            BCValue::LoadOf(_, value) => write!(f, "*{value}"),
            BCValue::FunctionRef(name) => write!(f, "{name}"),
            BCValue::Global(id) => write!(f, "g{id}"),
            BCValue::BlockResult { block_id, value_id } => write!(f, "{block_id}:v{value_id}"),
        }
    }
}

impl Display for BlockID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockID::Block(id) => write!(f, "b{id}"),
            BlockID::DeferredBlock(id) => write!(f, "d{id}"),
        }
    }
}

impl Display for BCInstructionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCInstructionKind::Temp { value } => {
                write!(f, "{value}")
            }
            BCInstructionKind::Allocate { _type, .. } => {
                write!(f, "stackallocate {_type}")
            }
            BCInstructionKind::Store { value, memory, .. } => {
                write!(f, "*{memory} := {value}")
            }
            BCInstructionKind::ZeroMemory { memory, _type } => {
                write!(f, "*{memory} := 0")
            }
            BCInstructionKind::StructAccess {
                struct_,
                field_index,
                ..
            } => {
                write!(f, "{struct_}.[{field_index}]")
            }
            BCInstructionKind::BoolExtend { value } => {
                write!(f, "bool_extend {value}")
            }
            BCInstructionKind::ZExtend { value } => {
                write!(f, "zextend {value}")
            }
            BCInstructionKind::SExtend { value } => {
                write!(f, "sextend {value}")
            }
            BCInstructionKind::Trunc { value } => {
                write!(f, "trunc {value}")
            }
            BCInstructionKind::PtrToInt { value } => {
                write!(f, "ptr_to_int {value}")
            }
            BCInstructionKind::IntToPtrDiff { value, ptr_type } => {
                write!(f, "int_to_ptrdiff ({ptr_type}*) {value}")
            }
            BCInstructionKind::IntToPtr { value } => {
                write!(f, "int_to_ptr {value}")
            }
            BCInstructionKind::Return { value } => {
                write!(f, "return")?;

                if let Some(value) = value {
                    write!(f, " {value}")?;
                }

                Ok(())
            }
            BCInstructionKind::Branch {
                condition,
                true_block,
                false_block,
            } => {
                write!(f, "if {condition} goto {true_block} else {false_block}")
            }
            BCInstructionKind::Phi { predecessors: from } => {
                write!(f, "phi")?;
                if !from.is_empty() {
                    write!(f, " from [")?;
                    for (i, (value, block_id)) in from.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{value} @ b{block_id}")?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            BCInstructionKind::Jump { target } => {
                write!(f, "jump {target}")
            }
            BCInstructionKind::GotoDefer => {
                write!(f, "goto defer")
            }
            BCInstructionKind::JumpTable {
                value,
                targets,
                default,
            } => {
                write!(f, "jump_table {value} -> [")?;
                for (i, (key, block_id)) in targets.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key} -> {block_id}")?;
                }
                write!(f, "] else {default}")
            }
            BCInstructionKind::DirectCall {
                method_sig, args, ..
            } => {
                write!(f, "@{}(", method_sig.name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            BCInstructionKind::IndirectCall { func_ptr, args, .. } => {
                write!(f, "@(*{func_ptr})(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            BCInstructionKind::PointerBinOp {
                left,
                ptr_type,
                right,
                op,
            } => {
                write!(f, "{left} {op} {right} [{ptr_type}*]")
            }
            BCInstructionKind::IntegerBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [i]")
            }
            BCInstructionKind::IntegerUnOp { op, value } => {
                write!(f, "{op:?} {value} [i]")
            }
            BCInstructionKind::FloatBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [f]")
            }
            BCInstructionKind::FloatUnOp { op, value } => {
                write!(f, "{op:?} {value} [f]")
            }
            BCInstructionKind::GetFunctionAddr { func: func_name } => {
                write!(f, "get_function_addr {func_name}")
            }
            BCInstructionKind::BitCast { value } => {
                write!(f, "bit_cast {value}")
            }
            BCInstructionKind::IntToFloat { from, value } => {
                write!(f, "int_to_float ({from}) {value}")
            }
            BCInstructionKind::FloatToInt { from, value } => {
                write!(f, "float_to_int ({from}) {value}")
            }
            BCInstructionKind::FloatCast { value } => {
                write!(f, "float_cast {value}")
            }
            BCInstructionKind::CompilerAssertion { condition, message } => {
                write!(f, "compiler_assertion {condition} ({message})")
            }
            BCInstructionKind::NOP => {
                write!(f, "nop")
            }
        }
    }
}

impl Display for MIRPtrBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRPtrBinOp::ADD => "+",
                MIRPtrBinOp::SUB => "-",

                MIRPtrBinOp::EQ => "==",
                MIRPtrBinOp::NE => "!=",

                MIRPtrBinOp::LT => "<",
                MIRPtrBinOp::GT => ">",
                MIRPtrBinOp::LE => "<=",
                MIRPtrBinOp::GE => ">=",
            },
        )
    }
}

impl Display for MIRIntBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRIntBinOp::ADD => "+",
                MIRIntBinOp::SUB => "-",
                MIRIntBinOp::MUL => "*",
                MIRIntBinOp::IDIV => "i/",
                MIRIntBinOp::IREM => "i%",

                MIRIntBinOp::UDIV => "u/",
                MIRIntBinOp::UREM => "u%",

                MIRIntBinOp::SHL => "<<",
                MIRIntBinOp::ASHR => "a>>",
                MIRIntBinOp::LSHR => "l>>",

                MIRIntBinOp::BAND => "&",
                MIRIntBinOp::BOR => "|",
                MIRIntBinOp::BXOR => "^",

                MIRIntBinOp::LAND => "&&",
                MIRIntBinOp::LOR => "||",

                MIRIntBinOp::EQ => "==",
                MIRIntBinOp::NE => "!=",

                MIRIntBinOp::ILT => "<",
                MIRIntBinOp::IGT => ">",
                MIRIntBinOp::ILE => "<=",
                MIRIntBinOp::IGE => ">=",

                MIRIntBinOp::ULT => "(u) <",
                MIRIntBinOp::UGT => "(u) >",
                MIRIntBinOp::ULE => "(u) <=",
                MIRIntBinOp::UGE => "(u) >=",
            },
        )
    }
}

impl Display for MIRIntUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRIntUnOp::BNOT => "~",
                MIRIntUnOp::LNOT => "!",
                MIRIntUnOp::NEG => "-",
            },
        )
    }
}

impl Display for MIRFloatBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRFloatBinOp::ADD => "+",
                MIRFloatBinOp::SUB => "-",
                MIRFloatBinOp::FMUL => "*",
                MIRFloatBinOp::FDIV => "/",
            },
        )
    }
}

impl Display for MIRFloatUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRFloatUnOp::NEG => "-",
            },
        )
    }
}

impl Display for BCType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for BCIntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            BCIntegerType::I8 => write!(f, "i8"),
            BCIntegerType::I16 => write!(f, "i16"),
            BCIntegerType::I32 => write!(f, "i32"),
            BCIntegerType::I64 => write!(f, "i64"),
            BCIntegerType::I128 => write!(f, "i128"),
        }
    }
}

impl Display for BCFloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            BCFloatType::F32 => write!(f, "f32"),
            BCFloatType::F64 => write!(f, "f64"),
        }
    }
}

impl Display for BCTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            BCTypeKind::Opaque { bytes } => write!(f, "opaque_{}", *bytes),
            BCTypeKind::Bool => write!(f, "bool"),
                       
            BCTypeKind::Integer(_type) => write!(f, "{}", _type),
            BCTypeKind::Float(_type) => write!(f, "{}", _type),

            BCTypeKind::Pointer {
                nullable,
                dereferenceable,
            } => {
                if !*nullable {
                    write!(f, "!")?;
                }

                write!(f, "*")?;

                if *dereferenceable > 0 {
                    write!(f, " (deref: {dereferenceable})")
                } else {
                    Ok(())
                }
            }

            BCTypeKind::Array { element, size } => {
                write!(f, "[{element}; {size}]")
            }
            BCTypeKind::Struct { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{name}: {_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "struct {{ {fields} }}")
            }
            BCTypeKind::Union { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{name}: {_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "union {{ {fields} }}")
            }

            BCTypeKind::Unit => write!(f, "()"),
            BCTypeKind::VariableSized { size, alignment } => {
                write!(f, "variable_sized (size: {size}, alignment: {alignment})")
            }
        }
    }
}
