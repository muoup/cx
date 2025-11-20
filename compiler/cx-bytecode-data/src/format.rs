use crate::types::{MIRType, MIRTypeKind};
use crate::{
    MIRFloatBinOp, MIRFloatUnOp, MIRIntUnOp, MIRPtrBinOp, BlockID, MIRBlock, MIRFunction,
    MIRFunctionPrototype, BCGlobalType, MIRInstruction, MIRInstructionKind, MIRIntBinOp, MIRUnit,
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

impl Display for MIRInstruction {
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

impl Display for MIRInstructionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRInstructionKind::Temp { value } => {
                write!(f, "{value}")
            }
            MIRInstructionKind::Allocate { _type, .. } => {
                write!(f, "stackallocate {_type}")
            }
            MIRInstructionKind::Store { value, memory, .. } => {
                write!(f, "*{memory} := {value}")
            }
            MIRInstructionKind::ZeroMemory { memory, _type } => {
                write!(f, "*{memory} := 0")
            }
            MIRInstructionKind::StructAccess {
                struct_,
                field_index,
                ..
            } => {
                write!(f, "{struct_}.[{field_index}]")
            }
            MIRInstructionKind::BoolExtend { value } => {
                write!(f, "bool_extend {value}")
            }
            MIRInstructionKind::ZExtend { value } => {
                write!(f, "zextend {value}")
            }
            MIRInstructionKind::SExtend { value } => {
                write!(f, "sextend {value}")
            }
            MIRInstructionKind::Trunc { value } => {
                write!(f, "trunc {value}")
            }
            MIRInstructionKind::PtrToInt { value } => {
                write!(f, "ptr_to_int {value}")
            }
            MIRInstructionKind::IntToPtrDiff { value, ptr_type } => {
                write!(f, "int_to_ptrdiff ({ptr_type}*) {value}")
            }
            MIRInstructionKind::IntToPtr { value } => {
                write!(f, "int_to_ptr {value}")
            }
            MIRInstructionKind::Return { value } => {
                write!(f, "return")?;

                if let Some(value) = value {
                    write!(f, " {value}")?;
                }

                Ok(())
            }
            MIRInstructionKind::Branch {
                condition,
                true_block,
                false_block,
            } => {
                write!(f, "if {condition} goto {true_block} else {false_block}")
            }
            MIRInstructionKind::Phi { predecessors: from } => {
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
            MIRInstructionKind::Jump { target } => {
                write!(f, "jump {target}")
            }
            MIRInstructionKind::GotoDefer => {
                write!(f, "goto defer")
            }
            MIRInstructionKind::JumpTable {
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
            MIRInstructionKind::DirectCall {
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
            MIRInstructionKind::IndirectCall { func_ptr, args, .. } => {
                write!(f, "@(*{func_ptr})(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            MIRInstructionKind::PointerBinOp {
                left,
                ptr_type,
                right,
                op,
            } => {
                write!(f, "{left} {op} {right} [{ptr_type}*]")
            }
            MIRInstructionKind::IntegerBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [i]")
            }
            MIRInstructionKind::IntegerUnOp { op, value } => {
                write!(f, "{op:?} {value} [i]")
            }
            MIRInstructionKind::FloatBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [f]")
            }
            MIRInstructionKind::FloatUnOp { op, value } => {
                write!(f, "{op:?} {value} [f]")
            }
            MIRInstructionKind::GetFunctionAddr { func: func_name } => {
                write!(f, "get_function_addr {func_name}")
            }
            MIRInstructionKind::BitCast { value } => {
                write!(f, "bit_cast {value}")
            }
            MIRInstructionKind::IntToFloat { from, value } => {
                write!(f, "int_to_float ({from}) {value}")
            }
            MIRInstructionKind::FloatToInt { from, value } => {
                write!(f, "float_to_int ({from}) {value}")
            }
            MIRInstructionKind::FloatCast { value } => {
                write!(f, "float_cast {value}")
            }
            MIRInstructionKind::CompilerAssertion { condition, message } => {
                write!(f, "compiler_assertion {condition} ({message})")
            }
            MIRInstructionKind::NOP => {
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

impl Display for MIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for MIRTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            MIRTypeKind::Opaque { bytes } => write!(f, "opaque_{}", *bytes),
            MIRTypeKind::Signed { bytes } => write!(f, "i{}", bytes * 8),
            MIRTypeKind::Unsigned { bytes } => write!(f, "u{}", bytes * 8),
            MIRTypeKind::Bool => write!(f, "bool"),
            MIRTypeKind::Float { bytes } => write!(f, "f{}", bytes * 8),

            MIRTypeKind::Pointer {
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

            MIRTypeKind::Array { element, size } => {
                write!(f, "[{element}; {size}]")
            }
            MIRTypeKind::Struct { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{name}: {_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "struct {{ {fields} }}")
            }
            MIRTypeKind::Union { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{name}: {_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "union {{ {fields} }}")
            }

            MIRTypeKind::Unit => write!(f, "()"),
            MIRTypeKind::VariableSized { size, alignment } => {
                write!(f, "variable_sized (size: {size}, alignment: {alignment})")
            }
        }
    }
}
