use std::fmt::{Display, Formatter};
use crate::{BCFloatBinOp, BCFloatUnOp, BCIntBinOp, BCIntUnOp, BlockInstruction, BytecodeFunction, BCFunctionPrototype, FunctionBlock, ProgramBytecode, ValueID, VirtualInstruction, VirtualValue};
use crate::types::{BCType, BCTypeKind};

impl Display for ProgramBytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Bytecode Program:")?;

        for func in self.fn_defs.iter() {
            writeln!(f, "{func}")?;
        }

        Ok(())
    }
}

impl Display for BytecodeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.prototype)?;

        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "block{}:", i)?;
            writeln!(f, "{}", block)?;
        }

        Ok(())
    }
}

impl Display for FunctionBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, instruction) in self.body.iter().enumerate() {
            writeln!(f, "    v{i} = {}", instruction)?;
        }

        Ok(())
    }
}

impl Display for BCFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;

        for (i, arg) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg.type_)?;
        }

        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for BlockInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.instruction, self.value)
    }
}

impl Display for VirtualValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)
    }
}

impl Display for ValueID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}@b{}", self.value_id, self.block_id)
    }
}

impl Display for VirtualInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VirtualInstruction::Allocate { size } => {
                write!(f, "alloca {size}")
            },
            VirtualInstruction::AddressOf { value } => {
                write!(f, "address_of {value}")
            },
            VirtualInstruction::FunctionParameter { param_index } => {
                write!(f, "parameter {param_index}")
            },
            VirtualInstruction::Load { value } => {
                write!(f, "load {value}")
            },
            VirtualInstruction::Store { value, memory, type_ } => {
                write!(f, "store {type_} {value} -> {memory}")
            },
            VirtualInstruction::Immediate { value } => {
                write!(f, "immediate {}", value)
            },
            VirtualInstruction::StructAccess { struct_, field_index, field_offset } => {
                write!(f, "struct_access {}[{}] + {}", struct_, field_index, field_offset)
            },
            VirtualInstruction::Assign { target, value } => {
                write!(f, "assign {target} <- {value}")
            },
            VirtualInstruction::ZExtend { value } => {
                write!(f, "zextend {value}")
            },
            VirtualInstruction::SExtend { value } => {
                write!(f, "sextend {value}")
            },
            VirtualInstruction::Trunc { value } => {
                write!(f, "trunc {value}")
            },
            VirtualInstruction::Return { value } => {
                write!(f, "return")?;

                if let Some(value) = value {
                    write!(f, " {value}")?;
                }

                Ok(())
            },
            VirtualInstruction::Branch { condition, true_block, false_block } => {
                write!(f, "branch on {condition}; true = {true_block}, false = {false_block}")
            },
            VirtualInstruction::Jump { target } => {
                write!(f, "jump {target}")
            },
            VirtualInstruction::DirectCall { func, args, .. } => {
                write!(f, "direct_call {func}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            },
            VirtualInstruction::IndirectCall { func_ptr, args, .. } => {
                write!(f, "indirect_call {func_ptr}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            },
            VirtualInstruction::IntegerBinOp { left, right, op } => {
                write!(f, "int_binop {op} {left} {right}")
            },
            VirtualInstruction::IntegerUnOp { op, value } => {
                write!(f, "int_unop {op:?} {value}")
            },
            VirtualInstruction::FloatBinOp { left, right, op } => {
                write!(f, "float_binop {op} {left} {right}")
            },
            VirtualInstruction::FloatUnOp { op, value } => {
                write!(f, "float_unop {op:?} {value}")
            },
            VirtualInstruction::Literal { val } => {
                write!(f, "literal {val}")
            },
            VirtualInstruction::StringLiteral { str_id } => {
                write!(f, "string_literal {str_id}")
            },
            VirtualInstruction::FunctionReference { name } => {
                write!(f, "function_reference {name}")
            },
            VirtualInstruction::GetFunctionAddr { func_name } => {
                write!(f, "get_function_addr {func_name}")
            },
            VirtualInstruction::BitCast { value } => {
                write!(f, "bit_cast {value}")
            },
            VirtualInstruction::IntToFloat { from, value } => {
                write!(f, "int_to_float ({from}) {value}")
            },
            VirtualInstruction::FloatToInt { from, value } => {
                write!(f, "float_to_int ({from}) {value}")
            },
            VirtualInstruction::FloatCast { value } => {
                write!(f, "float_cast {value}")
            },
            VirtualInstruction::NOP => {
                write!(f, "nop")
            }
        }
    }
}

impl Display for BCIntBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
           match self {
                BCIntBinOp::ADD => "+",
                BCIntBinOp::SUB => "-",
                BCIntBinOp::MUL => "*",
                BCIntBinOp::IDIV => "i/",
                BCIntBinOp::IREM => "i%",

                BCIntBinOp::UDIV => "u/",
                BCIntBinOp::UREM => "u%",

                BCIntBinOp::SHL => "<<",
                BCIntBinOp::ASHR => "a>>",
                BCIntBinOp::LSHR => "l>>",

                BCIntBinOp::BAND => "&",
                BCIntBinOp::BOR => "|",
                BCIntBinOp::BXOR => "^",

                BCIntBinOp::LAND => "&&",
                BCIntBinOp::LOR => "||",
               
                BCIntBinOp::EQ => "==",
                BCIntBinOp::NE => "!=",
               
                BCIntBinOp::ILT => "i<",
                BCIntBinOp::IGT => "i>",
                BCIntBinOp::ILE => "i<=",
                BCIntBinOp::IGE => "i>=",
               
                BCIntBinOp::ULT => "u<",
                BCIntBinOp::UGT => "u>",
                BCIntBinOp::ULE => "u<=",
                BCIntBinOp::UGE => "u>=",
           },
        )
    }
}

impl Display for BCIntUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                BCIntUnOp::BNOT => "~",
                BCIntUnOp::LNOT => "!",
                BCIntUnOp::NEG => "-",
            },
        )
    }
}

impl Display for BCFloatBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                BCFloatBinOp::ADD => "+",
                BCFloatBinOp::SUB => "-",
                BCFloatBinOp::FMUL => "*",
                BCFloatBinOp::FDIV => "/",
            },
        )
    }
}

impl Display for BCFloatUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                BCFloatUnOp::NEG => "-",
            },
        )
    }
}

impl Display for BCType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for BCTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            BCTypeKind::Opaque { bytes } => write!(f, "opaque_{}", *bytes),
            BCTypeKind::Signed { bytes } => write!(f, "i{bytes}"),
            BCTypeKind::Unsigned { bytes } => write!(f, "u{bytes}"),
            BCTypeKind::Float { bytes } => write!(f, "f{bytes}"),
            BCTypeKind::Pointer => write!(f, "*"),

            BCTypeKind::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{}: {}", name, _type))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "struct {{ {} }}", fields)
            },
            BCTypeKind::Array { size, _type } => {
                write!(f, "[{}; {}]", _type, size)
            },

            BCTypeKind::Unit => write!(f, "()"),
        }
    }
}