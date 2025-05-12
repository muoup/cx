use std::fmt::{Display, Formatter};
use crate::builder::{BlockInstruction, BytecodeFunction, BytecodeFunctionPrototype, FunctionBlock, ValueID, VirtualInstruction, VirtualValue};
use crate::ProgramBytecode;

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
        write!(f, "{}:", self.prototype)?;

        for block in self.blocks.iter() {
            writeln!(f, "{}", block)?;
        }

        Ok(())
    }
}

impl Display for FunctionBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "block:")?;

        for instruction in self.body.iter() {
            writeln!(f, "    {}", instruction)?;
        }

        Ok(())
    }
}

impl Display for BytecodeFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;

        for (i, arg) in self.args.iter().enumerate() {
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
            VirtualInstruction::FloatBinOp { left, right, op } => {
                write!(f, "float_binop {op} {left} {right}")
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
            VirtualInstruction::NOP => {
                write!(f, "nop")
            }
        }
    }
}