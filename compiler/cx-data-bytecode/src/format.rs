use std::fmt::{Display, Formatter};
use crate::{BCFloatBinOp, BCFloatUnOp, BCIntBinOp, BCIntUnOp, BlockInstruction, BytecodeFunction, BCFunctionPrototype, FunctionBlock, ProgramBytecode, MIRValue, VirtualInstruction, VirtualValue, BCPtrBinOp, BlockID};
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

impl Display for FunctionBlock {
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

impl Display for BCFunctionPrototype {
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

impl Display for BlockInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instruction)?;

        if !self.value_type.is_void() {
            write!(f, "\n\t\t ({})", self.value_type)?;
        }

        Ok(())
    }
}

impl Display for VirtualValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_)
    }
}

impl Display for MIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRValue::NULL => write!(f, "null"),
            MIRValue::IntImmediate { val, .. } => write!(f, "{val}"),
            MIRValue::FloatImmediate { val, ..} => {
                let float = f64::from_bits(*val as u64);
                write!(f, "{float}")
            },
            MIRValue::FunctionRef(name) => write!(f, "{name}"),
            MIRValue::Global(id) => write!(f, "g{}", id),
            MIRValue::BlockResult { block_id, value_id } => write!(f, "{}:v{}", block_id, value_id),
        }
    }
}

impl Display for BlockID {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "b{}{}", self.id, if self.in_deferral { "*" } else { "" })
    }
}

impl Display for VirtualInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VirtualInstruction::Allocate { _type, alignment } => {
                write!(f, "alloca {_type} (alignment: {alignment})")
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
            VirtualInstruction::ZeroMemory { memory, _type } => {
                write!(f, "zero_memory {memory} ({_type})")
            },
            VirtualInstruction::StructAccess { struct_, struct_type, field_index, field_offset, .. } => {
                write!(f, "access {struct_} at index {field_index}; offset: {field_offset}")
            },
            VirtualInstruction::BoolExtend { value } => {
                write!(f, "bool_extend {value}")
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
            VirtualInstruction::PtrToInt { value } => {
                write!(f, "ptr_to_int {value}")
            },
            VirtualInstruction::IntToPtrDiff { value, ptr_type } => {
                write!(f, "int_to_ptrdiff ({ptr_type}*) {value}")
            },
            VirtualInstruction::IntToPtr { value } => {
                write!(f, "int_to_ptr {value}")
            },
            VirtualInstruction::Return { value } => {
                write!(f, "return")?;

                if let Some(value) = value {
                    write!(f, " {value}")?;
                }

                Ok(())
            },
            VirtualInstruction::Branch { condition, true_block, false_block } => {
                write!(f, "branch on {condition}; true -> {true_block}, false -> {false_block}")
            },
            VirtualInstruction::Phi { predecessors: from } => {
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
            },
            VirtualInstruction::Jump { target } => {
                write!(f, "jump {target}")
            },
            VirtualInstruction::GotoDefer => {
                write!(f, "goto defer")
            }
            VirtualInstruction::JumpTable { value, targets, default } => {
                write!(f, "jump_table {value} -> [")?;
                for (i, (key, block_id)) in targets.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key} -> {block_id}")?;
                }
                write!(f, "] else {default}")
            }
            VirtualInstruction::DirectCall { method_sig, args, .. } => {
                write!(f, "@{}(", method_sig.name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            },
            VirtualInstruction::IndirectCall { func_ptr, args, .. } => {
                write!(f, "@(*{func_ptr})(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            },
            VirtualInstruction::PointerBinOp { left, ptr_type, right, op } => {
                write!(f, "{left} {op} {right} [{ptr_type}*]")
            },
            VirtualInstruction::IntegerBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [i]")
            },
            VirtualInstruction::IntegerUnOp { op, value } => {
                write!(f, "{op:?} {value} [i]")
            },
            VirtualInstruction::FloatBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [f]")
            },
            VirtualInstruction::FloatUnOp { op, value } => {
                write!(f, "{op:?} {value} [f]")
            },
            VirtualInstruction::StringLiteral { str_id } => {
                write!(f, "string_literal {str_id}")
            },
            VirtualInstruction::GetFunctionAddr { func: func_name } => {
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

impl Display for BCPtrBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self {
                BCPtrBinOp::ADD => "+",
                BCPtrBinOp::SUB => "-",
                
                BCPtrBinOp::EQ => "==",
                BCPtrBinOp::NE => "!=",
                
                BCPtrBinOp::LT => "<",
                BCPtrBinOp::GT => ">",
                BCPtrBinOp::LE => "<=",
                BCPtrBinOp::GE => ">=",
            },
        )
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
            BCTypeKind::Signed { bytes } => write!(f, "i{}", bytes * 8),
            BCTypeKind::Unsigned { bytes } => write!(f, "u{}", bytes * 8),
            BCTypeKind::Bool => write!(f, "bool"),
            BCTypeKind::Float { bytes } => write!(f, "f{}", bytes * 8),
            
            BCTypeKind::Pointer { nullable, dereferenceable } => {
                if !*nullable {
                    write!(f, "!")?;
                }
                
                write!(f, "*")?;
                
                if *dereferenceable > 0 {
                    write!(f, " (deref: {dereferenceable})")
                } else {
                    Ok(())
                }
            },
            
            BCTypeKind::Array { element, size } => {
                write!(f, "[{element}; {size}]")
            },
            BCTypeKind::Struct { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{name}: {_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "struct {{ {fields} }}")
            },
            BCTypeKind::Union { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(name, _type)| format!("{name}: {_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "union {{ {fields} }}")
            },

            BCTypeKind::Unit => write!(f, "()"),
            BCTypeKind::VariableSized { size, alignment } => {
                write!(f, "variable_sized (size: {size}, alignment: {alignment})")
            },
        }
    }
}