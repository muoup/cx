use crate::types::{BCFloatType, BCIntegerType, BCType, BCTypeKind};
use crate::{
    BCFloatBinOp, BCFloatUnOp, BCGlobalType, BCInstruction, BCInstructionKind, BCIntBinOp, BCIntUnOp, BCPtrBinOp, BCRegister, BCValue, BCBasicBlock, BCFunction, BCFunctionPrototype, BCUnit
};
use std::fmt::{Display, Formatter};

impl Display for BCUnit {
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

impl Display for BCFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.prototype)?;

        for block in self.blocks.iter() {
            writeln!(f, "{block}")?;
        }

        Ok(())
    }
}

impl Display for BCBasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ".{}:", self.id)?;

        for instruction in self.body.iter() {
            writeln!(f, "\t{instruction}")?;
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

impl Display for BCInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(result) = &self.result {
            write!(f, "{} = ", result)?;
        }
        
        write!(f, "{}", self.kind)
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

impl Display for BCRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.name)
    }
}

impl Display for BCValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCValue::NULL => write!(f, "null"),
            BCValue::ParameterRef(index) => write!(f, "param @{index}"),
            BCValue::IntImmediate { val, _type } => write!(f, "{_type} {val}"),
            BCValue::FloatImmediate { val, _type } => write!(f, "{_type} {val}"),
            BCValue::FunctionRef(name) => write!(f, "{name}"),
            BCValue::Global(id) => write!(f, "g{id}"),
            BCValue::Register { register, _type } => write!(f, "{_type} {register}"),
        }
    }
}

impl Display for BCInstructionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BCInstructionKind::Alias { value } => {
                write!(f, "{value}")
            }
            BCInstructionKind::Allocate { _type, .. } => {
                write!(f, "alloca {_type}")
            }
            BCInstructionKind::Store { value, memory, .. } => {
                write!(f, "store {value} in {memory}")
            }
            BCInstructionKind::Load { memory, _type, .. } => {
                write!(f, "load {_type} {memory}")
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
            BCInstructionKind::IntToPtrDiff { value, ptr_inner } => {
                write!(f, "int_to_ptr_diff {value} ({ptr_inner})")
            },
            BCInstructionKind::Coerce { value, coercion_type } => {
                write!(f, "coerce {value} ({coercion_type:?})")
            },
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
                    write!(f, " [")?;
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
            BCInstructionKind::CompilerAssertion { condition, message } => {
                write!(f, "compiler_assertion {condition} ({message})")
            }
            BCInstructionKind::CompilerAssumption { condition } => {
                write!(f, "compiler_assumption {condition}")
            }
            BCInstructionKind::NOP => {
                write!(f, "nop")
            }
        }
    }
}

impl Display for BCPtrBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
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
        write!(
            f,
            "{}",
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

                BCIntBinOp::ILT => "<",
                BCIntBinOp::IGT => ">",
                BCIntBinOp::ILE => "<=",
                BCIntBinOp::IGE => ">=",

                BCIntBinOp::ULT => "(u) <",
                BCIntBinOp::UGT => "(u) >",
                BCIntBinOp::ULE => "(u) <=",
                BCIntBinOp::UGE => "(u) >=",
            },
        )
    }
}

impl Display for BCIntUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
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
        write!(
            f,
            "{}",
            match self {
                BCFloatBinOp::ADD => "+",
                BCFloatBinOp::SUB => "-",
                BCFloatBinOp::FMUL => "*",
                BCFloatBinOp::FDIV => "/",
                
                BCFloatBinOp::EQ => "==",
                BCFloatBinOp::NEQ => "!=",
                BCFloatBinOp::FLT => "<",
                BCFloatBinOp::FLE => "<=",
                BCFloatBinOp::FGT => ">",
                BCFloatBinOp::FGE => ">=",
            },
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
        }
    }
}
