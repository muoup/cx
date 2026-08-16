use crate::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use crate::{
    LMIRBasicBlock, LMIRFloatBinOp, LMIRFloatUnOp, LMIRFunction, LMIRFunctionPrototype, LMIRGlobalType,
    LMIRInstruction, LMIRInstructionKind, LMIRIntBinOp, LMIRIntUnOp, LMIRPtrBinOp, LMIRRegister, LMIRUnit,
    LMIRValue,
};
use std::fmt::{Display, Formatter};

impl Display for LMIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "LMIR Program:")?;

        for global in self.global_vars.iter() {
            writeln!(f, "{} :: {}", global.name, global._type)?;
        }

        for func in self.fn_defs.iter() {
            writeln!(f, "{func}")?;
        }

        Ok(())
    }
}

impl Display for LMIRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.prototype)?;

        for block in self.blocks.iter() {
            writeln!(f, "{block}")?;
        }

        Ok(())
    }
}

impl Display for LMIRBasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            ".{}:   ({})",
            self.id,
            self.debug_name.as_ref().unwrap_or(&String::new())
        )?;

        for instruction in self.body.iter() {
            writeln!(f, "\t{instruction}")?;
        }

        Ok(())
    }
}

impl Display for LMIRFunctionPrototype {
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

impl Display for LMIRGlobalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LMIRGlobalType::StringLiteral(s) => {
                write!(f, "string_literal \"{}\"", s.replace('\n', "\\n"))
            }
            LMIRGlobalType::Variable {
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

impl Display for LMIRRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.name)
    }
}

impl Display for LMIRValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LMIRValue::NULL => write!(f, "null"),
            LMIRValue::ParameterRef(index) => write!(f, "@param.{index}"),
            LMIRValue::IntImmediate { val, _type } => write!(f, "{_type} {val}"),
            LMIRValue::FloatImmediate { val, _type } => write!(f, "{_type} {val}"),
            LMIRValue::FunctionRef(name) => write!(f, "{name}"),
            LMIRValue::Global(id) => write!(f, "g{id}"),
            LMIRValue::Register { register, _type } => write!(f, "{_type} {register}"),
        }
    }
}

impl Display for LMIRInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(result) = &self.result {
            write!(f, "{} = ", result)?;
        }

        match &self.kind {
            LMIRInstructionKind::Alias { value } => {
                write!(f, "{value}")
            }
            LMIRInstructionKind::Allocate { _type, .. } => {
                write!(f, "alloca {_type}")
            }
            LMIRInstructionKind::Store {
                value,
                memory,
                _type,
            } => {
                write!(f, "store ({_type}) {value}, {memory}")
            }
            LMIRInstructionKind::Memcpy {
                dest, src, size, ..
            } => {
                write!(f, "memcpy {dest}, {src}, {size}")
            }
            LMIRInstructionKind::Load { memory, _type, .. } => {
                write!(f, "load {_type}, {memory}")
            }
            LMIRInstructionKind::ZeroMemory { memory, _type } => {
                write!(f, "*{memory} := 0")
            }
            LMIRInstructionKind::StructAccess {
                struct_,
                field_index,
                field_offset,
                ..
            } => {
                write!(f, "{struct_}.[{field_index}] (+{field_offset})")
            }

            LMIRInstructionKind::Coercion {
                value,
                coercion_type,
            } => {
                write!(f, "{} coerce {value} ({coercion_type:?})", self.value_type)
            }
            LMIRInstructionKind::Return { value } => {
                write!(f, "return")?;

                if let Some(value) = value {
                    write!(f, " {value}")?;
                }

                Ok(())
            }
            LMIRInstructionKind::Branch {
                condition,
                true_block,
                false_block,
            } => {
                write!(f, "if {condition} goto {true_block} else {false_block}")
            }
            LMIRInstructionKind::Phi { predecessors: from } => {
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
            LMIRInstructionKind::Jump { target } => {
                write!(f, "jump {target}")
            }
            LMIRInstructionKind::JumpTable {
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
            LMIRInstructionKind::DirectCall {
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
            LMIRInstructionKind::IndirectCall { func_ptr, args, .. } => {
                write!(f, "@(*{func_ptr})(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            LMIRInstructionKind::PointerBinOp {
                left,
                ptr_type,
                type_padded_size,
                right,
                op,
            } => {
                write!(f, "{left} {op} {right} [{ptr_type}*, {type_padded_size}]")
            }
            LMIRInstructionKind::IntegerBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [i]")
            }
            LMIRInstructionKind::IntegerUnOp { op, value } => {
                write!(f, "{op:?} {value} [i]")
            }
            LMIRInstructionKind::FloatBinOp { left, right, op } => {
                write!(f, "{left} {op} {right} [f]")
            }
            LMIRInstructionKind::FloatUnOp { op, value } => {
                write!(f, "{op:?} {value} [f]")
            }
            LMIRInstructionKind::GetFunctionAddr { func: func_name } => {
                write!(f, "get_function_addr {func_name}")
            }
            LMIRInstructionKind::CompilerAssumption { condition } => {
                write!(f, "compiler_assumption {condition}")
            }
        }
    }
}

impl Display for LMIRPtrBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LMIRPtrBinOp::ADD => "+",
                LMIRPtrBinOp::SUB => "-",

                LMIRPtrBinOp::EQ => "==",
                LMIRPtrBinOp::NE => "!=",

                LMIRPtrBinOp::LT => "<",
                LMIRPtrBinOp::GT => ">",
                LMIRPtrBinOp::LE => "<=",
                LMIRPtrBinOp::GE => ">=",
            },
        )
    }
}

impl Display for LMIRIntBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LMIRIntBinOp::ADD => "+",
                LMIRIntBinOp::SUB => "-",
                LMIRIntBinOp::MUL => "*",
                LMIRIntBinOp::IMUL => "i*",
                LMIRIntBinOp::UDIV => "u/",
                LMIRIntBinOp::IDIV => "i/",
                LMIRIntBinOp::UREM => "u%",
                LMIRIntBinOp::IREM => "i%",

                LMIRIntBinOp::SHL => "<<",
                LMIRIntBinOp::ASHR => "a>>",
                LMIRIntBinOp::LSHR => "l>>",

                LMIRIntBinOp::BAND => "&",
                LMIRIntBinOp::BOR => "|",
                LMIRIntBinOp::BXOR => "^",

                LMIRIntBinOp::LAND => "&&",
                LMIRIntBinOp::LOR => "||",

                LMIRIntBinOp::EQ => "==",
                LMIRIntBinOp::NE => "!=",

                LMIRIntBinOp::ILT => "<",
                LMIRIntBinOp::IGT => ">",
                LMIRIntBinOp::ILE => "<=",
                LMIRIntBinOp::IGE => ">=",

                LMIRIntBinOp::ULT => "(u) <",
                LMIRIntBinOp::UGT => "(u) >",
                LMIRIntBinOp::ULE => "(u) <=",
                LMIRIntBinOp::UGE => "(u) >=",
            },
        )
    }
}

impl Display for LMIRIntUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LMIRIntUnOp::BNOT => "~",
                LMIRIntUnOp::LNOT => "!",
                LMIRIntUnOp::NEG => "-",
            },
        )
    }
}

impl Display for LMIRFloatBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LMIRFloatBinOp::ADD => "+",
                LMIRFloatBinOp::SUB => "-",
                LMIRFloatBinOp::FMUL => "*",
                LMIRFloatBinOp::FDIV => "/",

                LMIRFloatBinOp::EQ => "==",
                LMIRFloatBinOp::NEQ => "!=",
                LMIRFloatBinOp::FLT => "<",
                LMIRFloatBinOp::FLE => "<=",
                LMIRFloatBinOp::FGT => ">",
                LMIRFloatBinOp::FGE => ">=",
            },
        )
    }
}

impl Display for LMIRFloatUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LMIRFloatUnOp::NEG => "-",
            },
        )
    }
}

impl Display for LMIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for LMIRIntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            LMIRIntegerType::I1 => write!(f, "i1"),
            LMIRIntegerType::I8 => write!(f, "i8"),
            LMIRIntegerType::I16 => write!(f, "i16"),
            LMIRIntegerType::I32 => write!(f, "i32"),
            LMIRIntegerType::I64 => write!(f, "i64"),
            LMIRIntegerType::I128 => write!(f, "i128"),
        }
    }
}

impl Display for LMIRFloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            LMIRFloatType::F32 => write!(f, "f32"),
            LMIRFloatType::F64 => write!(f, "f64"),
        }
    }
}

impl Display for LMIRTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            LMIRTypeKind::Opaque { bytes } => write!(f, "opaque_{}", *bytes),

            LMIRTypeKind::Integer(_type) => write!(f, "{}", _type),
            LMIRTypeKind::Float(_type) => write!(f, "{}", _type),

            LMIRTypeKind::Pointer {
                nullable,
                dereferenceable,
            } => {
                if !*nullable {
                    write!(f, "nonnull ")?;
                }

                write!(f, "ptr")?;

                if *dereferenceable > 0 {
                    write!(f, " (deref: {dereferenceable})")
                } else {
                    Ok(())
                }
            }

            LMIRTypeKind::Array { element, size } => {
                write!(f, "[{element}; {size}]")
            }
            LMIRTypeKind::Struct { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(_, _type)| format!("{_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "struct {{ {fields} }}")
            }
            LMIRTypeKind::Union { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(_, _type)| format!("{_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "union {{ {fields} }}")
            }

            LMIRTypeKind::Unit => write!(f, "()"),
        }
    }
}
