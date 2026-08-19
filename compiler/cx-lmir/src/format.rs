use crate::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use crate::{
    LMIRBasicBlock, LMIRBlockTarget, LMIRFloatBinOp, LMIRFloatUnOp, LMIRFunction,
    LMIRFunctionPrototype, LMIRFunctionSignature, LMIRGlobalInitializer, LMIRGlobalState,
    LMIRGlobalType, LMIRInstruction, LMIRInstructionKind, LMIRIntBinOp, LMIRIntUnOp, LMIRPtrBinOp,
    LMIRRegister, LMIRUnit, LMIRValue,
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
        write!(f, ".{}", self.id)?;
        if !self.params.is_empty() {
            f.write_str("(")?;
            for (index, param) in self.params.iter().enumerate() {
                if index != 0 {
                    f.write_str(", ")?;
                }
                write!(f, "{}: {}", param.register, param._type)?;
            }
            f.write_str(")")?;
        }
        writeln!(
            f,
            ":   ({})",
            self.debug_name.as_deref().unwrap_or_default()
        )?;

        for instruction in self.body.iter() {
            writeln!(f, "\t{instruction}")?;
        }

        Ok(())
    }
}

impl Display for LMIRFunctionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;

        for (i, arg) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg._type)?;
        }

        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for LMIRFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} :: {}", self.name, self.signature())
    }
}

impl Display for LMIRGlobalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LMIRGlobalType::StringLiteral(s) => {
                write!(f, "string_literal \"{}\"", s.replace('\n', "\\n"))
            }
            LMIRGlobalType::Variable { _type, state } => {
                if matches!(state, LMIRGlobalState::External) {
                    write!(f, "variable {_type} external")
                } else {
                    write!(f, "variable {_type} = {state}")
                }
            }
        }
    }
}

impl Display for LMIRGlobalState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::External => f.write_str("external"),
            Self::ZeroInitialized => f.write_str("zero"),
            Self::Initialized(initializer) => Display::fmt(initializer, f),
        }
    }
}

impl Display for LMIRGlobalInitializer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer {
                value,
                _type,
                signed,
            } => write!(
                f,
                "{value}:{}{}",
                if *signed { 'i' } else { 'u' },
                integer_width(*_type)
            ),
            Self::Float { value, _type } => write!(f, "{value}:{}", float_name(*_type)),
            Self::Aggregate { fields } => {
                f.write_str("{")?;
                for (index, value) in fields.iter().enumerate() {
                    if index != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{}: {}", value.0, value.1)?;
                }
                f.write_str("}")
            }
            Self::Global(global) => write!(f, "global({global})"),
            Self::GlobalOffset { global, offset } => {
                write!(f, "global({global}) + {offset}")
            }
            Self::Function(function) => write!(f, "function({function})"),
            Self::Null => f.write_str("null"),
        }
    }
}

fn integer_width(ty: LMIRIntegerType) -> u16 {
    match ty {
        LMIRIntegerType::I1 => 1,
        LMIRIntegerType::I8 => 8,
        LMIRIntegerType::I16 => 16,
        LMIRIntegerType::I32 => 32,
        LMIRIntegerType::I64 => 64,
        LMIRIntegerType::I128 => 128,
    }
}

fn float_name(ty: LMIRFloatType) -> &'static str {
    match ty {
        LMIRFloatType::F32 => "f32",
        LMIRFloatType::F64 => "f64",
    }
}

impl Display for LMIRBlockTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.block)?;
        if self.args.is_empty() {
            return Ok(());
        }

        f.write_str("(")?;
        for (index, argument) in self.args.iter().enumerate() {
            if index != 0 {
                f.write_str(", ")?;
            }
            write!(f, "{argument}")?;
        }
        f.write_str(")")
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
                let coerce_type_str = format!("{coercion_type:?}").to_ascii_lowercase();
                
                write!(f, "coerce.{} {value} -> {}", coerce_type_str, self.value_type)
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
                true_target,
                false_target,
            } => {
                write!(f, "if {condition} goto {true_target} else {false_target}")
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
                for (i, (key, target)) in targets.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key} -> {target}")?;
                }
                write!(f, "] else {default}")
            }
            LMIRInstructionKind::DirectCall { func, args, .. } => {
                write!(f, "@{}(", func)?;
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
            LMIRInstructionKind::VaStart { list, last } => {
                write!(f, "va_start({list}, {last})")
            }
            LMIRInstructionKind::VaEnd { list } => write!(f, "va_end({list})"),
            LMIRInstructionKind::VaArg { list, _type } => {
                write!(f, "va_arg({list}, {_type})")
            }
            LMIRInstructionKind::PointerBinOp {
                left,
                ptr_type,
                type_size,
                right,
                op,
            } => {
                write!(f, "{left} {op} {right} [{ptr_type}*, {type_size}]")
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
            LMIRInstructionKind::Unreachable => f.write_str("unreachable"),
        }
    }
}

impl Display for TypeSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}b", usize::from(*self))
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
                ..
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
            LMIRTypeKind::Vector { element, count } => {
                write!(f, "<{count} x {element}>")
            }
            LMIRTypeKind::Struct { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(_, _type)| format!("{_type}"))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "struct {{ {fields} }}")
            }

            LMIRTypeKind::Void => write!(f, "()"),
        }
    }
}
