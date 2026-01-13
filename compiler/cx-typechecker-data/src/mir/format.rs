use crate::mir::expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRUnOp};
use crate::mir::program::{MIRFunction, MIRGlobalVarKind, MIRGlobalVariable, MIRUnit};
use crate::mir::types::{
    CXFloatType, CXIntegerType, MIRFunctionPrototype, MIRParameter, MIRType, MIRTypeKind,
};
use std::fmt::{Display, Formatter};

impl Display for MIRUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "MIR Unit:")?;

        writeln!(f, "\nFunction Prototypes:")?;
        for prototype in &self.prototypes {
            writeln!(f, "{prototype}")?;
        }

        writeln!(f, "\nFunctions:")?;
        for function in &self.functions {
            writeln!(f, "{function}")?;
        }

        writeln!(f, "\nEnd of MIR Unit")?;
        for global in &self.global_variables {
            writeln!(f, "{global}")?;
        }

        Ok(())
    }
}

impl Display for MIRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.prototype)?;
        writeln!(f, "{}", self.body)?;
        Ok(())
    }
}

impl Display for MIRFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        if self.var_args {
            if !self.params.is_empty() {
                write!(f, ", ")?;
            }
            write!(f, "...")?;
        }
        write!(f, ") -> {}", self.return_type)
    }
}

impl Display for MIRGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "global {} ", self.linkage)?;
        write!(f, "{}", self.kind)?;
        write!(
            f,
            " [{}]",
            if self.is_mutable {
                "mutable"
            } else {
                "immutable"
            }
        )?;
        Ok(())
    }
}

impl Display for MIRGlobalVarKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRGlobalVarKind::StringLiteral { name, value } => {
                // do basic sanitization of the string value for display
                let escaped_value = value
                    .replace('\\', "\\\\")
                    .replace('\n', "\\n")
                    .replace('\t', "\\t")
                    .replace('\"', "\\\"");

                write!(f, "string {} = \"{}\"", name, escaped_value)
            }
            MIRGlobalVarKind::Variable {
                name,
                _type,
                initializer,
            } => {
                if let Some(init) = initializer {
                    write!(f, "{} {} = {}", _type, name, init)
                } else {
                    write!(f, "{} {}", _type, name)
                }
            }
        }
    }
}

impl Display for MIRExpressionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRExpressionKind::BoolLiteral(value) => write!(f, "{}", value),
            MIRExpressionKind::IntLiteral(value, int_type, signed) => {
                if *signed {
                    write!(f, "i{}:{}", int_type.bytes() * 8, value)
                } else {
                    write!(f, "u{}:{}", int_type.bytes() * 8, value)
                }
            }
            MIRExpressionKind::FloatLiteral(value, float_type) => {
                write!(f, "f{}:{}", float_type.bytes() * 8, value)
            }
            MIRExpressionKind::Null => write!(f, "null"),
            MIRExpressionKind::Unit => write!(f, "()"),
            MIRExpressionKind::Parameter(name) => write!(f, "param {}", name),
            MIRExpressionKind::GlobalVariable(name) => write!(f, "global {}", name),
            MIRExpressionKind::LocalVariable(name) => write!(f, "var {}", name),
            MIRExpressionKind::FunctionReference { prototype, .. } => {
                write!(f, "fn {}", prototype.name)
            }
            MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
                write!(f, "({} {} {})", lhs.as_ref(), op, **rhs)
            }
            MIRExpressionKind::UnaryOperation { operand, op } => {
                write!(f, "({}{})", op, operand.as_ref())
            }
            MIRExpressionKind::MemoryRead { source, _type } => {
                write!(f, "load[{}] {}", _type, source.as_ref())
            }
            MIRExpressionKind::MemoryWrite { target, value } => {
                write!(f, "store {} <- {}", target.as_ref(), **value)
            }
            MIRExpressionKind::StackAllocation { _type } => write!(f, "alloca {}", _type),
            MIRExpressionKind::CopyRegion { source, _type } => {
                write!(f, "memcpy {} [{}]", source.as_ref(), _type)
            }
            MIRExpressionKind::StructFieldAccess {
                base, field_index, ..
            } => write!(f, "getfield {} [{}]", base.as_ref(), field_index),
            MIRExpressionKind::ArrayAccess { array, index, .. } => {
                write!(f, "index {} [{}]", array.as_ref(), index)
            }
            MIRExpressionKind::TaggedUnionTag { value, .. } => {
                write!(f, "tagof {}", value.as_ref())
            }
            MIRExpressionKind::TaggedUnionGet {
                value,
                variant_type,
            } => write!(f, "getvariant {} [{}]", value.as_ref(), variant_type),
            MIRExpressionKind::ConstructTaggedUnion {
                variant_index,
                value,
                ..
            } => write!(f, "makevariant [{}] {}", variant_index, value.as_ref()),
            MIRExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "if {} {{ {} }}",
                    condition.as_ref(),
                    then_branch.as_ref()
                )?;
                if let Some(else_branch) = else_branch {
                    write!(f, " else {{ {} }}", else_branch.as_ref())?;
                }
                Ok(())
            }
            MIRExpressionKind::While {
                condition,
                body,
                pre_eval,
            } => {
                if *pre_eval {
                    write!(
                        f,
                        "while_eval {} {{ {} }}",
                        condition.as_ref(),
                        body.as_ref()
                    )
                } else {
                    write!(f, "while {} {{ {} }}", condition.as_ref(), body.as_ref())
                }
            }
            MIRExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                write!(
                    f,
                    "for ({}; {}; {}) {{ {} }}",
                    init.as_ref(),
                    condition.as_ref(),
                    increment.as_ref(),
                    body.as_ref()
                )
            }
            MIRExpressionKind::Match {
                condition,
                arms,
                default,
            } => {
                write!(f, "match {} {{", condition.as_ref())?;
                for (pattern, arm_body) in arms {
                    write!(f, " {} => {{ {} }}", pattern.as_ref(), **arm_body)?;
                }
                if let Some(default) = default {
                    write!(f, " _ => {{ {} }}", default.as_ref())?;
                }
                write!(f, " }}")
            }
            MIRExpressionKind::Return { value } => {
                if let Some(value) = value {
                    write!(f, "return {}", value.as_ref())
                } else {
                    write!(f, "return")
                }
            }
            MIRExpressionKind::Block { statements } => {
                write!(f, "{{")?;
                for stmt in statements {
                    write!(f, "    {};", stmt)?;
                }
                write!(f, " }}")
            }
            MIRExpressionKind::CallFunction {
                function,
                arguments,
            } => {
                write!(f, "call {}(", function.as_ref())?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            MIRExpressionKind::TypeConversion {
                operand,
                conversion,
            } => {
                write!(f, "cast({}) as {:?}", operand.as_ref(), conversion)
            }
            MIRExpressionKind::LifetimeStart { variable, _type } => {
                write!(f, "lifetime_start {} ({})", variable, _type)
            }
            MIRExpressionKind::LifetimeEnd { variable, _type } => {
                write!(f, "lifetime_end {} ({})", variable, _type)
            }
            MIRExpressionKind::LeakLifetime { expression } => {
                write!(f, "leak_lifetime {}", expression.as_ref())
            }
            MIRExpressionKind::Defer { expression } => write!(f, "defer {}", expression.as_ref()),
        }
    }
}

impl Display for MIRExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self._type)
    }
}

impl Display for MIRParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{name}: {}", self._type)
        } else {
            write!(f, "{}", self._type)
        }
    }
}

impl Display for MIRBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRBinOp::Float { ftype, op } => write!(f, "f{} {:?}", ftype.bytes() * 8, op),
            MIRBinOp::Integer { itype, op } => write!(f, "i{} {:?}", itype.bytes() * 8, op),
            MIRBinOp::PtrDiff { ptr_inner, op } => write!(f, "ptrdiff<{}> {:?}", ptr_inner, op),
            MIRBinOp::Pointer { op } => write!(f, "ptr {:?}", op),
        }
    }
}

impl Display for MIRUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MIRUnOp::NEG => "neg",
                MIRUnOp::INEG => "ineg",
                MIRUnOp::FNEG => "fneg",
                MIRUnOp::BNOT => "bnot",
                MIRUnOp::LNOT => "lnot",
            }
        )
    }
}

impl Display for MIRCoercion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRCoercion::Integral { sextend, to_type } => write!(
                f,
                "integral({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::FloatCast { to_type } => write!(f, "fp_integral(to: {})", to_type),
            MIRCoercion::PtrToInt { to_type } => write!(f, "ptr_to_int(to: {})", to_type),
            MIRCoercion::IntToPtr { sextend } => {
                write!(f, "int_to_ptr({})", if *sextend { "sext" } else { "zext" })
            }
            MIRCoercion::IntToFloat { to_type, sextend } => write!(
                f,
                "int_to_float({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::FloatToInt { sextend, to_type } => write!(
                f,
                "float_to_int({}, to: {})",
                if *sextend { "sext" } else { "zext" },
                to_type
            ),
            MIRCoercion::ReinterpretBits => write!(f, "reinterpret_bits"),
        }
    }
}

impl Display for MIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Here you might want to add specifiers if they are relevant for display
        write!(f, "{}", self.kind)
    }
}

impl Display for MIRTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MIRTypeKind::Integer { _type, signed } => {
                write!(f, "{}{}", if *signed { 'i' } else { 'u' }, _type)
            }
            MIRTypeKind::Float { _type } => write!(f, "{}", _type),
            MIRTypeKind::Structured { name, .. } => {
                write!(
                    f,
                    "struct {}",
                    name.as_ref()
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "".to_string())
                )
            }
            MIRTypeKind::Union { name, .. } => {
                write!(
                    f,
                    "union {}",
                    name.as_ref()
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "".to_string())
                )
            }
            MIRTypeKind::TaggedUnion { name, .. } => {
                write!(f, "tagged_union {} ", name)
            }
            MIRTypeKind::Unit => write!(f, "()"),
            MIRTypeKind::PointerTo { inner_type, .. } => write!(f, "{}*", inner_type),
            MIRTypeKind::MemoryReference(inner) => write!(f, "{}&", inner),
            MIRTypeKind::Array { size, inner_type } => write!(f, "[{}; {}]", inner_type, size),
            MIRTypeKind::Opaque { name, .. } => write!(f, "opaque {}", name),
            MIRTypeKind::Function { prototype } => write!(f, "{prototype}"),
        }
    }
}

impl Display for CXFloatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXFloatType::F32 => write!(f, "f32"),
            CXFloatType::F64 => write!(f, "f64"),
        }
    }
}

impl Display for CXIntegerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXIntegerType::I1 => write!(f, "i1"),
            CXIntegerType::I8 => write!(f, "i8"),
            CXIntegerType::I16 => write!(f, "i16"),
            CXIntegerType::I32 => write!(f, "i32"),
            CXIntegerType::I64 => write!(f, "i64"),
            CXIntegerType::I128 => write!(f, "i128"),
        }
    }
}
