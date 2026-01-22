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
        MIRExpressionFormatter::new(&self.body, 1).fmt(f)?;
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

struct MIRExpressionFormatter<'a> {
    expr: &'a MIRExpression,
    depth: usize,
}

impl<'a> MIRExpressionFormatter<'a> {
    fn new(expr: &'a MIRExpression, depth: usize) -> Self {
        Self { expr, depth }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.depth {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl Display for MIRExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        MIRExpressionFormatter::new(self, 0).fmt(f)
    }
}

impl<'a> Display for MIRExpressionFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.indent(f)?;
        match &self.expr.kind {
            MIRExpressionKind::BoolLiteral(value) => {
                writeln!(f, "BoolLiteral {} <'{}>", value, self.expr._type)
            }
            MIRExpressionKind::IntLiteral(value, int_type, signed) => {
                let prefix = if *signed { "i" } else { "u" };
                writeln!(
                    f,
                    "IntLiteral {}{}:{} <'{}>",
                    prefix,
                    int_type.bytes() * 8,
                    value,
                    self.expr._type
                )
            }
            MIRExpressionKind::FloatLiteral(value, float_type) => {
                writeln!(
                    f,
                    "FloatLiteral f{}:{} <'{}>",
                    float_type.bytes() * 8,
                    value,
                    self.expr._type
                )
            }
            MIRExpressionKind::Null => {
                writeln!(f, "Null <'{}>", self.expr._type)
            }
            MIRExpressionKind::Unit => {
                writeln!(f, "Unit <'{}>", self.expr._type)
            }
            MIRExpressionKind::Variable(name) => {
                writeln!(f, "LocalVariable {} <'{}>", name, self.expr._type)
            }
            MIRExpressionKind::FunctionReference { implicit_variables } => {
                writeln!(f, "FunctionReference <'{}>", self.expr._type)?;
                for var in implicit_variables {
                    MIRExpressionFormatter::new(var, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
                writeln!(f, "BinaryOperation {} <'{}>", op, self.expr._type)?;
                MIRExpressionFormatter::new(lhs, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(rhs, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::UnaryOperation { operand, op } => {
                writeln!(f, "UnaryOperation {} <'{}>", op, self.expr._type)?;
                MIRExpressionFormatter::new(operand, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::MemoryRead { source } => {
                writeln!(f, "MemoryRead <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(source, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::MemoryWrite { target, value } => {
                writeln!(f, "MemoryWrite <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(target, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(value, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::Typechange(expression) => {
                writeln!(f, "Typechange <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(expression, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::CreateStackVariable { name, _type } => {
                let name_str = name.as_ref().map(|t| t.as_str()).unwrap_or("(unnamed)");
                writeln!(
                    f,
                    "CreateStackVariable {}: {} <'{}>",
                    name_str, _type, self.expr._type
                )
            }
            MIRExpressionKind::CopyRegion { source, _type } => {
                writeln!(f, "CopyRegion [{}] <'{}>", _type, self.expr._type)?;
                MIRExpressionFormatter::new(source, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::StructFieldAccess {
                base, field_index, ..
            } => {
                writeln!(
                    f,
                    "StructFieldAccess index {} <'{}>",
                    field_index, self.expr._type
                )?;
                MIRExpressionFormatter::new(base, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::UnionAliasAccess {
                base,
                variant_type,
                union_type,
            } => {
                writeln!(
                    f,
                    "UnionAliasAccess [{} as {}] <'{}>",
                    variant_type, union_type, self.expr._type
                )?;
                MIRExpressionFormatter::new(base, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::ArrayAccess { array, index, .. } => {
                writeln!(f, "ArrayAccess <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(array, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(index, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::PatternIs {
                lhs,
                sum_type,
                variant_index,
                inner_name,
            } => {
                writeln!(
                    f,
                    "PatternIs variant {} of {} (create {}) <'{}>",
                    variant_index, sum_type, self.expr._type, inner_name
                )?;
                MIRExpressionFormatter::new(lhs, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::TaggedUnionTag { value, .. } => {
                writeln!(f, "TaggedUnionTag <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(value, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::TaggedUnionGet {
                value,
                variant_type,
            } => {
                writeln!(
                    f,
                    "TaggedUnionGet [{}] <'{}>",
                    variant_type, self.expr._type
                )?;
                MIRExpressionFormatter::new(value, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::TaggedUnionSet {
                target,
                variant_index,
                inner_value,
                ..
            } => {
                writeln!(
                    f,
                    "TaggedUnionSet variant {} <'{}>",
                    variant_index, self.expr._type
                )?;
                MIRExpressionFormatter::new(target, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(inner_value, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::ConstructTaggedUnion {
                variant_index,
                value,
                ..
            } => {
                writeln!(
                    f,
                    "ConstructTaggedUnion variant {} <'{}>",
                    variant_index, self.expr._type
                )?;
                MIRExpressionFormatter::new(value, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::StructInitializer {
                initializations,
                struct_type,
            } => {
                writeln!(
                    f,
                    "StructInitializer {} {{ <'{}>",
                    struct_type, self.expr._type
                )?;
                for (field_index, field_value) in initializations {
                    self.indent(f)?;
                    writeln!(f, "  Field {}:", field_index)?;
                    MIRExpressionFormatter::new(field_value, self.depth + 2).fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")
            }
            MIRExpressionKind::ArrayInitializer {
                elements,
                element_type,
            } => {
                writeln!(
                    f,
                    "ArrayInitializer {} [ <'{}>",
                    element_type, self.expr._type
                )?;
                for element in elements {
                    MIRExpressionFormatter::new(element, self.depth + 1).fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "]")
            }
            MIRExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(f, "If <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(condition, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(then_branch, self.depth + 1).fmt(f)?;
                if let Some(else_branch) = else_branch {
                    MIRExpressionFormatter::new(else_branch, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::While {
                condition,
                body,
                pre_eval,
            } => {
                let name = if *pre_eval { "WhileEval" } else { "While" };
                writeln!(f, "{} <'{}>", name, self.expr._type)?;
                MIRExpressionFormatter::new(condition, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(body, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                writeln!(f, "For <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(init, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(condition, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(increment, self.depth + 1).fmt(f)?;
                MIRExpressionFormatter::new(body, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::CSwitch {
                condition,
                cases,
                default,
            } => {
                writeln!(f, "CSwitch <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(condition, self.depth + 1).fmt(f)?;
                for (case_value, label) in cases {
                    self.indent(f)?;
                    writeln!(f, "Case:")?;
                    MIRExpressionFormatter::new(case_value, self.depth + 2).fmt(f)?;
                    MIRExpressionFormatter::new(label, self.depth + 2).fmt(f)?;
                }
                if let Some(default) = default {
                    self.indent(f)?;
                    writeln!(f, "Default:")?;
                    MIRExpressionFormatter::new(default, self.depth + 2).fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Match {
                condition,
                arms,
                default,
            } => {
                writeln!(f, "Match <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(condition, self.depth + 1).fmt(f)?;
                for (pattern, arm_body) in arms {
                    self.indent(f)?;
                    writeln!(f, "Arm:")?;
                    MIRExpressionFormatter::new(pattern, self.depth + 2).fmt(f)?;
                    MIRExpressionFormatter::new(arm_body, self.depth + 2).fmt(f)?;
                }
                if let Some(default) = default {
                    self.indent(f)?;
                    writeln!(f, "Default:")?;
                    MIRExpressionFormatter::new(default, self.depth + 2).fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Return { value } => {
                writeln!(f, "Return <'{}>", self.expr._type)?;
                if let Some(value) = value {
                    MIRExpressionFormatter::new(value, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::Block { statements } => {
                writeln!(f, "Block {{ <'{}>", self.expr._type)?;
                for stmt in statements {
                    MIRExpressionFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")
            }
            MIRExpressionKind::CallFunction {
                function,
                arguments,
            } => {
                writeln!(f, "CallFunction <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(function, self.depth + 1).fmt(f)?;
                for arg in arguments {
                    MIRExpressionFormatter::new(arg, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            MIRExpressionKind::TypeConversion {
                operand,
                conversion,
            } => {
                writeln!(f, "TypeConversion {:?} <'{}>", conversion, self.expr._type)?;
                MIRExpressionFormatter::new(operand, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::LifetimeStart { variable, _type } => {
                writeln!(
                    f,
                    "LifetimeStart {} ({}) <'{}>",
                    variable, _type, self.expr._type
                )
            }
            MIRExpressionKind::LifetimeEnd { variable, _type } => {
                writeln!(
                    f,
                    "LifetimeEnd {} ({}) <'{}>",
                    variable, _type, self.expr._type
                )
            }
            MIRExpressionKind::LeakLifetime { expression } => {
                writeln!(f, "LeakLifetime <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(expression, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::Defer { expression } => {
                writeln!(f, "Defer <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(expression, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::Move { source } => {
                writeln!(f, "Move <'{}>", self.expr._type)?;
                MIRExpressionFormatter::new(source, self.depth + 1).fmt(f)
            }
            MIRExpressionKind::Break => {
                writeln!(f, "Break <'{}>", self.expr._type)
            }
            MIRExpressionKind::Continue => {
                writeln!(f, "Continue <'{}>", self.expr._type)
            }
        }
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
        match self {
            MIRUnOp::NEG => write!(f, "neg"),
            MIRUnOp::INEG => write!(f, "ineg"),
            MIRUnOp::FNEG => write!(f, "fneg"),
            MIRUnOp::BNOT => write!(f, "bnot"),
            MIRUnOp::LNOT => write!(f, "lnot"),

            MIRUnOp::PreIncrement(amt) => write!(f, "pre_increment({})", amt),
            MIRUnOp::PostIncrement(amt) => write!(f, "post_increment({})", amt),
        }
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
            MIRCoercion::IntToBool => write!(f, "int_to_bool"),
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
            CXIntegerType::I1 => write!(f, "1"),
            CXIntegerType::I8 => write!(f, "8"),
            CXIntegerType::I16 => write!(f, "16"),
            CXIntegerType::I32 => write!(f, "32"),
            CXIntegerType::I64 => write!(f, "64"),
            CXIntegerType::I128 => write!(f, "128"),
        }
    }
}
