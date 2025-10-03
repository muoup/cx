use super::ast::*;
use crate::cx_types::{CXFunctionPrototype, CXParameter, CXType, CXTypeKind};
use std::fmt::{Display, Formatter, Result};

// Helper struct for indented formatting of TCExpr
struct TCExprFormatter<'a> {
    expr: &'a TCExpr,
    depth: usize,
}

impl<'a> TCExprFormatter<'a> {
    fn new(expr: &'a TCExpr, depth: usize) -> Self {
        Self { expr, depth }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..self.depth {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl Display for TCAST {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "TCAST for file: {}", self.source_file)?;
        if !self.global_variables.is_empty() {
            writeln!(f, "\n-- Global Variables --")?;
            for global in &self.global_variables {
                writeln!(f, "{global}")?;
            }
        }
        if !self.function_defs.is_empty() {
            writeln!(f, "\n-- Function Definitions --")?;
            for func in &self.function_defs {
                writeln!(f, "{func}")?;
            }
        }
        Ok(())
    }
}

impl Display for TCGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            TCGlobalVariable::UnaddressableConstant { name, val } => {
                write!(f, "UnaddressableConstant {name} = {val}")
            }
            TCGlobalVariable::StringLiteral { name, value } => {
                write!(f, "StringLiteral {} = \"{}\"", name, value.escape_default())
            }
            TCGlobalVariable::Variable {
                name,
                _type,
                initializer,
            } => {
                write!(f, "Variable {name} : {_type}")?;
                if let Some(init) = initializer {
                    write!(f, " = {init}")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for TCFunctionDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "FunctionDef {} {{", self.prototype)?;
        write!(f, "{}", TCExprFormatter::new(&self.body, 1))?;
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl Display for TCExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        TCExprFormatter::new(self, 0).fmt(f)
    }
}

impl<'a> Display for TCExprFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.indent(f)?;
        match &self.expr.kind {
            TCExprKind::Taken => writeln!(f, "Taken {}", self.expr._type)?,
            TCExprKind::Unit => writeln!(f, "Unit {}", self.expr._type)?,
            TCExprKind::Block { statements } => {
                writeln!(f, "Block {{{}}}", self.expr._type)?;
                for stmt in statements {
                    TCExprFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")?;
            }
            TCExprKind::IntLiteral { value } => {
                writeln!(f, "IntLiteral {} {}", value, self.expr._type)?;
            }
            TCExprKind::FloatLiteral { value } => {
                writeln!(f, "FloatLiteral {} {}", value, self.expr._type)?;
            }
            TCExprKind::SizeOf { _type } => {
                writeln!(f, "SizeOf {} {}", _type, self.expr._type)?;
            }
            TCExprKind::VariableDeclaration { type_, name } => {
                writeln!(
                    f,
                    "VariableDeclaration {} {} {}",
                    name, type_, self.expr._type
                )?;
            }
            TCExprKind::GlobalVariableReference { name } => {
                writeln!(f, "GlobalVariableReference {} {}", name, self.expr._type)?;
            }
            TCExprKind::VariableReference { name } => {
                writeln!(f, "VariableReference {} {}", name, self.expr._type)?;
            }
            TCExprKind::FunctionReference { name } => {
                writeln!(f, "FunctionReference {} {}", name, self.expr._type)?;
            }
            TCExprKind::MemberFunctionReference {
                target,
                mangled_name,
            } => {
                writeln!(
                    f,
                    "MemberFunctionReference {} {}",
                    mangled_name, self.expr._type
                )?;
                TCExprFormatter::new(target, self.depth + 1).fmt(f)?;
            }
            TCExprKind::FunctionCall {
                function,
                arguments,
                direct_call,
            } => {
                writeln!(
                    f,
                    "FunctionCall (direct: {}) {}",
                    direct_call, self.expr._type
                )?;
                TCExprFormatter::new(function, self.depth + 1).fmt(f)?;
                for arg in arguments {
                    TCExprFormatter::new(arg, self.depth + 1).fmt(f)?;
                }
            }
            TCExprKind::Access { target, field } => {
                writeln!(f, "Access .{} {}", field, self.expr._type)?;
                TCExprFormatter::new(target, self.depth + 1).fmt(f)?;
            }
            TCExprKind::Assignment {
                target,
                value,
                additional_op,
            } => {
                if let Some(op) = additional_op {
                    writeln!(f, "Assignment (op={:?}) {}", op, self.expr._type)?;
                } else {
                    writeln!(f, "Assignment {}", self.expr._type)?;
                }
                TCExprFormatter::new(target, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(value, self.depth + 1).fmt(f)?;
            }
            TCExprKind::BinOp { lhs, rhs, op } => {
                writeln!(f, "BinOp {:?} {}", op, self.expr._type)?;
                TCExprFormatter::new(lhs, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(rhs, self.depth + 1).fmt(f)?;
            }
            TCExprKind::UnOp { operand, operator } => {
                writeln!(f, "UnOp {:?} {}", operator, self.expr._type)?;
                TCExprFormatter::new(operand, self.depth + 1).fmt(f)?;
            }
            TCExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(f, "If {}", self.expr._type)?;
                TCExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(then_branch, self.depth + 1).fmt(f)?;
                if let Some(else_b) = else_branch {
                    TCExprFormatter::new(else_b, self.depth + 1).fmt(f)?;
                }
            }
            TCExprKind::While {
                condition,
                body,
                pre_eval,
            } => {
                writeln!(f, "While (pre_eval: {}) {}", pre_eval, self.expr._type)?;
                TCExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(body, self.depth + 1).fmt(f)?;
            }
            TCExprKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                writeln!(f, "For {}", self.expr._type)?;
                TCExprFormatter::new(init, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(increment, self.depth + 1).fmt(f)?;
                TCExprFormatter::new(body, self.depth + 1).fmt(f)?;
            }
            TCExprKind::CSwitch {
                condition,
                block,
                cases,
                default_case,
            } => {
                writeln!(
                    f,
                    "CSwitch cases: {:?}, default: {:?} {}",
                    cases, default_case, self.expr._type
                )?;
                TCExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                for stmt in block {
                    TCExprFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
            }
            TCExprKind::Match {
                condition,
                cases,
                default_case,
            } => {
                writeln!(f, "Match {}", self.expr._type)?;
                TCExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                for case_ in cases {
                    self.indent(f)?;
                    writeln!(f, "{case_}")?;
                    TCExprFormatter::new(&case_.body, self.depth + 1).fmt(f)?;
                }
                if let Some(default_b) = default_case {
                    self.indent(f)?;
                    writeln!(f, "Default:")?;
                    TCExprFormatter::new(default_b, self.depth + 1).fmt(f)?;
                }
            }
            TCExprKind::ConstructorMatchIs {
                expr,
                union_type,
                var_name,
                ..
            } => {
                writeln!(
                    f,
                    "ConstructorMatchIs is {}::{} {}",
                    union_type, var_name, self.expr._type
                )?;
                TCExprFormatter::new(expr, self.depth + 1).fmt(f)?;
            }
            TCExprKind::ImplicitLoad { operand } => {
                writeln!(f, "ImplicitLoad {}", self.expr._type)?;
                TCExprFormatter::new(operand, self.depth + 1).fmt(f)?;
            }
            TCExprKind::TemporaryBuffer { _type } => {
                writeln!(f, "TemporaryBuffer {} {}", _type, self.expr._type)?;
            }
            TCExprKind::Coercion { operand, cast_type } => {
                writeln!(f, "Coercion to {:?} {}", cast_type, self.expr._type)?;
                TCExprFormatter::new(operand, self.depth + 1).fmt(f)?;
            }
            TCExprKind::Defer { operand } => {
                writeln!(f, "Defer {}", self.expr._type)?;
                TCExprFormatter::new(operand, self.depth + 1).fmt(f)?;
            }
            TCExprKind::New {
                _type,
                array_length,
            } => {
                if let Some(len) = array_length {
                    writeln!(f, "New[] {} {}", _type, self.expr._type)?;
                    TCExprFormatter::new(len, self.depth + 1).fmt(f)?;
                } else {
                    writeln!(f, "New {} {}", _type, self.expr._type)?;
                }
            }
            TCExprKind::Move { operand } => {
                writeln!(f, "Move {}", self.expr._type)?;
                TCExprFormatter::new(operand, self.depth + 1).fmt(f)?;
            }
            TCExprKind::Return { value } => {
                writeln!(f, "Return {}", self.expr._type)?;
                if let Some(val) = value {
                    TCExprFormatter::new(val, self.depth + 1).fmt(f)?;
                }
            }
            TCExprKind::InitializerList { indices } => {
                writeln!(f, "InitializerList {}", self.expr._type)?;
                for index in indices {
                    TCInitIndexFormatter::new(index, self.depth + 1).fmt(f)?;
                }
            }
            TCExprKind::TypeConstructor {
                name,
                union_type,
                input,
                ..
            } => {
                writeln!(
                    f,
                    "TypeConstructor {}::{} {}",
                    union_type, name, self.expr._type
                )?;
                TCExprFormatter::new(input, self.depth + 1).fmt(f)?;
            }
            TCExprKind::Break => writeln!(f, "Break {}", self.expr._type)?,
            TCExprKind::Continue => writeln!(f, "Continue {}", self.expr._type)?,
        }

        Ok(())
    }
}

impl Display for TCTagMatch {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "case tag {} as {}", self.tag_value, self.instance_name)?;
        Ok(())
    }
}

struct TCInitIndexFormatter<'a> {
    index: &'a TCInitIndex,
    depth: usize,
}

impl<'a> TCInitIndexFormatter<'a> {
    fn new(index: &'a TCInitIndex, depth: usize) -> Self {
        Self { index, depth }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..self.depth {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl<'a> Display for TCInitIndexFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.indent(f)?;
        if let Some(name) = &self.index.name {
            writeln!(f, ".{name} = ")?;
        } else {
            writeln!(f, "[{}] = ", self.index.index)?;
        }
        TCExprFormatter::new(&self.index.value, self.depth + 1).fmt(f)?;
        Ok(())
    }
}

impl Display for CXType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.specifiers != 0 {
            // Not all specifiers may be covered, so this is a fallback
            write!(f, "specifiers: {} ", self.specifiers)?;
        }
        write!(f, "{}", self.kind)
    }
}

impl Display for CXTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            CXTypeKind::Integer { bytes, signed } => {
                write!(f, "{}{}", if *signed { "i" } else { "u" }, bytes * 8)
            }
            CXTypeKind::Float { bytes } => write!(f, "f{}", bytes * 8),
            CXTypeKind::Bool => write!(f, "bool"),
            CXTypeKind::Structured { name, .. } => {
                write!(
                    f,
                    "struct {}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or("<anonymous>")
                )
            }
            CXTypeKind::Union { name, .. } => {
                write!(
                    f,
                    "union {}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or("<anonymous>")
                )
            }
            CXTypeKind::TaggedUnion { name, .. } => {
                write!(f, "tagged union {name}")
            }
            CXTypeKind::Unit => write!(f, "()"),
            CXTypeKind::StrongPointer { inner_type, .. } => write!(f, "@{inner_type}"),
            CXTypeKind::PointerTo { inner_type, .. } => write!(f, "*{inner_type}"),
            CXTypeKind::MemoryReference(inner) => write!(f, "&{inner}"),
            CXTypeKind::Array { size, inner_type } => write!(f, "[{inner_type}; {size}]"),
            CXTypeKind::VariableLengthArray { _type, .. } => write!(f, "[{_type}..]"),
            CXTypeKind::Opaque { name, .. } => write!(f, "opaque {name}"),
            CXTypeKind::Function { prototype } => write!(f, "fn {prototype}"),
        }
    }
}

impl Display for CXFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}(", self.name)?;
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

impl Display for CXParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(name) = &self.name {
            write!(f, "{}: {}", name, self._type)
        } else {
            write!(f, "{}", self._type)
        }
    }
}
