use cx_util::identifier::CXIdent;
use std::fmt::{Display, Formatter, Result};

use crate::{
    ast::{CXBinOp, CXExpr, CXExprKind, CXFunctionStmt, CXGlobalVariable, CXInitIndex, CXAST},
    data::{
        CXFunctionKey, CXFunctionKind, CXFunctionTypeIdent, CXLinkageMode, CXNaivePrototype,
        CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind, CXTemplate,
    },
};

// Helper struct for indented formatting of CXExpr
struct CXExprFormatter<'a> {
    expr: &'a CXExpr,
    depth: usize,
}

impl<'a> CXExprFormatter<'a> {
    fn new(expr: &'a CXExpr, depth: usize) -> Self {
        Self { expr, depth }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..self.depth {
            write!(f, "  ")?;
        }
        Ok(())
    }

    fn indent_plus_one(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..(self.depth + 1) {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl Display for CXAST {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "CXAST for file: {}", self.file_path)?;
        if !self.global_variables.is_empty() {
            writeln!(f, "\n-- Global Variables --")?;
            for global in &self.global_variables {
                writeln!(f, "{}: {}", global.0, global.1.resource)?;
            }
        }
        if !self.function_stmts.is_empty() {
            writeln!(f, "\n-- Function Definitions --")?;
            for func in &self.function_stmts {
                writeln!(f, "{func}")?;
            }
        }
        Ok(())
    }
}

impl Display for CXLinkageMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            CXLinkageMode::Standard => write!(f, "standard"),
            CXLinkageMode::Static => write!(f, "static"),
            CXLinkageMode::Extern => write!(f, "extern"),
        }
    }
}

impl Display for CXGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            CXGlobalVariable::EnumConstant(val) => {
                write!(f, "enum constant {}", val)
            }

            CXGlobalVariable::Standard {
                _type,
                is_mutable,
                initializer,
            } => {
                write!(
                    f,
                    "global variable {} {}",
                    if *is_mutable { "mut" } else { "const" },
                    _type
                )?;

                if let Some(initializer) = initializer {
                    write!(f, " = {}", initializer)?;
                }

                Ok(())
            }
        }
    }
}

impl Display for CXFunctionStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            CXFunctionStmt::TypeDecl { name, _type } => {
                if let Some(name) = name {
                    writeln!(f, "type {} = {}", name, _type)
                } else {
                    writeln!(f, "type {}", _type)
                }
            }

            CXFunctionStmt::FunctionDefinition { prototype, body } => {
                writeln!(f, "FunctionDef {} {{ ", prototype)?;
                write!(f, "{}", CXExprFormatter::new(body, 1))?;
                writeln!(f, "}}")
            }

            CXFunctionStmt::DestructorDefinition { _type, body } => {
                writeln!(f, "DestructorDef for {} {{ ", _type)?;
                write!(f, "{}", CXExprFormatter::new(body, 1))?;
                writeln!(f, "}}")
            }

            CXFunctionStmt::TemplatedFunction { prototype, body } => {
                writeln!(f, "TemplatedFunction {prototype} {{ ")?;
                write!(f, "{}", CXExprFormatter::new(body, 1))?;
                writeln!(f, "}}")
            }
        }
    }
}

impl Display for CXExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        CXExprFormatter::new(self, 0).fmt(f)
    }
}

impl<'a> Display for CXExprFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.indent(f)?;
        match &self.expr.kind {
            CXExprKind::Block { exprs, .. } => {
                writeln!(f, "Block {{ ")?;
                for stmt in exprs {
                    CXExprFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")
            }
            CXExprKind::Identifier(ident) => writeln!(f, "Identifier {}", ident),
            CXExprKind::TemplatedIdentifier {
                name: fn_name,
                template_input,
            } => {
                let arg_string = template_input
                    .params
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                writeln!(f, "TemplatedIdentifier {}<{}>", fn_name, arg_string)
            }
            CXExprKind::VarDeclaration { name, _type } => {
                writeln!(f, "VarDeclaration {}: {}", name, _type)
            }
            CXExprKind::IntLiteral { val, .. } => writeln!(f, "IntLiteral {}", val),
            CXExprKind::FloatLiteral { val, .. } => writeln!(f, "FloatLiteral {}", val),
            CXExprKind::StringLiteral { val, .. } => {
                writeln!(f, "StringLiteral \"{}\"", val.escape_default())
            }
            CXExprKind::Return { value } => {
                writeln!(f, "Return")?;
                if let Some(value) = value {
                    CXExprFormatter::new(value, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            CXExprKind::BinOp { lhs, rhs, op } => {
                writeln!(f, "BinOp {:?}", op)?;
                CXExprFormatter::new(lhs, self.depth + 1).fmt(f)?;
                CXExprFormatter::new(rhs, self.depth + 1).fmt(f)?;
                Ok(())
            }
            CXExprKind::Move { expr } => {
                writeln!(f, "Move")?;
                CXExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            CXExprKind::InitializerList { indices } => {
                writeln!(f, "InitializerList")?;
                for index in indices {
                    CXInitIndexFormatter::new(index, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            CXExprKind::UnOp { operator, operand } => {
                writeln!(f, "UnOp {:?}", operator)?;
                CXExprFormatter::new(operand, self.depth + 1).fmt(f)
            }
            CXExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(f, "If")?;
                CXExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                CXExprFormatter::new(then_branch, self.depth + 1).fmt(f)?;
                if let Some(else_branch) = else_branch {
                    CXExprFormatter::new(else_branch, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            CXExprKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                writeln!(f, "For")?;
                CXExprFormatter::new(init, self.depth + 1).fmt(f)?;
                CXExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                CXExprFormatter::new(increment, self.depth + 1).fmt(f)?;
                CXExprFormatter::new(body, self.depth + 1).fmt(f)?;
                Ok(())
            }
            CXExprKind::While {
                condition, body, ..
            } => {
                writeln!(f, "While")?;
                CXExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                CXExprFormatter::new(body, self.depth + 1).fmt(f)?;
                Ok(())
            }
            CXExprKind::Defer { expr } => {
                writeln!(f, "Defer")?;
                CXExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            CXExprKind::New { _type } => {
                writeln!(f, "New {}", _type)
            }
            CXExprKind::SizeOf { expr } => {
                writeln!(f, "SizeOf")?;
                CXExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            CXExprKind::Taken => writeln!(f, "Taken"),
            CXExprKind::Unit => writeln!(f, "Unit"),
            CXExprKind::Match {
                condition,
                arms,
                default,
            } => {
                writeln!(f, "Match")?;
                CXExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                for (pattern, arm_expr) in arms {
                    self.indent_plus_one(f)?;
                    writeln!(f, "Pattern: {}", pattern)?;
                    CXExprFormatter::new(arm_expr, self.depth + 1).fmt(f)?;
                }
                if let Some(default_expr) = default {
                    self.indent_plus_one(f)?;
                    writeln!(f, "Default:")?;
                    CXExprFormatter::new(default_expr, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            CXExprKind::Switch {
                condition,
                block,
                cases,
                default_case,
            } => {
                writeln!(f, "Switch")?;
                CXExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                for (case_value, case_expr) in cases {
                    self.indent(f)?;
                    writeln!(f, "Case: {} -> ID: {}", case_value, case_expr)?;
                }
                if let Some(default_expr) = default_case {
                    self.indent(f)?;
                    writeln!(f, "Default -> ID: {}", default_expr)?;
                }
                for (i, stmt) in block.iter().enumerate() {
                    self.indent(f)?;
                    writeln!(f, "Stmt[{}]: ", i)?;
                    CXExprFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            CXExprKind::TypeConstructor {
                union_name,
                variant_name,
                inner,
            } => {
                writeln!(f, "TypeConstructor {}::{}", union_name, variant_name)?;
                CXExprFormatter::new(inner, self.depth + 1).fmt(f)
            }
            CXExprKind::Break => writeln!(f, "Break"),
            CXExprKind::Continue => writeln!(f, "Continue"),
        }
    }
}

impl Display for CXBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            CXBinOp::Add => write!(f, "+"),
            CXBinOp::Subtract => write!(f, "-"),
            CXBinOp::Multiply => write!(f, "*"),
            CXBinOp::Divide => write!(f, "/"),
            CXBinOp::Modulus => write!(f, "%"),
            CXBinOp::Equal => write!(f, "=="),
            CXBinOp::NotEqual => write!(f, "!="),
            CXBinOp::Less => write!(f, "<"),
            CXBinOp::LessEqual => write!(f, "<="),
            CXBinOp::Greater => write!(f, ">"),
            CXBinOp::GreaterEqual => write!(f, ">="),
            CXBinOp::Access => write!(f, "."),
            CXBinOp::ScopeRes => write!(f, "::"),
            CXBinOp::MethodCall => write!(f, "()"),
            CXBinOp::ArrayIndex => write!(f, "[]"),
            CXBinOp::Comma => write!(f, ","),
            CXBinOp::Assign(add) => {
                if let Some(add) = add {
                    write!(f, "{} =", add)
                } else {
                    write!(f, "=")
                }
            }
            CXBinOp::Is => write!(f, "is"),

            CXBinOp::LAnd => write!(f, "&&"),
            CXBinOp::LOr => write!(f, "||"),
            CXBinOp::BitAnd => write!(f, "&"),
            CXBinOp::BitOr => write!(f, "|"),
            CXBinOp::BitXor => write!(f, "^"),
            CXBinOp::LShift => write!(f, "<<"),
            CXBinOp::RShift => write!(f, ">>"),
        }
    }
}

struct CXInitIndexFormatter<'a> {
    index: &'a CXInitIndex,
    depth: usize,
}

impl<'a> CXInitIndexFormatter<'a> {
    fn new(index: &'a CXInitIndex, depth: usize) -> Self {
        Self { index, depth }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..self.depth {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl<'a> Display for CXInitIndexFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.indent(f)?;
        if let Some(name) = &self.index.name {
            writeln!(f, ".{name} = ")?;
        } else {
            writeln!(f, "[] = ")?;
        }
        CXExprFormatter::new(&self.index.value, self.depth + 1).fmt(f)?;
        Ok(())
    }
}

impl Display for CXNaiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for CXNaiveTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CXNaiveTypeKind::Identifier { name, .. } => write!(f, "{name}"),
            CXNaiveTypeKind::TemplatedIdentifier { name, input } => write!(f, "{name}{input}"),
            CXNaiveTypeKind::ExplicitSizedArray(inner, size) => write!(f, "[{inner}; {size}]"),
            CXNaiveTypeKind::ImplicitSizedArray(inner) => write!(f, "[{inner}]"),
            CXNaiveTypeKind::MemoryReference { inner_type } => {
                write!(f, "&{}", inner_type)
            }
            CXNaiveTypeKind::PointerTo { inner_type, weak } => {
                write!(f, "{}{}", if *weak { "weak " } else { "" }, inner_type)
            }

            CXNaiveTypeKind::Structured { name, fields, .. } => {
                let fields_str = fields
                    .iter()
                    .map(|(_, ty)| format!("{ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "struct {} {{ {} }}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or("__anonymous__"),
                    fields_str
                )
            }
            CXNaiveTypeKind::Union { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|(_, ty)| format!("{ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "union {} {{ {} }}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or(""),
                    fields_str
                )
            }
            CXNaiveTypeKind::TaggedUnion { name, variants } => {
                let variants_str = variants
                    .iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "union class {name} {{ {variants_str} }}")
            }
            CXNaiveTypeKind::FunctionPointer { prototype } => {
                write!(f, "FunctionPointer({prototype})")
            }
        }
    }
}

impl Display for CXNaivePrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| {
                format!(
                    "{}: {}",
                    param.name.as_ref().unwrap_or(&CXIdent::new("_")),
                    param._type
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{} {}({})", self.return_type, self.kind, params_str)
    }
}

impl Display for CXNaiveTemplateInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params_str = self
            .params
            .iter()
            .map(|param| param.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "<{params_str}>")
    }
}

impl<Shell: Display> Display for CXTemplate<Shell> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.shell.fmt(f)?;

        let params_str = self.prototype.types.join(", ");
        write!(f, "<{params_str}>")
    }
}

impl Display for CXFunctionTypeIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXFunctionTypeIdent::Standard(name) => write!(f, "{name}"),
            CXFunctionTypeIdent::Templated(name, template_input) => {
                write!(f, "{}{}", name, template_input)
            }
        }
    }
}

impl Display for CXFunctionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXFunctionKind::Standard(name) => write!(f, "{name}"),
            CXFunctionKind::MemberFunction { member_type, name } => {
                write!(f, "{member_type}::{name}")
            }
            CXFunctionKind::StaticMemberFunction { member_type, name } => {
                write!(f, "{member_type}::{name}")
            }
            CXFunctionKind::Destructor(base) => {
                write!(f, "~{base}")
            }
        }
    }
}

impl Display for CXFunctionKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXFunctionKey::Standard(name) => write!(f, "{name}"),
            CXFunctionKey::MemberFunction { type_base_name, name } => {
                write!(f, "(non-static) {type_base_name}::{name}")
            }
            CXFunctionKey::StaticMemberFunction { type_base_name, name } => {
                write!(f, "(static) {type_base_name}::{name}")
            }
            CXFunctionKey::Destructor { type_base_name } => {
                write!(f, "~{type_base_name}")
            }
        }
    }
}
