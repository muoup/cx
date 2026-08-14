use cx_util::identifier::CXIdent;
use std::fmt::{Debug, Display, Formatter, Result};

use crate::ast::{
    expression::{HIRBinOp, HIRExprKind, HIRExpression, HIRInitIndex},
    function::{
        HIRComptimeFnPrototype, HIRComptimeValueType, HIRFunctionKind, HIRFunctionPrototype,
    },
    global_var::{HIREnumVariant, HIRGlobalVariable},
    pattern::HIRPattern,
    template::HIRTemplateInput,
    types::{HIRField, HIRMoveSemantics, HIRType, HIRTypeKind},
    HIRDefinition, HIRStmt, HIR,
};

// Helper struct for indented formatting of CXExpr
struct HIRExprFormatter<'a> {
    expr: &'a HIRExpression,
    depth: usize,
}

impl<'a> HIRExprFormatter<'a> {
    fn new(expr: &'a HIRExpression, depth: usize) -> Self {
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

impl Display for HIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        writeln!(f, "HIR for file: {}", self.module_path)?;

        for def in self.definition_stmts.iter() {
            writeln!(f, "{}", def)?;
        }

        Ok(())
    }
}

impl Display for HIREnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(value) = &self.value {
            write!(f, "{} = {}", self.name, value)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl Display for HIRGlobalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            HIRGlobalVariable::EnumDefinition(variant) => variant.fmt(f),

            HIRGlobalVariable::Standard {
                _type,
                is_mutable,
                initializer,
                ..
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

impl Display for HIRDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.namespace.is_root() {
            write!(f, "[root] ")?;
        } else {
            write!(f, "[{}] ", self.namespace)?;
        }

        Display::fmt(&self.stmt, f)
    }
}

impl Display for HIRStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            HIRStmt::TypeDefinition {
                name,
                visibility,
                template_prototype,
                _type,
            } => {
                write!(f, "{visibility:?} ")?;
                if let Some(template) = template_prototype {
                    let params = template
                        .types
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "template <{params}> ")?;
                }

                match name {
                    Some(name) => write!(f, "type {name} = {_type};"),
                    None => write!(f, "type {_type};"),
                }
            }

            HIRStmt::FunctionDefinition {
                prototype,
                visibility,
                template_prototype,
                body,
            } => {
                write!(f, "{visibility:?} ")?;
                if let Some(template) = template_prototype {
                    let params = template
                        .types
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "template <{params}> ")?;
                }
                write!(f, "fn {prototype}")?;
                if let Some(body) = body {
                    write!(f, " {body}")
                } else {
                    write!(f, ";")
                }
            }

            HIRStmt::ComptimeFunctionDefinition {
                prototype,
                visibility,
                template_prototype,
                body,
            } => {
                write!(f, "{visibility:?} ")?;
                if let Some(template) = template_prototype {
                    let params = template
                        .types
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "template <{params}> ")?;
                }
                write!(f, "comptime fn {prototype} {body}")
            }

            HIRStmt::GlobalVariableDefinition {
                visibility,
                variable,
            } => write!(f, "{visibility:?} {variable};"),
        }
    }
}

impl Display for HIRExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        HIRExprFormatter::new(self, 0).fmt(f)
    }
}

impl<'a> Display for HIRExprFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.indent(f)?;
        match &self.expr.kind {
            HIRExprKind::Taken => writeln!(f, "Taken"),
            HIRExprKind::Block {
                exprs,
                creates_scope,
            } => {
                if *creates_scope {
                    writeln!(f, "Scoped Block {{ ")?;
                } else {
                    writeln!(f, "Block {{ ")?;
                }
                for stmt in exprs {
                    HIRExprFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
                self.indent(f)?;
                writeln!(f, "}}")
            }
            HIRExprKind::Defer { expr } => {
                writeln!(f, "Defer")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::StagedExpression { params, body } => {
                writeln!(
                    f,
                    "StagedExpression |{}|",
                    params
                        .iter()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
                HIRExprFormatter::new(body, self.depth + 1).fmt(f)
            }
            HIRExprKind::Then => writeln!(f, "Then"),
            HIRExprKind::Identifier {
                name,
                template_input,
                ..
            } => {
                if let Some(template_input) = template_input {
                    let arg_string = template_input
                        .params
                        .iter()
                        .map(|arg| format!("{}", arg))
                        .collect::<Vec<_>>()
                        .join(", ");

                    writeln!(f, "Identifier {}<{}>", name, arg_string)
                } else {
                    writeln!(f, "Identifier {}", name)
                }
            }
            HIRExprKind::VarDeclaration {
                name,
                _type,
                initial_value,
            } => {
                writeln!(f, "VarDeclaration {name}: {_type}")?;

                self.indent_plus_one(f)?;

                if let Some(init) = initial_value {
                    writeln!(f, "InitialValue:")?;
                    HIRExprFormatter::new(init, self.depth + 2).fmt(f)?;
                } else {
                    writeln!(f, "No initial value")?;
                }

                Ok(())
            }
            HIRExprKind::IntLiteral { magnitude, .. } => {
                writeln!(f, "IntLiteral {}", magnitude)
            }
            HIRExprKind::FloatLiteral { val, .. } => writeln!(f, "FloatLiteral {}", val),
            HIRExprKind::StringLiteral { val, .. } => {
                writeln!(f, "StringLiteral \"{}\"", val.escape_default())
            }
            HIRExprKind::Return { value } => {
                writeln!(f, "Return")?;
                if let Some(value) = value {
                    HIRExprFormatter::new(value, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            HIRExprKind::Yield { value } => {
                writeln!(f, "Yield")?;
                if let Some(value) = value {
                    HIRExprFormatter::new(value, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            HIRExprKind::Emit { expr } => {
                writeln!(f, "Emit")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::BinOp { lhs, rhs, op } => {
                writeln!(f, "BinOp {:?}", op)?;
                HIRExprFormatter::new(lhs, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(rhs, self.depth + 1).fmt(f)?;
                Ok(())
            }
            HIRExprKind::Unpack { expr, bindings } => {
                writeln!(f, "Unpack")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)?;
                for binding in bindings {
                    self.indent_plus_one(f)?;
                    writeln!(f, "{}: {}", binding.field, binding.binding)?;
                }
                Ok(())
            }
            HIRExprKind::InitializerList { indices } => {
                writeln!(f, "InitializerList")?;
                for index in indices {
                    HIRInitIndexFormatter::new(index, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            HIRExprKind::UnOp { operator, operand } => {
                writeln!(f, "UnOp {:?}", operator)?;
                HIRExprFormatter::new(operand, self.depth + 1).fmt(f)
            }
            HIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(f, "If")?;
                HIRExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(then_branch, self.depth + 1).fmt(f)?;
                if let Some(else_branch) = else_branch {
                    HIRExprFormatter::new(else_branch, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            HIRExprKind::Ternary {
                condition,
                then_branch,
                else_branch,
            } => {
                writeln!(f, "Ternary")?;
                HIRExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(then_branch, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(else_branch, self.depth + 1).fmt(f)?;
                Ok(())
            }
            HIRExprKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                writeln!(f, "For")?;
                HIRExprFormatter::new(init, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(increment, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(body, self.depth + 1).fmt(f)?;
                Ok(())
            }
            HIRExprKind::While {
                condition, body, ..
            } => {
                writeln!(f, "While")?;
                HIRExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                HIRExprFormatter::new(body, self.depth + 1).fmt(f)?;
                Ok(())
            }

            HIRExprKind::Unsafe { expr } => {
                writeln!(f, "Unsafe")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::Leak { expr } => {
                writeln!(f, "Leak")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::Adopt { expr } => {
                writeln!(f, "Adopt")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::SizeOfExpr { expr } => {
                writeln!(f, "SizeOf")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::SizeOfType { _type } => {
                writeln!(f, "SizeOfType ({_type})")
            }
            HIRExprKind::AlignOfExpr { expr } => {
                writeln!(f, "AlignOf")?;
                HIRExprFormatter::new(expr, self.depth + 1).fmt(f)
            }
            HIRExprKind::AlignOfType { _type } => {
                writeln!(f, "AlignOfType ({_type})")
            }
            HIRExprKind::Void => writeln!(f, "Unit"),
            HIRExprKind::Match {
                condition,
                arms,
                default,
            } => {
                writeln!(f, "Match")?;
                HIRExprFormatter::new(condition, self.depth + 1).fmt(f)?;
                for (pattern, arm_expr) in arms {
                    self.indent_plus_one(f)?;
                    writeln!(f, "Pattern: {}", pattern)?;
                    HIRExprFormatter::new(arm_expr, self.depth + 1).fmt(f)?;
                }
                if let Some(default_expr) = default {
                    self.indent_plus_one(f)?;
                    writeln!(f, "Default:")?;
                    HIRExprFormatter::new(default_expr, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            HIRExprKind::Switch {
                condition,
                block,
                cases,
                default_case,
            } => {
                writeln!(f, "Switch")?;
                HIRExprFormatter::new(condition, self.depth + 1).fmt(f)?;
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
                    HIRExprFormatter::new(stmt, self.depth + 1).fmt(f)?;
                }
                Ok(())
            }
            HIRExprKind::Break => writeln!(f, "Break"),
            HIRExprKind::Continue => writeln!(f, "Continue"),
        }
    }
}

impl Display for HIRBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            HIRBinOp::Add => write!(f, "+"),
            HIRBinOp::Subtract => write!(f, "-"),
            HIRBinOp::Multiply => write!(f, "*"),
            HIRBinOp::Divide => write!(f, "/"),
            HIRBinOp::Modulus => write!(f, "%"),
            HIRBinOp::Equal => write!(f, "=="),
            HIRBinOp::NotEqual => write!(f, "!="),
            HIRBinOp::Less => write!(f, "<"),
            HIRBinOp::LessEqual => write!(f, "<="),
            HIRBinOp::Greater => write!(f, ">"),
            HIRBinOp::GreaterEqual => write!(f, ">="),
            HIRBinOp::Access => write!(f, "."),
            HIRBinOp::MethodCall => write!(f, "()"),
            HIRBinOp::ArrayIndex => write!(f, "[]"),
            HIRBinOp::Comma => write!(f, ","),
            HIRBinOp::Assign(add) => {
                if let Some(add) = add {
                    write!(f, "{} =", add)
                } else {
                    write!(f, "=")
                }
            }

            HIRBinOp::LAnd => write!(f, "&&"),
            HIRBinOp::LOr => write!(f, "||"),
            HIRBinOp::BitAnd => write!(f, "&"),
            HIRBinOp::BitOr => write!(f, "|"),
            HIRBinOp::BitXor => write!(f, "^"),
            HIRBinOp::LShift => write!(f, "<<"),
            HIRBinOp::RShift => write!(f, ">>"),
            HIRBinOp::Pipe => write!(f, "|>"),
            HIRBinOp::BackwardPipe => write!(f, "<|"),
        }
    }
}

struct HIRInitIndexFormatter<'a> {
    index: &'a HIRInitIndex,
    depth: usize,
}

impl<'a> HIRInitIndexFormatter<'a> {
    fn new(index: &'a HIRInitIndex, depth: usize) -> Self {
        Self { index, depth }
    }

    fn indent(&self, f: &mut Formatter<'_>) -> Result {
        for _ in 0..self.depth {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

impl<'a> Display for HIRInitIndexFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.indent(f)?;
        if let Some(name) = &self.index.name {
            writeln!(f, ".{name} = ")?;
        } else {
            writeln!(f, "[] = ")?;
        }
        HIRExprFormatter::new(&self.index.value, self.depth + 1).fmt(f)?;
        Ok(())
    }
}

impl Display for HIRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for HIRTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HIRTypeKind::Identifier {
                name,
                template_input,
                ..
            } => {
                if let Some(input) = template_input {
                    write!(f, "{name}{input}")
                } else {
                    write!(f, "{name}")
                }
            }
            HIRTypeKind::ExplicitSizedArray(inner, size) => write!(f, "[{inner}; {size}]"),
            HIRTypeKind::ImplicitSizedArray(inner) => write!(f, "[{inner}]"),
            HIRTypeKind::MemoryReference { inner_type } => write!(f, "&{inner_type}"),
            HIRTypeKind::PointerTo { inner_type } => {
                write!(f, "*{}", inner_type)
            }

            HIRTypeKind::Structured {
                name,
                attributes,
                fields,
            } => {
                let fields_str = fields
                    .iter()
                    .map(|field| match field {
                        HIRField::Standard { _type, .. } => format!("{_type}"),
                        HIRField::Bitfield {
                            name,
                            integer_type,
                            width,
                        } => format!(
                            "{}{} : {}",
                            integer_type,
                            name.as_deref()
                                .map(|name| format!(" {name}"))
                                .unwrap_or_default(),
                            width
                        ),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut attrs = Vec::new();

                match attributes.semantics {
                    HIRMoveSemantics::POD => {}
                    HIRMoveSemantics::Nocopy => attrs.push("@nocopy"),
                    HIRMoveSemantics::Nodrop => attrs.push("@nodrop"),
                }
                if attributes.unsafe_move {
                    attrs.push("@unsafe_move");
                }

                let attr_str = if attrs.is_empty() {
                    String::new()
                } else {
                    format!(" : {}", attrs.join(", "))
                };
                write!(
                    f,
                    "struct {}{} {{ {} }}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or("__anonymous__"),
                    attr_str,
                    fields_str
                )
            }
            HIRTypeKind::Union { name, fields } => {
                let fields_str = fields
                    .iter()
                    .map(|field| match field {
                        HIRField::Standard { _type, .. } => format!("{_type}"),
                        HIRField::Bitfield {
                            name,
                            integer_type,
                            width,
                        } => format!(
                            "{}{} : {}",
                            integer_type,
                            name.as_deref()
                                .map(|name| format!(" {name}"))
                                .unwrap_or_default(),
                            width
                        ),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(
                    f,
                    "union {} {{ {} }}",
                    name.as_ref().map(|n| n.as_str()).unwrap_or(""),
                    fields_str
                )
            }
            HIRTypeKind::TaggedUnion {
                name,
                attributes,
                variants,
            } => {
                let variants_str = variants
                    .iter()
                    .map(|field| match field {
                        HIRField::Standard { name, _type } => {
                            format!("{name}: {_type}")
                        }
                        HIRField::Bitfield { .. } => "<invalid bitfield variant>".to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut attrs = Vec::new();
                match attributes.semantics {
                    HIRMoveSemantics::POD => {}
                    HIRMoveSemantics::Nocopy => attrs.push("@nocopy"),
                    HIRMoveSemantics::Nodrop => attrs.push("@nodrop"),
                }
                if attributes.unsafe_move {
                    attrs.push("@unsafe_move");
                }
                write!(
                    f,
                    "union class {name}{} {{ {variants_str} }}",
                    if attrs.is_empty() {
                        "".to_string()
                    } else {
                        format!(" : {}", attrs.join(", "))
                    }
                )
            }
            HIRTypeKind::FunctionPointer { prototype } => {
                write!(f, "FunctionPointer({prototype})")
            }
        }
    }
}

impl Display for HIRFunctionPrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut params = Vec::new();

        params.extend(self.params.iter().map(|param| {
            format!(
                "{}: {}",
                param.name.as_ref().unwrap_or(&CXIdent::new("_")),
                param._type
            )
        }));

        let params_str = params.join(", ");
        write!(f, "{} :: {}({})", self.return_type, self.kind, params_str)
    }
}

impl Display for HIRComptimeValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.expr {
            write!(f, "expr ")?;
            if !self.params.is_empty() {
                write!(f, "(")?;
                for (index, param) in self.params.iter().enumerate() {
                    if index != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{param}")?;
                }
                write!(f, ") ")?;
            }
        }

        write!(f, "{}", self._type)
    }
}

impl Display for HIRComptimeFnPrototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut params = Vec::new();

        params.extend(self.params.iter().map(|param| {
            format!(
                "{}: {}",
                param.name.as_ref().unwrap_or(&CXIdent::new("_")),
                param.value_type
            )
        }));

        let params_str = params.join(", ");
        write!(f, "{} :: {}({})", self.return_type, self.kind, params_str)
    }
}

impl Display for HIRTemplateInput {
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

impl Display for HIRFunctionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HIRFunctionKind::Standard(name) => write!(f, "{name}"),
            HIRFunctionKind::AssociatedFunction { namespace, name } => {
                write!(f, "{namespace}::{name}")
            }
        }
    }
}

impl Display for HIRPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            HIRPattern::Binding(name) => write!(f, "{name}"),
            HIRPattern::Integer(value) => write!(f, "{value}"),
            HIRPattern::Float(value) => write!(f, "{value}"),
            HIRPattern::Variant {
                constructor,
                template_input,
                inner,
            } => {
                write!(f, "{constructor}")?;
                if let Some(input) = template_input {
                    write!(
                        f,
                        "<{}>",
                        input
                            .params
                            .iter()
                            .map(|param| param.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
                if let Some(inner) = inner {
                    write!(f, "({inner})")?;
                }
                Ok(())
            }
        }
    }
}
