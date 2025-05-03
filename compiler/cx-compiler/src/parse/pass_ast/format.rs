use std::fmt::{Display, Formatter};
use crate::{fwrite, fwriteln};
use crate::parse::format::{dedent, indent};
use crate::parse::pass_ast::{CXBinOp, CXExpr, CXFunctionPrototype, CXGlobalStmt, CXInitIndex, CXParameter, CXUnOp, CXAST};

impl Display for CXAST<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.global_stmts {
            writeln!(f, "{}\n", stmt)?;
        }

        Ok(())
    }
}

impl Display for CXGlobalStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXGlobalStmt::GlobalVariable { name, type_, .. } => {
                writeln!(f, "{}: {:?}", name, type_)
            },
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                write!(f, "{}", prototype)?;
                write!(f, "{}", body)?;
                Ok(())
            },
            CXGlobalStmt::FunctionForward { prototype } => {
                writeln!(f, "{};", prototype)
            },
        }
    }
}

impl Display for CXFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn {}({}) -> {:?}",
                 self.name,
                 self.parameters.iter().map(|p| format!("{}", p)).collect::<Vec<_>>().join(", "),
                 self.return_type
        )
    }
}

impl Display for CXExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXExpr::Block { exprs, .. } => {
                indent();
                fwriteln!(f, "{{")?;
                for stmt in exprs.iter().take(exprs.len() - 1) {
                    fwriteln!(f, "{};", stmt)?;
                }
                dedent();
                if let Some(last_stmt) = exprs.last() {
                    fwriteln!(f, "{}", last_stmt)?;
                }
                fwrite!(f, "}}")?;
                Ok(())
            },

            CXExpr::IndirectFunctionCall { callee, args, .. } => {
                let arg_strs = args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");

                fwrite!(f, "{}({})", callee.as_ref(), arg_strs)
            },
            CXExpr::DirectFunctionCall { name, args } => {
                let arg_strs = args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");

                fwrite!(f, "{}({})", name, arg_strs)
            },

            CXExpr::Identifier(ident) => fwrite!(f, "{}", ident),
            CXExpr::VarDeclaration { name, type_ } => {
                fwrite!(f, "let {}: {}", name, type_)?;

                Ok(())
            },

            CXExpr::IntLiteral { val, .. } => fwrite!(f, "{}", val),
            CXExpr::FloatLiteral { val, .. } => fwrite!(f, "{}", val),
            CXExpr::StringLiteral { val, .. } => fwrite!(f, "\"{}\"", val),

            CXExpr::Return { value } => {
                fwrite!(f, "return")?;
                if let Some(value) = value {
                    fwrite!(f, " {}", value)?;
                }
                Ok(())
            },

            CXExpr::BinOp { lhs, rhs, op } => {
                fwrite!(f, "({} {} {})", lhs, op, rhs)
            },

            CXExpr::ImplicitCast { expr, to_type, .. } => {
                fwrite!(f, "{}<{}>", expr, to_type)
            },

            CXExpr::InitializerList { indices } => {
                indent();
                fwriteln!(f, "{{")?;
                for (i, index) in indices.iter().enumerate() {
                    fwrite!(f, "{}", index)?;
                    if i != indices.len() - 1 {
                        fwrite!(f, ", ")?;
                    } else {
                        dedent();
                    }
                    fwriteln!(f, "")?;
                }
                fwrite!(f, "}}")
            },

            CXExpr::UnOp { operator, operand } => {
                match operator {
                    CXUnOp::Negative => fwrite!(f, "-{}", operand),
                    CXUnOp::LNot => fwrite!(f, "!{}", operand),
                    CXUnOp::BNot => fwrite!(f, "~{}", operand),
                    CXUnOp::InitializerIndex => fwrite!(f, ".{}", operand),
                    CXUnOp::Dereference => fwrite!(f, "*{}", operand),
                    CXUnOp::ArrayIndex => fwrite!(f, "{}[]", operand),
                    CXUnOp::AddressOf => fwrite!(f, "&{}", operand),
                }
            }

            _ => fwrite!(f, "{:?}", self)
        }
    }
}

impl Display for CXParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: {:?}", name, self.type_)
        } else {
            write!(f, "{:?}", self.type_)
        }
    }
}

impl Display for CXBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXBinOp::Add => fwrite!(f, "+"),
            CXBinOp::Subtract => fwrite!(f, "-"),
            CXBinOp::Multiply => fwrite!(f, "*"),
            CXBinOp::Divide => fwrite!(f, "/"),
            CXBinOp::Equal => fwrite!(f, "=="),
            CXBinOp::NotEqual => fwrite!(f, "!="),

            CXBinOp::Access => fwrite!(f, "."),

            _ => fwrite!(f, "{:?}", self)
        }
    }
}

impl Display for CXInitIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXInitIndex::Unnamed(index) => {
                fwrite!(f, "{}", index)
            },
            CXInitIndex::Named(name, index) => {
                fwrite!(f, ".{} = {}", name, index.as_ref())
            },
        }
    }
}