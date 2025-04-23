use std::fmt::{Display, Formatter};
use crate::{fwrite, fwriteln};
use crate::parse::format::{dedent, indent};
use crate::parse::pass_molded::{CXBinOp, CXExpr, CXGlobalStmt, CXParameter, CXAST};

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
            CXGlobalStmt::FunctionDefinition { name, return_type, parameters, body } => {
                writeln!(f, "fn {}({}) -> {:?} {{", name, parameters.iter().map(|p| format!("{}", p)).collect::<Vec<_>>().join(", "), return_type)?;
                indent();
                write!(f, "\t{}", body)?;
                dedent();
                writeln!(f, "")?;
                writeln!(f, "}}")
            },
        }
    }
}

impl Display for CXExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXExpr::Block { exprs } => {
                for stmt in exprs.iter().take(exprs.len() - 1) {
                    fwriteln!(f, "{};", stmt)?;
                }
                let last_stmt = exprs.last().unwrap();
                fwrite!(f, "{};", last_stmt);
                Ok(())
            },

            CXExpr::FunctionCall { callee, args, .. } => {
                let arg_strs = args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<_>>()
                    .join(", ");

                fwrite!(f, "{}({})", callee.as_ref(), arg_strs)
            },

            CXExpr::Identifier(ident) => fwrite!(f, "{}", ident),
            CXExpr::VarReference(ident) => fwrite!(f, "{}", ident),
            CXExpr::VarInitialization { type_, name } => {
                fwrite!(f, "{}: {:?}", name, type_)
            },

            CXExpr::VarDeclaration { name, type_, initializer } => {
                fwrite!(f, "let {}: {:?}", name, type_)?;

                if let Some(initializer) = initializer {
                    fwrite!(f, " = {}", initializer)?;
                }

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
                fwrite!(f, "({}) {} ({})", lhs, op, rhs)
            },

            CXExpr::Assignment { lhs, rhs, op } => {
                if let Some(op) = op {
                    fwrite!(f, "{} {}= {}", lhs, op, rhs)
                } else {
                    fwrite!(f, "{} = {}", lhs, rhs)
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

            _ => todo!(),
        }
    }
}