use std::fmt::{Display, Formatter};
use crate::{fwrite, fwriteln};
use crate::parse::format::{dedent, indent};
use crate::parse::pass_molded::{CXExpr, CXGlobalStmt, CXParameter, CXAST};

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

                write!(f, "{}({})", callee.as_ref(), arg_strs)
            },

            CXExpr::Identifier(ident) => write!(f, "{}", ident),

            CXExpr::IntLiteral { val, .. } => write!(f, "{}", val),
            CXExpr::FloatLiteral { val, .. } => write!(f, "{}", val),
            CXExpr::StringLiteral { val, .. } => write!(f, "\"{}\"", val),

            CXExpr::Return { value } => {
                write!(f, "return")?;
                if let Some(value) = value {
                    write!(f, " {}", value)?;
                }
                Ok(())
            },

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