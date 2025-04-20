use std::fmt::{Display, Formatter};
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt, UVAST};

impl Display for UVAST {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.stmts {
            writeln!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

impl Display for UVGlobalStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UVGlobalStmt::Import(path) => writeln!(f, "import \"{}\";", path),
            UVGlobalStmt::BodiedExpression { header, body } => {
                writeln!(f, "{} {{", header)?;
                write!(f, "\t{}", body)?;
                writeln!(f, "}}")
            },
            UVGlobalStmt::SingleExpression { expression } => {
                write!(f, "{}", expression)
            },

            _ => write!(f, "{:?}", self),
        }
    }
}

impl Display for UVExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UVExpr::Identifier(id) => write!(f, "{}", id),
            UVExpr::Compound { left, right } => write!(f, "{} {}", left, right),

            UVExpr::IntLiteral(i) => write!(f, "{}", i),
            UVExpr::FloatLiteral(flt) => write!(f, "{}", flt),
            UVExpr::StringLiteral(s) => write!(f, "\"{}\"", s),

            UVExpr::Return { value } => {
                if let Some(value) = value {
                    write!(f, "return {}", value)
                } else {
                    write!(f, "return")
                }
            }

            UVExpr::UnOp { operator, operand } => writeln!(f, "(`{:?}` {})", operator, operand),
            UVExpr::Parenthesized(expr) => {
                if let Some(expr) = expr {
                    write!(f, "({})", expr)
                } else {
                    write!(f, "()")
                }
            }
            UVExpr::If { condition, then_branch, else_branch } => {
                writeln!(f, "if {} {{\n", condition)?;
                writeln!(f, "\t{}", then_branch)?;

                if let Some(else_branch) = else_branch {
                    writeln!(f, "}} else {{\n")?;
                    writeln!(f, "\t{}", else_branch)?;
                    write!(f, "}}")
                } else {
                    write!(f, "")
                }?;

                writeln!(f, "}}")
            }
            UVExpr::While { condition, body } => {
                writeln!(f, "while {} {{", condition)?;
                write!(f, "    {}", body)?;
                write!(f, "}}")
            }
            UVExpr::For { init, condition, increment, body } => {
                write!(f, "for (")?;
                if let Some(init) = init {
                    write!(f, "{}; ", init)?;
                }
                if let Some(condition) = condition {
                    write!(f, "{}; ", condition)?;
                }
                if let Some(increment) = increment {
                    write!(f, "{}", increment)?;
                }
                write!(f, ") {{\n")?;
                writeln!(f, "\t{}", body)?;
                write!(f, "}}")
            },
            UVExpr::Complex { operator_stack, expression_stack } => {
                let op_str = operator_stack.iter()
                    .map(|op| format!("{:?}", op))
                    .collect::<Vec<_>>()
                    .join(", ");
                let expr_str = expression_stack.iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<Vec<_>>()
                    .join(", ");

                write!(f, "`{}` | `{}`", op_str, expr_str)
            },
            UVExpr::ExprChain(list) => {
                for expr in list {
                    writeln!(f, "{};", expr)?;
                }

                Ok(())
            }
        }
    }
}