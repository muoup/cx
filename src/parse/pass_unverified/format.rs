use std::fmt::{Display, Formatter};
use crate::{fwrite, fwriteln};
use crate::parse::format::{dedent, indent};
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt, UVAST};
use crate::parse::pass_unverified::expression::requires_semicolon;
impl Display for UVAST {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.stmts {
            fwriteln!(f, "{}\n", stmt)?;
        }

        Ok(())
    }
}

impl Display for UVGlobalStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UVGlobalStmt::Import(path) => fwriteln!(f, "import \"{}\";", path),
            UVGlobalStmt::BodiedExpression { header, body } => {
                indent();
                fwriteln!(f, "{} {{", header)?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "");
                fwriteln!(f, "}}")
            },
            UVGlobalStmt::SingleExpression { expression } => {
                fwrite!(f, "{};", expression)
            },
            UVGlobalStmt::HandledInternally => Ok(()),
            UVGlobalStmt::TypeDeclaration { name, type_ } => {
                fwrite!(f, "type {} = {};", name, type_)
            },

            _ => fwrite!(f, "{:?}", self),
        }
    }
}

impl Display for UVExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UVExpr::Identifier(id) => fwrite!(f, "{}", id),
            UVExpr::Compound { left, right } => fwrite!(f, "{} {}", left, right),

            UVExpr::IntLiteral(i) => fwrite!(f, "{}", i),
            UVExpr::FloatLiteral(flt) => fwrite!(f, "{}", flt),
            UVExpr::StringLiteral(s) => fwrite!(f, "\"{}\"", s),

            UVExpr::Return { value } => {
                if let Some(value) = value {
                    fwrite!(f, "return {}", value)
                } else {
                    fwrite!(f, "return")
                }
            }

            UVExpr::UnOp { operator, operand } => writeln!(f, "(`{:?}` {})", operator, operand),
            UVExpr::Parenthesized(expr) => {
                if let Some(expr) = expr {
                    fwrite!(f, "({})", expr)
                } else {
                    fwrite!(f, "()")
                }
            }
            UVExpr::If { condition, then_branch, else_branch } => {
                fwriteln!(f, "if {} {{\n", condition)?;
                indent();
                fwriteln!(f, "{}", then_branch)?;
                dedent();

                if let Some(else_branch) = else_branch {
                    indent();
                    fwriteln!(f, "}} else {{\n")?;
                    fwriteln!(f, "{}", else_branch)?;
                    dedent();
                    fwrite!(f, "}}")
                } else {
                    fwrite!(f, "")
                }?;

                fwriteln!(f, "}}")
            }
            UVExpr::While { condition, body } => {
                indent();
                fwriteln!(f, "while {} {{", condition)?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "");
                fwrite!(f, "}}")
            }
            UVExpr::For { init, condition, increment, body } => {
                fwrite!(f, "for (")?;
                if let Some(init) = init {
                    fwrite!(f, "{}; ", init)?;
                }
                if let Some(condition) = condition {
                    fwrite!(f, "{}; ", condition)?;
                }
                if let Some(increment) = increment {
                    fwrite!(f, "{}", increment)?;
                }
                fwrite!(f, ") {{\n")?;
                indent();
                fwriteln!(f, "\t{}", body)?;
                dedent();
                fwrite!(f, "}}")
            },
            UVExpr::Complex { op_stack: operator_stack, expr_stack: expression_stack } => {
                let op_str = operator_stack.iter()
                    .map(|op| format!("{:?}", op))
                    .collect::<Vec<_>>()
                    .join(", ");
                let expr_str = expression_stack.iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<Vec<_>>()
                    .join(", ");

                fwrite!(f, "`{}` | `{}`", op_str, expr_str)
            },
            UVExpr::ExprChain(list) => {
                for (i, expr) in list.iter().enumerate() {
                    fwrite!(f, "{}", expr)?;

                    if requires_semicolon(expr) {
                        fwrite!(f, ";")?;
                    }

                    if i != list.len() - 1 {
                        fwriteln!(f, "")?;
                    }
                }

                Ok(())
            },

            UVExpr::Braced(expr) => {
                indent();
                fwriteln!(f, "{{")?;
                fwrite!(f, "{}", expr)?;
                dedent();
                fwriteln!(f, "}}")
            }
        }
    }
}