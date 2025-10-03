use crate::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXGlobalStmt, CXInitIndex, CXUnOp, CXAST};
use cx_util::format::{dedent, indent};
use cx_util::{fwrite, fwriteln};
use std::fmt::{Display, Formatter};

impl Display for CXAST {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for stmt in &self.global_stmts {
            writeln!(f, "{stmt}\n")?;
        }

        Ok(())
    }
}

impl Display for CXGlobalStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXGlobalStmt::TypeDecl { name, type_ } => {
                if let Some(name) = name {
                    fwriteln!(f, "type {} = {}", name, type_)
                } else {
                    fwriteln!(f, "type {}", type_)
                }
            }

            CXGlobalStmt::GlobalVariable { name, type_, .. } => {
                fwriteln!(f, "{}: {}", name, type_)
            }

            CXGlobalStmt::FunctionPrototype { prototype } => {
                fwriteln!(f, "{};", prototype)
            }

            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                indent();
                fwriteln!(f, "{} {{", prototype)?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")?;
                Ok(())
            }

            CXGlobalStmt::DestructorDefinition { _type, body } => {
                indent();
                fwriteln!(f, "destructor for {} {{", _type)?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")?;
                Ok(())
            }

            CXGlobalStmt::TemplatedFunction { prototype, body } => {
                indent();
                fwriteln!(f, "template {prototype} {{")?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")?;
                Ok(())
            }
        }
    }
}

impl Display for CXExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for CXExprKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXExprKind::Block { exprs, .. } => {
                for (i, stmt) in exprs.iter().enumerate() {
                    fwrite!(f, "{}", stmt)?;

                    if i != exprs.len() - 1 {
                        fwriteln!(f, "")?;
                    }
                }
                Ok(())
            }

            CXExprKind::Identifier(ident) => fwrite!(f, "{}", ident),
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

                fwrite!(f, "{}<{}>", fn_name, arg_string)
            }

            CXExprKind::VarDeclaration { name, type_ } => {
                fwrite!(f, "let {}: {}", name, type_)?;

                Ok(())
            }

            CXExprKind::IntLiteral { val, .. } => fwrite!(f, "{}", val),
            CXExprKind::FloatLiteral { val, .. } => fwrite!(f, "{}", val),
            CXExprKind::StringLiteral { val, .. } => {
                // Sanitize input (i.e. convert \n to \\n)
                let sanitized = val
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\n")
                    .replace('\r', "\\r")
                    .replace('\t', "\\t");
                fwrite!(f, "\"{}\"", sanitized)
            }

            CXExprKind::Return { value } => {
                fwrite!(f, "return")?;
                if let Some(value) = value {
                    fwrite!(f, " {}", value)?;
                }
                Ok(())
            }

            CXExprKind::BinOp {
                lhs,
                rhs,
                op: CXBinOp::MethodCall,
            } => {
                fwrite!(f, "{}({})", lhs, rhs)
            }

            CXExprKind::BinOp {
                lhs,
                rhs,
                op: CXBinOp::ArrayIndex,
            } => {
                fwrite!(f, "{}[{}]", lhs, rhs)
            }

            CXExprKind::BinOp {
                lhs,
                rhs,
                op: CXBinOp::Access,
            } => {
                fwrite!(f, "{}.{}", lhs, rhs)
            }

            CXExprKind::BinOp { lhs, rhs, op } => {
                fwrite!(f, "{} {} {}", lhs, op, rhs)
            }

            CXExprKind::Move { expr } => {
                fwrite!(f, "(move {})", expr)
            }

            CXExprKind::InitializerList { indices } => {
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
            }

            CXExprKind::UnOp { operator, operand } => match operator {
                CXUnOp::Negative => fwrite!(f, "-({})", operand),
                CXUnOp::LNot => fwrite!(f, "!({})", operand),
                CXUnOp::BNot => fwrite!(f, "~({})", operand),
                CXUnOp::Dereference => fwrite!(f, "*({})", operand),
                CXUnOp::AddressOf => fwrite!(f, "&({})", operand),

                CXUnOp::PreIncrement(1) => fwrite!(f, "++{}", operand),
                CXUnOp::PostIncrement(1) => fwrite!(f, "{}++", operand),
                CXUnOp::PreIncrement(-1) => fwrite!(f, "--{}", operand),
                CXUnOp::PostIncrement(-1) => fwrite!(f, "{}--", operand),

                CXUnOp::ExplicitCast(to_type) => fwrite!(f, "({to_type}) ({operand})"),

                CXUnOp::PreIncrement(_) | CXUnOp::PostIncrement(_) => {
                    panic!("Invalid increment operator")
                }
            },

            CXExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                indent();
                fwriteln!(f, "if {} {{", condition)?;
                fwrite!(f, "{}", then_branch)?;
                dedent();
                fwriteln!(f, "")?;
                if let Some(else_branch) = else_branch {
                    indent();
                    fwriteln!(f, "}} else {{")?;
                    fwrite!(f, "{}", else_branch)?;
                    dedent();
                    fwriteln!(f, "")?;
                }
                fwrite!(f, "}}")
            }

            CXExprKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                indent();
                fwriteln!(f, "for ({}; {}; {}) {{", init, condition, increment)?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")
            }

            CXExprKind::While {
                condition, body, ..
            } => {
                indent();
                fwriteln!(f, "while ({condition}) {{")?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")
            }

            CXExprKind::Defer { expr } => {
                fwrite!(f, "defer {}", expr)
            }

            CXExprKind::New { _type } => {
                fwrite!(f, "new {}", _type)
            }

            CXExprKind::SizeOf { expr } => {
                fwrite!(f, "sizeof({})", expr)
            }

            _ => fwrite!(f, "{:?}", self),
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
            CXBinOp::Modulus => fwrite!(f, "%"),

            CXBinOp::Equal => fwrite!(f, "=="),
            CXBinOp::NotEqual => fwrite!(f, "!="),

            CXBinOp::Less => fwrite!(f, "<"),
            CXBinOp::LessEqual => fwrite!(f, "<="),
            CXBinOp::Greater => fwrite!(f, ">"),
            CXBinOp::GreaterEqual => fwrite!(f, ">="),

            CXBinOp::Access => fwrite!(f, "."),

            CXBinOp::Comma => fwrite!(f, ","),

            CXBinOp::Assign(add) => {
                if let Some(add) = add {
                    fwrite!(f, "{}=", add)
                } else {
                    fwrite!(f, "=")
                }
            }

            CXBinOp::Is => fwrite!(f, "is"),

            _ => fwrite!(f, "{:?}", self),
        }
    }
}

impl Display for CXInitIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: {}", name, self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}
