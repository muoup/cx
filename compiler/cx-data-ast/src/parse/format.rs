use std::env::args;
use std::fmt::{Display, Formatter};
use cx_util::format::{dedent, indent};
use cx_util::{fwrite, fwriteln};
use crate::parse::ast::{CXBinOp, CXExpr, CXFunctionPrototype, CXGlobalStmt, CXInitIndex, CXParameter, CXUnOp, CXAST};
use crate::parse::value_type::{CXTypeUnion, CXValType};

impl Display for CXAST {
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
                writeln!(f, "{}: {}", name, type_)
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
        writeln!(f, "fn {}({}) -> {}",
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

            CXExpr::Identifier(ident) => fwrite!(f, "{}", ident),
            CXExpr::VarDeclaration { name, type_ } => {
                fwrite!(f, "let {}: {}", name, type_)?;

                Ok(())
            },

            CXExpr::Identifier(ident) => {
                fwrite!(f, "{}", ident)
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

            CXExpr::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
                fwrite!(f, "{}({})", lhs, rhs)
            },

            CXExpr::BinOp { lhs, rhs, op } => {
                fwrite!(f, "({} {} {})", lhs, op, rhs)
            },

            CXExpr::ImplicitCast { expr, to_type, .. } => {
                fwrite!(f, "{}#to({})", expr, to_type)
            },

            CXExpr::ImplicitLoad { expr, loaded_type  } => {
                fwrite!(f, "{}#load({})", expr, loaded_type)
            },

            CXExpr::GetFunctionAddr { func_name, func_sig } => {
                fwrite!(f, "{}#fn_addr({})", func_name, func_sig)
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

                    CXUnOp::PreIncrement(1) => fwrite!(f, "++{}", operand),
                    CXUnOp::PostIncrement(1) => fwrite!(f, "{}++", operand),
                    CXUnOp::PreIncrement(-1) => fwrite!(f, "--{}", operand),
                    CXUnOp::PostIncrement(-1) => fwrite!(f, "{}--", operand),

                    CXUnOp::PreIncrement(_) |
                    CXUnOp::PostIncrement(_) => panic!("Invalid increment operator"),
                }
            },

            CXExpr::If { condition, then_branch, else_branch } => {
                fwrite!(f, "if {} {{", condition)?;
                indent();
                fwriteln!(f, "{}", then_branch)?;
                dedent();
                if let Some(else_branch) = else_branch {
                    fwrite!(f, "}} else {{")?;
                    indent();
                    fwriteln!(f, "{}", else_branch)?;
                    dedent();
                }
                fwrite!(f, "}}")
            },

            CXExpr::For { init, condition, increment, body } => {
                fwrite!(f, "for ({}; {}; {})", init, condition, increment)?;
                indent();
                fwriteln!(f, "{}", body)?;
                dedent();
                Ok(())
            },

            _ => fwrite!(f, "{:?}", self)
        }
    }
}

impl Display for CXValType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.internal_type)
    }
}

impl Display for CXTypeUnion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXTypeUnion::Integer { bytes, signed } => {
                let signed_str = if *signed { "i" } else { "u" };
                let signed_bytes = *bytes * 8;
                write!(f, "{}i{}", signed_str, signed_bytes)
            },
            CXTypeUnion::Float { bytes } => {
                let float_bytes = *bytes * 8;
                write!(f, "f{}", float_bytes)
            },
            CXTypeUnion::Structured { fields, name } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{}: {}", name, type_))
                    .collect::<Vec<_>>()
                    .join(", ");
                let name_str = if let Some(name) = name {
                    format!("{} ", name)
                } else {
                    "".to_string()
                };

                write!(f, "struct {} {{ {} }}", name_str, field_strs)
            },
            CXTypeUnion::Unit => write!(f, "()"),
            CXTypeUnion::PointerTo(inner) => {
                write!(f, "*{}", inner)
            },
            CXTypeUnion::Array { size, _type } => {
                write!(f, "[{}; {}]", size, _type)
            },
            CXTypeUnion::Opaque { name, size } => {
                write!(f, "OPAQUE_{}(\"{}\")", size, name)
            },
            CXTypeUnion::Identifier(name) => {
                write!(f, "{}", name)
            },
            CXTypeUnion::Function { return_type, args } => {
                let arg_strs = args.iter()
                    .map(|type_| format!("{}", type_))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({}) -> {}", arg_strs, return_type)
            },
            CXTypeUnion::MemoryReference(inner) => {
                write!(f, "mem({})", inner)
            },
        }
    }
}


impl Display for CXParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}: {}", name, self.type_)
        } else {
            write!(f, "{}", self.type_)
        }
    }
}

impl Display for CXBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXBinOp::Add        => fwrite!(f, "+"),
            CXBinOp::Subtract   => fwrite!(f, "-"),
            CXBinOp::Multiply   => fwrite!(f, "*"),
            CXBinOp::Divide     => fwrite!(f, "/"),
            CXBinOp::Modulus    => fwrite!(f, "%"),

            CXBinOp::Equal      => fwrite!(f, "=="),
            CXBinOp::NotEqual   => fwrite!(f, "!="),

            CXBinOp::Access     => fwrite!(f, "."),

            CXBinOp::Assign(add) => {
                if let Some(add) = add {
                    fwrite!(f, "{}=", add)
                } else {
                    fwrite!(f, "=")
                }
            }

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