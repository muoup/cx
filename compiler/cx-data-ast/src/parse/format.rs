use crate::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXFunctionPrototype, CXGlobalStmt, CXInitIndex, CXParameter, CXUnOp, CXAST};
use crate::parse::value_type::{CXTypeKind, CXType};
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
            CXGlobalStmt::GlobalVariable { name, type_, .. } => {
                fwriteln!(f, "{}: {}", name, type_)
            },
            CXGlobalStmt::FunctionDefinition { prototype, body } => {
                indent();
                fwriteln!(f, "{} {{", prototype)?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")?;
                Ok(())
            },
        }
    }
}

impl Display for CXFunctionPrototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}({}) -> {}",
               self.name,
               self.params.iter().map(|p| format!("{p}")).collect::<Vec<_>>().join(", "),
               self.return_type
        )
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
            },

            CXExprKind::Identifier(ident) => fwrite!(f, "{}", ident),
            CXExprKind::VarDeclaration { name, type_ } => {
                fwrite!(f, "let {}: {}", name, type_)?;

                Ok(())
            },

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
            },

            CXExprKind::Return { value } => {
                fwrite!(f, "return")?;
                if let Some(value) = value {
                    fwrite!(f, " {}", value)?;
                }
                Ok(())
            },

            CXExprKind::BinOp { lhs, rhs, op: CXBinOp::MethodCall } => {
                fwrite!(f, "{}({})", lhs, rhs)
            },

            CXExprKind::BinOp { lhs, rhs, op: CXBinOp::ArrayIndex } => {
                fwrite!(f, "{}[{}]", lhs, rhs)
            },

            CXExprKind::BinOp { lhs, rhs, op } => {
                fwrite!(f, "{} {} {}", lhs, op, rhs)
            },

            CXExprKind::ImplicitCast { expr, to_type, .. } => {
                fwrite!(f, "{}#to({})", expr, to_type)
            },

            CXExprKind::ImplicitLoad { expr, loaded_type  } => {
                fwrite!(f, "{}#load({})", expr, loaded_type)
            },

            CXExprKind::GetFunctionAddr { func_name, func_sig } => {
                fwrite!(f, "{}#fn_addr({})", func_name, func_sig)
            },

            CXExprKind::Move { expr } => {
                fwrite!(f, "(move {})", expr)
            },

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
            },

            CXExprKind::UnOp { operator, operand } => {
                match operator {
                    CXUnOp::Negative => fwrite!(f, "-({})", operand),
                    CXUnOp::LNot => fwrite!(f, "!({})", operand),
                    CXUnOp::BNot => fwrite!(f, "~({})", operand),
                    CXUnOp::InitializerIndex => fwrite!(f, ".{}", operand),
                    CXUnOp::Dereference => fwrite!(f, "*({})", operand),
                    CXUnOp::ArrayIndex => fwrite!(f, "({})[]", operand),
                    CXUnOp::AddressOf => fwrite!(f, "&({})", operand),

                    CXUnOp::PreIncrement(1) => fwrite!(f, "++{}", operand),
                    CXUnOp::PostIncrement(1) => fwrite!(f, "{}++", operand),
                    CXUnOp::PreIncrement(-1) => fwrite!(f, "--{}", operand),
                    CXUnOp::PostIncrement(-1) => fwrite!(f, "{}--", operand),

                    CXUnOp::ExplicitCast(to_type)
                        => fwrite!(f, "({to_type}) ({operand})"),

                    CXUnOp::PreIncrement(_) |
                    CXUnOp::PostIncrement(_) => panic!("Invalid increment operator"),
                }
            },

            CXExprKind::If { condition, then_branch, else_branch } => {
                indent();
                fwriteln!(f, "if {} {{", condition)?;
                fwriteln!(f, "{}", then_branch)?;
                dedent();
                fwriteln!(f, "")?;
                if let Some(else_branch) = else_branch {
                    indent();
                    fwriteln!(f, "}} else {{")?;
                    fwriteln!(f, "{}", else_branch)?;
                    dedent();
                } 
                fwrite!(f, "}}")
            },

            CXExprKind::For { init, condition, increment, body } => {
                indent();
                fwriteln!(f, "for ({}; {}; {}) {{", init, condition, increment)?;
                fwriteln!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")
            },

            CXExprKind::While { condition, body, pre_eval: true } => {
                indent();
                fwriteln!(f, "while ({condition}) {{")?;
                fwriteln!(f, "{}", body)?;
                dedent();
                fwrite!(f, "}}")
            },

            CXExprKind::While { condition, body, pre_eval: false } => {
                indent();
                fwriteln!(f, "while ({condition}) {{")?;
                fwrite!(f, "{}", body)?;
                dedent();
                fwriteln!(f, "")?;
                fwrite!(f, "}}")
            },

            CXExprKind::Defer { expr } => {
                fwrite!(f, "defer {}", expr)
            },
            
            CXExprKind::SizeOf { expr } => {
                fwrite!(f, "sizeof({})", expr)
            },

            _ => fwrite!(f, "{:?}", self)
        }
    }
}

impl Display for CXType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for CXTypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CXTypeKind::Integer { bytes, signed } => {
                let signed_str = if *signed { "i" } else { "u" };
                let signed_bytes = *bytes * 8;
                write!(f, "{signed_str}{signed_bytes}")
            },
            CXTypeKind::Float { bytes } => {
                let float_bytes = *bytes * 8;
                write!(f, "f{float_bytes}")
            },
            CXTypeKind::Bool => write!(f, "bool"),
            CXTypeKind::Structured { fields, name } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{name}: {type_}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let name_str = if let Some(name) = name {
                    format!("{name} ")
                } else {
                    "".to_string()
                };

                write!(f, "struct {name_str} {{ {field_strs} }}")
            },
            CXTypeKind::Union { fields, name } => {
                let field_strs = fields.iter()
                    .map(|(name, type_)| format!("{name}: {type_}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let name_str = if let Some(name) = name {
                    format!("{name} ")
                } else {
                    "".to_string()
                };

                write!(f, "union {name_str} {{ {field_strs} }}")
            },
            CXTypeKind::Unit => write!(f, "()"),
            CXTypeKind::PointerTo { inner, explicitly_weak: false } => {
                write!(f, "{inner}*")
            },
            CXTypeKind::PointerTo { inner, explicitly_weak: true } => {
                write!(f, "{inner} weak*")
            },
            CXTypeKind::StrongPointer { inner, .. } => {
                write!(f, "{inner} strong*")
            },
            CXTypeKind::Array { size, _type } => {
                write!(f, "[{size}; {_type}]")
            },
            CXTypeKind::VariableLengthArray { _type, .. } => {
                write!(f, "[{_type}; variable]")
            },
            CXTypeKind::Opaque { name, size } => {
                write!(f, "OPAQUE_{size}(\"{name}\")")
            },
            CXTypeKind::Identifier { name, .. } => {
                write!(f, "{name}")
            },
            CXTypeKind::Function { prototype } => {
                write!(f, "fn {prototype}")
            },
            CXTypeKind::MemoryAlias(inner) => {
                write!(f, "mem({inner})")
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

            CXBinOp::Less       => fwrite!(f, "<"),
            CXBinOp::LessEqual  => fwrite!(f, "<="),
            CXBinOp::Greater    => fwrite!(f, ">"),
            CXBinOp::GreaterEqual => fwrite!(f, ">="),

            CXBinOp::Access     => fwrite!(f, "."),

            CXBinOp::Comma      => fwrite!(f, ","),

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