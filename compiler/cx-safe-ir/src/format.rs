use std::fmt::{Display, Formatter, Result};

use crate::ast::*;

struct Indent(usize);

impl Indent {
    fn new() -> Self {
        Indent(0)
    }

    fn push(&self) -> Self {
        Indent(self.0 + 2)
    }

    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:width$}", "", width = self.0)
    }
}

impl Display for FMIRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRType::CMonad { inner, .. } => {
                write!(f, "CMonad {}", inner)
            }

            FMIRType::Mapping {
                from_type: parameter,
                to_type: return_type,
                ..
            } => {
                write!(f, "{} -> {}", parameter, return_type)
            }

            FMIRType::Standard(mir_type) => {
                write!(f, "{}", mir_type)
            }
        }
    }
}

impl Display for FMIRNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.fmt_with_indent(f, &Indent::new())
    }
}

impl FMIRNode {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: &Indent) -> Result {
        self.body.fmt_with_indent(f, indent)
    }
}

impl FMIRNodeBody {
    fn fmt_with_indent(&self, f: &mut Formatter<'_>, indent: &Indent) -> Result {
        match self {
            FMIRNodeBody::Application { function, argument } => match &function.body {
                FMIRNodeBody::Application { .. } => {
                    write!(f, "({}) {}", function, argument)
                }
                _ => {
                    write!(f, "{} {}", function, argument)
                }
            },

            FMIRNodeBody::Read => {
                write!(f, "_read")
            }

            FMIRNodeBody::Write => {
                write!(f, "_write")
            }

            FMIRNodeBody::Alloca => {
                write!(f, "_alloca")
            }

            FMIRNodeBody::Pure => {
                write!(f, "_pure")
            }

            FMIRNodeBody::CLoop { condition, body } => {
                write!(f, "_cloop (")?;
                condition.fmt_with_indent(f, indent)?;
                write!(f, ")")?;
                let nested = indent.push();
                writeln!(f)?;
                nested.fmt(f)?;
                write!(f, "$ ")?;
                body.fmt_with_indent(f, &nested)
            }

            FMIRNodeBody::Bind { monad, function } => {
                monad.fmt_with_indent(f, indent)?;
                write!(f, " >>=")?;
                let nested = indent.push();
                writeln!(f)?;
                nested.fmt(f)?;
                function.fmt_with_indent(f, &nested)
            }

            FMIRNodeBody::Then { first, second } => {
                first.fmt_with_indent(f, indent)?;
                write!(f, " >>")?;
                let nested = indent.push();
                writeln!(f)?;
                nested.fmt(f)?;
                second.fmt_with_indent(f, &nested)
            }

            FMIRNodeBody::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if ")?;
                condition.fmt_with_indent(f, indent)?;
                let nested = indent.push();
                writeln!(f)?;
                nested.fmt(f)?;
                write!(f, "then ")?;
                then_branch.fmt_with_indent(f, &nested)?;
                writeln!(f)?;
                nested.fmt(f)?;
                write!(f, "else ")?;
                else_branch.fmt_with_indent(f, &nested)
            }

            FMIRNodeBody::CReturn { value } => {
                write!(f, "_creturn {}", value)
            }

            FMIRNodeBody::IntegerLiteral(n) => {
                write!(f, "{}", n)
            }

            FMIRNodeBody::BooleanLiteral(b) => {
                write!(f, "{}", b)
            }

            FMIRNodeBody::Unit => {
                write!(f, "()")
            }
        }
    }
}

impl Display for FMIRNodeBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.fmt_with_indent(f, &Indent::new())
    }
}

impl Display for FMIRFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} :: ", self.prototype.name)?;

        write!(f, "{}", self.prototype.return_type)?;
        for param in &self.prototype.params {
            write!(f, " -> {}", param._type)?;
        }

        writeln!(f)?;
        writeln!(f, "{} =", self.prototype.name)?;
        let base_indent = Indent::new().push();
        base_indent.fmt(f)?;
        self.body.fmt_with_indent(f, &base_indent)?;
        writeln!(f)?;

        Ok(())
    }
}