use std::fmt::{Display, Formatter, Result};

use crate::ast::*;

impl Display for MemoryLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            MemoryLocation::Stack { name, depth } => {
                write!(f, "stack({}@{})", name, depth)
            }
            MemoryLocation::Parameter(name) => {
                write!(f, "param({})", name)
            }
            MemoryLocation::Global(name) => {
                write!(f, "global({})", name)
            }
        }
    }
}

impl Display for CVMOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            CVMOperation::Unsafe => {
                write!(f, "Unsafe")
            }
            CVMOperation::Access { reads, writes } => {
                write!(f, "Access{{")?;
                if !reads.is_empty() {
                    write!(f, " reads: [")?;
                    for (i, r) in reads.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", r)?;
                    }
                    write!(f, "]")?;
                }
                if !writes.is_empty() {
                    if !reads.is_empty() {
                        write!(f, ",")?;
                    }
                    write!(f, " writes: [")?;
                    for (i, w) in writes.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", w)?;
                    }
                    write!(f, "]")?;
                }
                write!(f, " }}")
            }
        }
    }
}

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
            FMIRType::Pure { mir_type } => {
                write!(f, "{}", mir_type)
            }

            FMIRType::CMonad { inner, effect } => {
                match effect {
                    CVMOperation::Unsafe => {
                        write!(f, "Effect<Unsafe, {}>", inner)
                    }
                    CVMOperation::Access { reads, writes } => {
                        write!(f, "Effect<Access{{")?;
                        if !reads.is_empty() {
                            write!(f, " reads: [")?;
                            for (i, r) in reads.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{}", r)?;
                            }
                            write!(f, "]")?;
                        }
                        if !writes.is_empty() {
                            if !reads.is_empty() {
                                write!(f, ",")?;
                            }
                            write!(f, " writes: [")?;
                            for (i, w) in writes.iter().enumerate() {
                                if i > 0 {
                                    write!(f, ", ")?;
                                }
                                write!(f, "{}", w)?;
                            }
                            write!(f, "]")?;
                        }
                        write!(f, " }}, {}>", inner)
                    }
                }
            }

            FMIRType::Mapping {
                from_type: parameter,
                to_type: return_type,
                ..
            } => {
                write!(f, "{} -> {}", parameter, return_type)
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

            FMIRNodeBody::UnsafeBlock => {
                write!(f, "_unsafe_block")
            }

            FMIRNodeBody::DeclareAccess { reads, writes } => {
                write!(f, "_declare_access{{")?;
                if !reads.is_empty() {
                    write!(f, " reads: [")?;
                    for (i, r) in reads.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", r)?;
                    }
                    write!(f, "]")?;
                }
                if !writes.is_empty() {
                    if !reads.is_empty() {
                        write!(f, ",")?;
                    }
                    write!(f, " writes: [")?;
                    for (i, w) in writes.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", w)?;
                    }
                    write!(f, "]")?;
                }
                write!(f, " }}")
            }

            FMIRNodeBody::Load { pointer } => {
                write!(f, "_load $ {pointer}")
            }

            FMIRNodeBody::Store { pointer, value }=> {
                write!(f, "_store $ {pointer} $ {value}")
            }

            FMIRNodeBody::Alloca => {
                write!(f, "_alloca")
            }

            FMIRNodeBody::Pure => {
                write!(f, "_pure")
            }
            
            FMIRNodeBody::VariableAlias { name } => {
                write!(f, "{}", name)
            },

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

            FMIRNodeBody::Bind { monad, capture, function } => {
                monad.fmt_with_indent(f, indent)?;
                write!(f, " >>= \\{capture} ->")?;
                let nested = indent.push();
                writeln!(f)?;
                nested.fmt(f)?;
                function.fmt_with_indent(f, &nested)
            }

            FMIRNodeBody::Then { first, second } => {
                first.fmt_with_indent(f, indent)?;
                writeln!(f)?;
                write!(f, ">> ")?;
                second.fmt_with_indent(f, indent)
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

            FMIRNodeBody::FloatLiteral(x) => {
                write!(f, "{}", x)
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