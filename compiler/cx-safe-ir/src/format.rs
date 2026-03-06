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

            FMIRType::CMonad { inner, operation } => {
                match operation {
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

impl Display for FMIRIntrinsicFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for FMIRIntrinsicKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRIntrinsicKind::Unary(op) => write!(f, "intrinsic.unary.{op}"),
            FMIRIntrinsicKind::Binary(op) => write!(f, "intrinsic.binary.{op}"),
            FMIRIntrinsicKind::Cast(op) => write!(f, "intrinsic.cast.{op}"),
        }
    }
}

impl Display for FMIRUnaryIntrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRUnaryIntrinsic::Neg => write!(f, "neg"),
            FMIRUnaryIntrinsic::INeg => write!(f, "ineg"),
            FMIRUnaryIntrinsic::FNeg => write!(f, "fneg"),
            FMIRUnaryIntrinsic::BNot => write!(f, "bnot"),
            FMIRUnaryIntrinsic::LNot => write!(f, "lnot"),
        }
    }
}

impl Display for FMIRBinaryIntrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRBinaryIntrinsic::Integer { bits, op } => write!(f, "i{bits}.{op}"),
            FMIRBinaryIntrinsic::Float { bits, op } => write!(f, "f{bits}.{op}"),
            FMIRBinaryIntrinsic::Pointer { op } => write!(f, "ptr.{op}"),
            FMIRBinaryIntrinsic::PointerDiff { op } => write!(f, "ptrdiff.{op}"),
        }
    }
}

impl Display for FMIRIntegerBinaryIntrinsicOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRIntegerBinaryIntrinsicOp::Add => write!(f, "add"),
            FMIRIntegerBinaryIntrinsicOp::Sub => write!(f, "sub"),
            FMIRIntegerBinaryIntrinsicOp::Mul => write!(f, "mul"),
            FMIRIntegerBinaryIntrinsicOp::Div => write!(f, "div"),
            FMIRIntegerBinaryIntrinsicOp::Mod => write!(f, "mod"),
            FMIRIntegerBinaryIntrinsicOp::IMul => write!(f, "imul"),
            FMIRIntegerBinaryIntrinsicOp::IDiv => write!(f, "idiv"),
            FMIRIntegerBinaryIntrinsicOp::IMod => write!(f, "imod"),
            FMIRIntegerBinaryIntrinsicOp::Eq => write!(f, "eq"),
            FMIRIntegerBinaryIntrinsicOp::Ne => write!(f, "ne"),
            FMIRIntegerBinaryIntrinsicOp::Lt => write!(f, "lt"),
            FMIRIntegerBinaryIntrinsicOp::Le => write!(f, "le"),
            FMIRIntegerBinaryIntrinsicOp::Gt => write!(f, "gt"),
            FMIRIntegerBinaryIntrinsicOp::Ge => write!(f, "ge"),
            FMIRIntegerBinaryIntrinsicOp::ILt => write!(f, "ilt"),
            FMIRIntegerBinaryIntrinsicOp::ILe => write!(f, "ile"),
            FMIRIntegerBinaryIntrinsicOp::IGt => write!(f, "igt"),
            FMIRIntegerBinaryIntrinsicOp::IGe => write!(f, "ige"),
            FMIRIntegerBinaryIntrinsicOp::LAnd => write!(f, "land"),
            FMIRIntegerBinaryIntrinsicOp::LOr => write!(f, "lor"),
            FMIRIntegerBinaryIntrinsicOp::BAnd => write!(f, "band"),
            FMIRIntegerBinaryIntrinsicOp::BOr => write!(f, "bor"),
            FMIRIntegerBinaryIntrinsicOp::BXor => write!(f, "bxor"),
        }
    }
}

impl Display for FMIRFloatBinaryIntrinsicOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRFloatBinaryIntrinsicOp::Add => write!(f, "add"),
            FMIRFloatBinaryIntrinsicOp::Sub => write!(f, "sub"),
            FMIRFloatBinaryIntrinsicOp::Mul => write!(f, "mul"),
            FMIRFloatBinaryIntrinsicOp::Div => write!(f, "div"),
            FMIRFloatBinaryIntrinsicOp::Eq => write!(f, "eq"),
            FMIRFloatBinaryIntrinsicOp::Ne => write!(f, "ne"),
            FMIRFloatBinaryIntrinsicOp::Lt => write!(f, "lt"),
            FMIRFloatBinaryIntrinsicOp::Le => write!(f, "le"),
            FMIRFloatBinaryIntrinsicOp::Gt => write!(f, "gt"),
            FMIRFloatBinaryIntrinsicOp::Ge => write!(f, "ge"),
        }
    }
}

impl Display for FMIRPointerBinaryIntrinsicOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRPointerBinaryIntrinsicOp::Eq => write!(f, "eq"),
            FMIRPointerBinaryIntrinsicOp::Ne => write!(f, "ne"),
            FMIRPointerBinaryIntrinsicOp::Lt => write!(f, "lt"),
            FMIRPointerBinaryIntrinsicOp::Gt => write!(f, "gt"),
            FMIRPointerBinaryIntrinsicOp::Le => write!(f, "le"),
            FMIRPointerBinaryIntrinsicOp::Ge => write!(f, "ge"),
        }
    }
}

impl Display for FMIRPointerDiffBinaryIntrinsicOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRPointerDiffBinaryIntrinsicOp::Add => write!(f, "add"),
            FMIRPointerDiffBinaryIntrinsicOp::Sub => write!(f, "sub"),
        }
    }
}

impl Display for FMIRCastIntrinsic {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRCastIntrinsic::Integral { sextend, to_bits } => {
                write!(f, "integral.to_i{to_bits}.sx{}", if *sextend { 1 } else { 0 })
            }
            FMIRCastIntrinsic::FloatCast { to_bits } => write!(f, "float.to_f{to_bits}"),
            FMIRCastIntrinsic::PtrToInt { to_bits } => write!(f, "ptr.to_i{to_bits}"),
            FMIRCastIntrinsic::IntToPtr { sextend } => {
                write!(f, "int.to_ptr.sx{}", if *sextend { 1 } else { 0 })
            }
            FMIRCastIntrinsic::IntToFloat { to_bits, sextend } => {
                write!(f, "int.to_f{to_bits}.sx{}", if *sextend { 1 } else { 0 })
            }
            FMIRCastIntrinsic::FloatToInt { to_bits, sextend } => {
                write!(f, "float.to_i{to_bits}.sx{}", if *sextend { 1 } else { 0 })
            }
            FMIRCastIntrinsic::IntToBool => write!(f, "int.to_bool"),
            FMIRCastIntrinsic::ReinterpretBits => write!(f, "reinterpret_bits"),
        }
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

            FMIRNodeBody::IntrinsicFunction(intrinsic) => {
                write!(f, "{}", intrinsic)
            }

            FMIRNodeBody::UnsafeBlock => {
                write!(f, "_unsafe_block")
            }
            
            FMIRNodeBody::CompilerAssert { condition, message } => {
                write!(f, "_compiler_assert (")?;
                condition.fmt_with_indent(f, indent)?;
                write!(f, ") \"{}\"", message)
            },

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
