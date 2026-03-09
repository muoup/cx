use std::fmt::{Display, Formatter, Result};

use crate::ast::*;
use crate::intrinsic::*;

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
            FMIRUnaryIntrinsic::NEG  => write!(f, "neg"),
            FMIRUnaryIntrinsic::INEG => write!(f, "ineg"),
            FMIRUnaryIntrinsic::FNEG => write!(f, "fneg"),
            FMIRUnaryIntrinsic::BNOT => write!(f, "bnot"),
            FMIRUnaryIntrinsic::LNOT => write!(f, "lnot"),
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

impl Display for FMIRIntrinsicIBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRIntrinsicIBinOp::ADD  => write!(f, "add"),
            FMIRIntrinsicIBinOp::SUB  => write!(f, "sub"),
            FMIRIntrinsicIBinOp::MUL  => write!(f, "mul"),
            FMIRIntrinsicIBinOp::DIV  => write!(f, "div"),
            FMIRIntrinsicIBinOp::MOD  => write!(f, "mod"),
            FMIRIntrinsicIBinOp::IMUL => write!(f, "imul"),
            FMIRIntrinsicIBinOp::IDIV => write!(f, "idiv"),
            FMIRIntrinsicIBinOp::IMOD => write!(f, "imod"),
            FMIRIntrinsicIBinOp::EQ   => write!(f, "eq"),
            FMIRIntrinsicIBinOp::NE   => write!(f, "ne"),
            FMIRIntrinsicIBinOp::LT   => write!(f, "lt"),
            FMIRIntrinsicIBinOp::LE   => write!(f, "le"),
            FMIRIntrinsicIBinOp::GT   => write!(f, "gt"),
            FMIRIntrinsicIBinOp::GE   => write!(f, "ge"),
            FMIRIntrinsicIBinOp::ILT  => write!(f, "ilt"),
            FMIRIntrinsicIBinOp::ILE  => write!(f, "ile"),
            FMIRIntrinsicIBinOp::IGT  => write!(f, "igt"),
            FMIRIntrinsicIBinOp::IGE  => write!(f, "ige"),
            FMIRIntrinsicIBinOp::LAND => write!(f, "land"),
            FMIRIntrinsicIBinOp::LOR  => write!(f, "lor"),
            FMIRIntrinsicIBinOp::BAND => write!(f, "band"),
            FMIRIntrinsicIBinOp::BOR  => write!(f, "bor"),
            FMIRIntrinsicIBinOp::BXOR => write!(f, "bxor"),
        }
    }
}

impl Display for FMIRIntrinsicFBinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRIntrinsicFBinOp::FADD => write!(f, "add"),
            FMIRIntrinsicFBinOp::FSUB => write!(f, "sub"),
            FMIRIntrinsicFBinOp::FMUL => write!(f, "mul"),
            FMIRIntrinsicFBinOp::FDIV => write!(f, "div"),
            FMIRIntrinsicFBinOp::FEQ => write!(f, "eq"),
            FMIRIntrinsicFBinOp::FNE => write!(f, "ne"),
            FMIRIntrinsicFBinOp::FLT => write!(f, "lt"),
            FMIRIntrinsicFBinOp::FLE => write!(f, "le"),
            FMIRIntrinsicFBinOp::FGT => write!(f, "gt"),
            FMIRIntrinsicFBinOp::FGE => write!(f, "ge"),
        }
    }
}

impl Display for FMIRPointerBinaryIntrinsicOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRPointerBinaryIntrinsicOp::EQ => write!(f, "eq"),
            FMIRPointerBinaryIntrinsicOp::NE => write!(f, "ne"),
            FMIRPointerBinaryIntrinsicOp::LT => write!(f, "lt"),
            FMIRPointerBinaryIntrinsicOp::GT => write!(f, "gt"),
            FMIRPointerBinaryIntrinsicOp::LE => write!(f, "le"),
            FMIRPointerBinaryIntrinsicOp::GE => write!(f, "ge"),
        }
    }
}

impl Display for FMIRIntrinsicPtrDiffBinop {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            FMIRIntrinsicPtrDiffBinop::ADD => write!(f, "add"),
            FMIRIntrinsicPtrDiffBinop::SUB => write!(f, "sub"),
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
