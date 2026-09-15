use cx_mir::visit::{MIRVisitor, MIRWalk};
use cx_mir::{MIRComptimeInstrKind, MIRComptimeOp, MIRInstrKind, MIRInstructionKind};

#[derive(Clone, Copy)]
pub enum Instruction<'a> {
    Standard(&'a MIRInstrKind),
    Comptime(&'a MIRComptimeOp),
}

impl Instruction<'_> {
    pub fn standard(&self) -> Option<&MIRInstrKind> {
        match self {
            Self::Standard(kind) => Some(kind),
            Self::Comptime(_) => None,
        }
    }

    pub fn visit<'ir, V: MIRVisitor<'ir>>(&'ir self, visitor: &mut V) -> Result<(), V::Error> {
        match self {
            Self::Standard(kind) => kind.visit(visitor),
            Self::Comptime(kind) => kind.visit(visitor),
        }
    }
}

pub(crate) trait AnalysisInstruction: MIRWalk + MIRInstructionKind {
    fn view(&self) -> Instruction<'_>;
}

impl AnalysisInstruction for MIRInstrKind {
    fn view(&self) -> Instruction<'_> {
        Instruction::Standard(self)
    }
}

impl AnalysisInstruction for MIRComptimeInstrKind {
    fn view(&self) -> Instruction<'_> {
        match self {
            Self::Standard(kind) => Instruction::Standard(kind),
            Self::Comptime(kind) => Instruction::Comptime(kind),
        }
    }
}
