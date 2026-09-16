use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    expr::intrinsic::MIRIntrinsic,
    ty::MIRTypeID,
    unit::MIRBasicBlockID,
    value::{MIRBlockTarget, MIRPlaceID, MIRRegisterID, MIRTemporaryID, MIRValue},
};

dense_id!(MIRScopeID);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRStagedExitKind {
    Break,
    Continue,
    Expr,
}

#[derive(Debug, Clone)]
pub struct MIRBasicBlock<K = MIRInstrKind> {
    pub id: MIRBasicBlockID,
    pub debug_name: Option<CXIdent>,
    pub params: Vec<MIRRegisterID>,
    pub instrs: Vec<MIRInstruction<K>>,
}

impl<K: MIRInstructionLike> MIRBasicBlock<K> {
    pub fn new(id: MIRBasicBlockID) -> Self {
        Self {
            id,
            params: Vec::new(),
            debug_name: None,
            instrs: Vec::new(),
        }
    }

    pub fn push(&mut self, kind: K) -> &mut MIRInstruction<K> {
        self.instrs
            .push(MIRInstruction::new(kind, TokenRange::internal()));
        self.instrs
            .last_mut()
            .expect("an instruction was just pushed")
    }

    pub fn terminator(&self) -> Option<&MIRInstruction<K>> {
        self.instrs
            .last()
            .filter(|instr| instr.kind.is_terminator())
    }
}

#[derive(Debug, Clone)]
pub struct MIRInstruction<K = MIRInstrKind> {
    pub kind: K,
    pub token_range: TokenRange,
}

impl<K: MIRInstructionLike> MIRInstruction<K> {
    pub fn new(kind: K, token_range: TokenRange) -> Self {
        Self { kind, token_range }
    }

    pub fn is_terminator(&self) -> bool {
        self.kind.is_terminator()
    }
}

#[derive(Debug, Clone)]
pub enum MIRInstrKind {
    /// Marks the beginning of a lexical scope for ownership analysis.
    ScopeEnter {
        scope: MIRScopeID,
    },
    /// Marks the end of a lexical scope for ownership analysis.
    ScopeExit {
        scope: MIRScopeID,
    },

    // Marks a place as live for ownership analysis
    Initialize {
        place: MIRPlaceID,
    },
    // Marks a place as no longer live for ownership analysis
    Invalidate {
        place: MIRPlaceID,
        leak: bool,
    },
    // Lifts a value out of a place into a temporary, if this temporary is untouched before use, it can avoid the need for a true
    // intermediate copy.
    LiftPlace {
        out: MIRTemporaryID,
        place: MIRPlaceID,
    },
    // Declares (for analysis) that 'place' must live at least as long as 'to' for ownership analysis
    BindLifetime {
        place: MIRPlaceID,
        to: MIRPlaceID,
    },

    Store {
        target: MIRPlaceID,
        value: MIRValue,
        ty: MIRTypeID,
    },

    Call {
        out: Option<MIRRegisterID>,
        callee: MIRValue,
        args: Vec<MIRValue>,
    },

    IntrinsicOp(MIRIntrinsic),

    Return {
        value: Option<MIRValue>,
    },
    Jump {
        target: MIRBlockTarget,
    },
    Branch {
        cond: MIRValue,
        true_target: MIRBlockTarget,
        false_target: MIRBlockTarget,
    },
    CaseBranch {
        value: MIRValue,
        cases: Vec<(usize, MIRBlockTarget)>,
        default: Option<MIRBlockTarget>,
    },

    Unreachable,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct MIRStagedTargets {
    pub return_target: Option<MIRBasicBlockID>,
    pub break_target: Option<MIRBasicBlockID>,
    pub continue_target: Option<MIRBasicBlockID>,
    pub yield_target: Option<MIRBasicBlockID>,
}

pub trait MIRInstructionLike {
    fn is_terminator(&self) -> bool;
}

impl MIRInstructionLike for MIRInstrKind {
    fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::Return { .. }
                | Self::Jump { .. }
                | Self::Branch { .. }
                | Self::CaseBranch { .. }
                | Self::VariantSwitch { .. }
                | Self::Unreachable
        )
    }
}
