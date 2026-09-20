use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    expr::intrinsic::MIRIntrinsic,
    ty::MIRTypeID,
    unit::MIRBasicBlockID,
    value::{MIRBindable, MIRBlockTarget, MIRPlaceID, MIRRegisterID, MIRValue},
};

dense_id!(MIRScopeID);

#[derive(Debug, Clone)]
pub struct MIRBasicBlock {
    pub id: MIRBasicBlockID,
    pub debug_name: Option<CXIdent>,
    pub params: Vec<MIRRegisterID>,
    pub instructions: Vec<MIRInstruction>,
}

impl MIRBasicBlock {
    pub fn new(id: MIRBasicBlockID) -> Self {
        Self {
            id,
            params: Vec::new(),
            debug_name: None,
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, instr: MIRInstruction) -> &mut MIRInstruction {
        self.instructions.push(instr);
        self.instructions.last_mut()
            .unwrap()
    }

    pub fn terminator(&self) -> Option<&MIRInstruction> {
        self.instructions
            .last()
            .filter(|instr| instr.kind.is_terminator())
    }
}

#[derive(Debug, Clone)]
pub struct MIRInstruction {
    pub kind: MIRInstrKind,
    pub token_range: TokenRange,
}

impl MIRInstruction {
    pub fn new(kind: MIRInstrKind, token_range: TokenRange) -> Self {
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
        place: MIRBindable,
        leak: bool,
    },

    // Lifts a value out of a place into a register, if the register is untouched before use, it can avoid the need for a true
    // intermediate copy.
    LiftPlace {
        out: MIRRegisterID,
        place: MIRPlaceID,
    },
    // Declares (for analysis) that a register or place must live at least as long as 'bind_to' for ownership analysis
    // registe
    BindLifetime {
        bind: MIRBindable,
        bind_to: MIRPlaceID,
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

impl MIRInstruction {
    fn is_terminator(&self) -> bool {
        matches!(
            self.kind,
            Self::Return { .. }
                | Self::Jump { .. }
                | Self::Branch { .. }
                | Self::CaseBranch { .. }
                | Self::Unreachable
        )
    }
}
