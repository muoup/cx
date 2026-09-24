use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    MIRTarget, expr::intrinsic::MIRIntrinsic, ty::MIRTypeID, unit::MIRBasicBlockID, value::{MIRBindable, MIRBlockTarget, MIRPlaceID, MIRRegisterID, MIRValue},
};

dense_id!(MIRScopeID, "scope.");

#[derive(Debug, Clone)]
pub struct MIRBasicBlock<I = MIRInstruction> {
    id: MIRBasicBlockID,
    debug_name: Option<CXIdent>,
    params: Vec<MIRRegisterID>,
    instructions: Vec<I>,
}

impl<I> MIRBasicBlock<I> {
    pub fn new(id: MIRBasicBlockID, debug_name: Option<CXIdent>) -> Self {
        Self {
            id,
            debug_name,

            params: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, instr: I) -> &mut I {
        self.instructions.push(instr);
        self.instructions.last_mut().unwrap()
    }

    pub fn id(&self) -> MIRBasicBlockID {
        self.id
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }

    pub fn params(&self) -> &[MIRRegisterID] {
        &self.params
    }

    pub fn param(&self, index: usize) -> Option<&MIRRegisterID> {
        self.params.get(index)
    }

    pub fn push_param(&mut self, param: MIRRegisterID) {
        self.params.push(param);
    }

    pub fn instructions(&self) -> &[I] {
        &self.instructions
    }

    pub fn instruction(&self, index: usize) -> Option<&I> {
        self.instructions.get(index)
    }

    pub fn last_instruction(&self) -> Option<&I> {
        self.instructions.last()
    }

    pub fn push_instruction(&mut self, instr: I) {
        self.instructions.push(instr);
    }
}

#[derive(Debug, Clone)]
pub struct MIRInstruction {
    pub kind: MIRInstructionKind,
    pub token_range: TokenRange,
}

impl MIRInstruction {
    pub fn new(kind: MIRInstructionKind, token_range: TokenRange) -> Self {
        Self { kind, token_range }
    }

    pub fn is_terminator(&self) -> bool {
        self.kind.is_terminator()
    }
}

pub trait MIRInstructionLike {
    fn is_terminator(&self) -> bool;
}

impl MIRInstructionLike for MIRInstruction {
    fn is_terminator(&self) -> bool {
        self.kind.is_terminator()
    }
}

#[derive(Debug, Clone)]
pub enum MIRInstructionKind {
    // Marks a place as live for ownership analysis
    Initialize {
        place: MIRBindable,
    },
    // Marks a place as no longer live for ownership analysis
    Invalidate {
        place: MIRBindable,
        kind: MIRInvalidationKind,
    },

    Lift {
        out: MIRRegisterID,
        source: MIRTarget,
    },
    // Declares (for analysis) that a register or place must live at least as long as 'bind_to' for ownership analysis
    // registe
    BindLifetime {
        bind: MIRBindable,
        bind_to: MIRPlaceID,
    },

    Store {
        target: MIRTarget,
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
        cases: Vec<(i128, MIRBlockTarget)>,
        default: Option<MIRBlockTarget>,
    },

    Unreachable,
}

impl MIRInstructionKind {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::Return { .. }
                | Self::Jump { .. }
                | Self::Branch { .. }
                | Self::CaseBranch { .. }
                | Self::Unreachable
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRInvalidationKind {
    Leak,
    Move,
    Drop,
}
