use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::{
    MIRTarget,
    expr::intrinsic::MIRIntrinsic,
    ty::{MIRBitfieldAccess, MIRTypeID},
    unit::MIRBasicBlockID,
    value::{MIRBindable, MIRBlockTarget, MIRPlaceID, MIRRegisterID, MIRValue},
};

dense_id!(MIRScopeID, "scope.");

#[derive(Debug, Clone)]
pub struct MIRBasicBlock {
    id: MIRBasicBlockID,
    debug_name: Option<CXIdent>,
    params: Vec<MIRRegisterID>,
    instructions: Vec<MIRInstruction>,
}

impl MIRBasicBlock {
    pub fn new(id: MIRBasicBlockID, debug_name: Option<CXIdent>) -> Self {
        Self {
            id,
            debug_name,

            params: Vec::new(),
            instructions: Vec::new(),
        }
    }

    pub fn push(&mut self, instr: MIRInstruction) -> &mut MIRInstruction {
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

    pub fn instructions(&self) -> &[MIRInstruction] {
        &self.instructions
    }

    pub fn instruction(&self, index: usize) -> Option<&MIRInstruction> {
        self.instructions.get(index)
    }

    pub fn last_instruction(&self) -> Option<&MIRInstruction> {
        self.instructions.last()
    }

    pub fn push_instruction(&mut self, instr: MIRInstruction) {
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

    // Produces the value addressed by 'source' (a place, or a reference register read through),
    // consuming the liveness of 'origin', the binding that owns that storage
    Lift {
        out: MIRRegisterID,
        source: MIRBindable,
        origin: MIRBindable,
    },
    // Declares that 'bind' is a view whose validity depends on 'bind_to' remaining live
    BindLifetime {
        bind: MIRBindable,
        bind_to: MIRPlaceID,
    },

    // Eagerly copies 'value' into 'target'; used for assignments and all copies
    Store {
        target: MIRTarget,
        value: MIRValue,
        ty: MIRTypeID,
        bitfield: Option<MIRStoreBitfield>,
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
    // 'signed' selects how 'value' and the case constants are interpreted
    CaseBranch {
        value: MIRValue,
        signed: bool,
        cases: Vec<(i128, MIRBlockTarget)>,
        default: Option<MIRBlockTarget>,
    },
    /// Jumps to the block that `address` is a block address of, which is one of `targets`.
    IndirectJump {
        address: MIRValue,
        targets: Vec<MIRBlockTarget>,
    },

    Unreachable,
}

/// Which side of a store addresses a bitfield; a single store never touches two bitfields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRStoreBitfield {
    /// 'value' references the bitfield's storage unit, and the extracted field is stored.
    Source(MIRBitfieldAccess),
    /// 'target' addresses the bitfield's storage unit, and the value is inserted into the field.
    Target(MIRBitfieldAccess),
}

impl MIRInstructionKind {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Self::Return { .. }
                | Self::Jump { .. }
                | Self::Branch { .. }
                | Self::CaseBranch { .. }
                | Self::IndirectJump { .. }
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
