use cx_util::identifier::CXIdent;

use crate::{op::{MIRBinaryOp, MIRUnaryOp}, ty::MIRType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRPlace(CXIdent);

#[derive(Debug, Clone)]
pub enum MIROperand {
    Place(MIRPlace),
    Register(CXIdent)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRBasicBlockID(CXIdent);

#[derive(Debug, Clone)]
pub struct MIRBasicBlock {
    id: MIRBasicBlockID,
    instrs: Vec<MIRInstr>    
}

#[derive(Debug, Clone)]
pub struct MIRInstr {
    kind: MIRInstrKind
}

#[derive(Debug, Clone)]
pub enum MIRInstrKind {
    LivenessStart(MIRPlace),
    LivenessEnd(MIRPlace),
    
    CreatePlace {
        out: MIRPlace,
        ty: MIRType,
    },
    
    CopyInto {
        dest: MIRPlace,
        src: MIROperand,
        ty: MIRType
    },

    MemberAccess {
        out: MIROperand,
        base: MIROperand,
        member_index: usize,
        aggregate_type: MIRType
    },

    ArrayAccess {
        out: MIROperand,
        base: MIROperand,
        index: MIROperand,
        aggregate_type: MIRType
    },

    SumTag {
        out: MIROperand,
        base: MIROperand,
        sum_type: MIRType
    },

    SumVariant {
        out: MIROperand,
        base: MIROperand,
        variant_index: usize,
        sum_type: MIRType
    },

    DirectCall {
        out: Option<MIRPlace>,
        symbol: CXIdent,
        args: Vec<MIROperand>,
    },

    IndirectCall {
        out: Option<MIRPlace>,
        callee: MIROperand,
        args: Vec<MIROperand>,
    },

    Return {
        value: Option<MIROperand>
    },

    Jump {
        target: MIRBasicBlockID
    },

    Branch {
        cond: MIROperand,
        true_target: MIRBasicBlockID,
        false_target: MIRBasicBlockID
    },

    IntSwitch {
        value: MIROperand,
        cases: Vec<(i64, MIRBasicBlockID)>,
        default: Option<MIRBasicBlockID>
    },

    BinOp {
        out: MIROperand,
        op: MIRBinaryOp,
        lhs: MIROperand,
        rhs: MIROperand,
    },

    UnOp {
        out: MIROperand,
        op: MIRUnaryOp,
        operand: MIROperand,
    }
}