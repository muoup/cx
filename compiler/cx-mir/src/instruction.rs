use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{
    global::{MIRFunctionID, MIRGlobalID},
    op::{MIRBinaryOp, MIRCoercion, MIRUnaryOp},
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
};

dense_id!(MIRPlaceID);
dense_id!(MIRRegister);
dense_id!(MIRBasicBlockID);
dense_id!(MIRScopeID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRConstant {
    Unit,
    Integer {
        value: i128,
        ty: MIRIntType,
        signed: bool,
    },
    Float {
        value: FloatWrapper,
        ty: MIRFloatType,
    },
    Aggregate {
        ty: MIRTypeID,
        fields: Vec<(usize, MIRConstant)>,
    },
    Global {
        global: MIRGlobalID,
        offset: i64,
        ty: MIRTypeID,
    },

    Nullptr {
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Undefined,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    Register(MIRRegister),
    Place(MIRPlaceID),
    Global(MIRGlobalID),
    Constant(MIRConstant),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRTarget {
    Place(MIRPlaceID),
    Global(MIRGlobalID),
    Indirect(MIRRegister)
}

#[derive(Debug, Clone)]
pub struct MIRBlockTarget {
    pub block: MIRBasicBlockID,
    pub args: Vec<MIRValue>,
}

impl MIRBlockTarget {
    pub fn new(block: MIRBasicBlockID) -> Self {
        Self {
            block,
            args: Vec::new(),
        }
    }

    pub fn with_args(block: MIRBasicBlockID, args: Vec<MIRValue>) -> Self {
        Self { block, args }
    }
}

impl From<MIRBasicBlockID> for MIRBlockTarget {
    fn from(block: MIRBasicBlockID) -> Self {
        Self::new(block)
    }
}

#[derive(Debug, Clone)]
pub enum MIRAggregateOp {
    Target {
        out: MIRRegister,
        op: MIRTargetAggregateOp,
    },
    Value {
        out: MIRRegister,
        op: MIRValueAggregateOp,
    },
}

#[derive(Debug, Clone)]
pub enum MIRTargetAggregateOp {
    Field {
        base: MIRTarget,
        field: usize,
        aggregate_type: MIRTypeID,
    },
    Index {
        base: MIRTarget,
        index: MIRValue,
        element_type: MIRTypeID,
    },
    Variant {
        base: MIRTarget,
        variant: usize,
        sum_type: MIRTypeID,
    },
}

#[derive(Debug, Clone)]
pub enum MIRValueAggregateOp {
    Discriminant {
        value: MIRValue,
        sum_type: MIRTypeID,
    },
    Construct {
        ty: MIRTypeID,
        fields: Vec<(usize, MIRValue)>,
    },
    Variant {
        variant: usize,
        value: MIRValue,
        sum_type: MIRTypeID,
    },
    ProjectVariant {
        variant: usize,
        value: MIRValue,
        sum_type: MIRTypeID,
    },
}

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
    pub params: Vec<MIRRegister>,
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

    Initialize {
        place: MIRPlaceID,
    },
    Bind {
        place: MIRPlaceID,
        to: MIRTarget,
    },
    Invalidate {
        place: MIRPlaceID,
        leak: bool,
    },

    Copy {
        out: MIRRegister,
        source: MIRTarget,
        ty: MIRTypeID,
    },
    Store {
        target: MIRTarget,
        value: MIRValue,
        ty: MIRTypeID,
    },

    AggregateOp(MIRAggregateOp),

    Call {
        out: Option<MIRRegister>,
        callee: MIRValue,
        args: Vec<MIRValue>,
    },
    Intrinsic(MIRIntrinsic),

    BinOp {
        out: MIRRegister,
        op: MIRBinaryOp,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    UnOp {
        out: MIRRegister,
        op: MIRUnaryOp,
        operand: MIRValue,
    },
    Coerce {
        out: MIRRegister,
        operand: MIRValue,
        coercion: MIRCoercion,
        to_type: MIRTypeID,
    },
    Assert {
        condition: MIRValue,
        message: Option<String>,
    },
    Assume {
        condition: MIRValue,
    },

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
    IntSwitch {
        value: MIRValue,
        cases: Vec<(MIRConstant, MIRBlockTarget)>,
        default: Option<MIRBlockTarget>,
    },
    VariantSwitch {
        subject: MIRValue,
        sum_type: MIRTypeID,
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
                | Self::IntSwitch { .. }
                | Self::VariantSwitch { .. }
                | Self::Unreachable
        )
    }
}

#[derive(Debug, Clone)]
pub enum MIRIntrinsic {
    VaStart {
        list: MIRValue,
        last: MIRValue,
    },
    VaEnd {
        list: MIRValue,
    },
    VaArg {
        out: MIRRegister,
        list: MIRValue,
        ty: MIRTypeID,
    },
}
