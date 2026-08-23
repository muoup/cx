use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{
    global::{MIRFunctionID, MIRGlobalID},
    op::{MIRBinaryOp, MIRCoercion, MIRUnaryOp},
    ty::{MIRFloatType, MIRIntType, MIRTypeID},
};

dense_id!(MIRPlaceID);
dense_id!(MIRParameterID);
dense_id!(MIRRegister);
dense_id!(MIRBasicBlockID);
dense_id!(MIRScopeID);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MIRPlace {
    FunctionLocal(MIRPlaceID),
    Parameter(MIRParameterID),
    Global(MIRGlobalID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRConstant {
    // TODO: Revamp and simplify this a bit; Add staged expressions w/ a more streamlined comptime engine in the MIR layer
    Unit,
    Bool(bool),
    String(String),
    Integer {
        value: i128,
        ty: MIRIntType,
        signed: bool,
    },
    Float {
        value: FloatWrapper,
        ty: MIRFloatType,
    },
    Null {
        ty: MIRTypeID,
    },
    Aggregate {
        ty: MIRTypeID,
        fields: Vec<(usize, MIRConstant)>,
    },
    Global {
        global: MIRGlobalID,
        ty: MIRTypeID,
    },
    GlobalOffset {
        global: MIRGlobalID,
        offset: i64,
        ty: MIRTypeID,
    },
    Function(MIRFunctionID),
    Undefined,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    Register(MIRRegister),
    PlaceRef(MIRPlace),
    Copy(MIRPlace),
    Move(MIRPlace),
    Constant(MIRConstant),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRAssignTarget {
    Place(MIRPlace),
    Register(MIRRegister),
}

#[derive(Debug, Clone, Copy)]
pub enum MIRInstrOperand<'a> {
    Value(&'a MIRValue),
    Place(MIRPlace),
}

impl MIRInstrOperand<'_> {
    pub const fn place(self) -> Option<MIRPlace> {
        match self {
            Self::Value(MIRValue::PlaceRef(place) | MIRValue::Copy(place) | MIRValue::Move(place)) => {
                Some(*place)
            }
            Self::Place(place) => Some(place),
            _ => None,
        }
    }

    pub const fn register(self) -> Option<MIRRegister> {
        match self {
            Self::Value(MIRValue::Register(register)) => Some(*register),
            _ => None,
        }
    }

    pub const fn function(self) -> Option<MIRFunctionID> {
        match self {
            Self::Value(MIRValue::Constant(MIRConstant::Function(function))) => Some(*function),
            _ => None,
        }
    }
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
    Place {
        out: MIRPlace,
        op: MIRPlaceAggregateOp,
    },
    Value {
        out: MIRRegister,
        op: MIRValueAggregateOp,
    },
}

#[derive(Debug, Clone)]
pub enum MIRPlaceAggregateOp {
    Field {
        base: MIRPlace,
        field: usize,
        aggregate_type: MIRTypeID,
    },
    Index {
        base: MIRPlace,
        index: MIRValue,
        element_type: MIRTypeID,
    },
    Variant {
        base: MIRPlace,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRCallKind {
    Runtime,
    Comptime,
}

#[derive(Debug, Clone)]
pub struct MIRBasicBlock {
    pub id: MIRBasicBlockID,
    pub debug_name: Option<CXIdent>,
    pub params: Vec<MIRRegister>,
    pub instrs: Vec<MIRInstr>,
}

impl MIRBasicBlock {
    pub fn new(id: MIRBasicBlockID) -> Self {
        Self {
            id,
            params: Vec::new(),
            debug_name: None,
            instrs: Vec::new(),
        }
    }

    pub fn push(&mut self, kind: MIRInstrKind) -> &mut MIRInstr {
        self.instrs.push(MIRInstr::new(kind, TokenRange::internal()));
        self.instrs
            .last_mut()
            .expect("an instruction was just pushed")
    }

    pub fn terminator(&self) -> Option<&MIRInstr> {
        self.instrs
            .last()
            .filter(|instr| instr.kind.is_terminator())
    }
}

#[derive(Debug, Clone)]
pub struct MIRInstr {
    pub kind: MIRInstrKind,
    pub token_range: TokenRange,
}

impl MIRInstr {
    pub fn new(kind: MIRInstrKind, token_range: TokenRange) -> Self {
        Self { kind, token_range }
    }

    pub fn is_terminator(&self) -> bool {
        self.kind.is_terminator()
    }

    pub fn successors(&self) -> impl ExactSizeIterator<Item = MIRBasicBlockID> + '_ {
        let mut successors = Vec::new();
        match &self.kind {
            MIRInstrKind::Jump { target } => successors.push(target.block),
            MIRInstrKind::Branch {
                true_target,
                false_target,
                ..
            } => {
                successors.push(true_target.block);
                successors.push(false_target.block);
            }
            MIRInstrKind::IntSwitch { cases, default, .. } => {
                successors.extend(cases.iter().map(|(_, target)| target.block));
                successors.extend(default.iter().map(|target| target.block));
            }
            MIRInstrKind::VariantSwitch { cases, default, .. } => {
                successors.extend(cases.iter().map(|(_, target)| target.block));
                successors.extend(default.iter().map(|target| target.block));
            }
            _ => {}
        }
        successors.into_iter()
    }

    pub fn defined_places(&self) -> impl Iterator<Item = MIRPlace> + '_ {
        let place = match &self.kind {
            MIRInstrKind::Initialize { place }
            | MIRInstrKind::Create { out: place, .. }
            | MIRInstrKind::Dereference { out: place, .. } => Some(*place),
            MIRInstrKind::Assign {
                target: MIRAssignTarget::Place(place),
                ..
            } => Some(*place),
            MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, .. }) => Some(*out),
            _ => None,
        };
        place.into_iter()
    }

    pub fn defined_registers(&self) -> impl Iterator<Item = MIRRegister> + '_ {
        let register = match &self.kind {
            MIRInstrKind::AddressOf { out, .. }
            | MIRInstrKind::BinOp { out, .. }
            | MIRInstrKind::UnOp { out, .. }
            | MIRInstrKind::Coerce { out, .. } => Some(*out),
            MIRInstrKind::Assign {
                target: MIRAssignTarget::Register(register),
                ..
            } => Some(*register),
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value { out, .. }) => Some(*out),
            MIRInstrKind::Call { out, .. } => *out,
            MIRInstrKind::VaArg { out, .. } => Some(*out),
            _ => None,
        };
        register.into_iter()
    }

    pub fn visit_operands(&self, mut visit: impl FnMut(MIRInstrOperand<'_>)) {
        match &self.kind {
            MIRInstrKind::Assign { value, .. }
            | MIRInstrKind::Emit { value }
            | MIRInstrKind::UnOp { operand: value, .. }
            | MIRInstrKind::Coerce { operand: value, .. }
            | MIRInstrKind::Assert {
                condition: value, ..
            }
            | MIRInstrKind::Assume { condition: value } => {
                visit(MIRInstrOperand::Value(value));
            }

            MIRInstrKind::AggregateOp(operation) => match operation {
                MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Field { base, .. },
                    ..
                }
                | MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Variant { base, .. },
                    ..
                } => visit(MIRInstrOperand::Place(*base)),
                MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Index { base, index, .. },
                    ..
                } => {
                    visit(MIRInstrOperand::Value(index));
                    visit(MIRInstrOperand::Place(*base));
                }
                MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::Discriminant { value, .. },
                    ..
                }
                | MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::Variant { value, .. },
                    ..
                }
                | MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::ProjectVariant { value, .. },
                    ..
                } => visit(MIRInstrOperand::Value(value)),
                MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::Construct { fields, .. },
                    ..
                } => {
                    for (_, value) in fields {
                        visit(MIRInstrOperand::Value(value));
                    }
                }
            },

            MIRInstrKind::Dereference { pointer, .. } => {
                visit(MIRInstrOperand::Value(pointer));
            }

            MIRInstrKind::Call { callee, args, .. } => {
                visit(MIRInstrOperand::Value(callee));
                for arg in args {
                    visit(MIRInstrOperand::Value(arg));
                }
            }
            MIRInstrKind::VaStart { list, last } => {
                visit(MIRInstrOperand::Value(list));
                visit(MIRInstrOperand::Value(last));
            }
            MIRInstrKind::VaEnd { list } | MIRInstrKind::VaArg { list, .. } => {
                visit(MIRInstrOperand::Value(list));
            }
            MIRInstrKind::BinOp { lhs, rhs, .. } => {
                visit(MIRInstrOperand::Value(lhs));
                visit(MIRInstrOperand::Value(rhs));
            }
            MIRInstrKind::Return { value: Some(value) } => visit(MIRInstrOperand::Value(value)),
            MIRInstrKind::Return { value: None } => {}
            MIRInstrKind::Jump { target } => {
                visit_target_operands(target, &mut visit);
            }
            MIRInstrKind::Branch {
                cond,
                true_target,
                false_target,
            } => {
                visit(MIRInstrOperand::Value(cond));
                visit_target_operands(true_target, &mut visit);
                visit_target_operands(false_target, &mut visit);
            }
            MIRInstrKind::IntSwitch {
                value,
                cases,
                default,
            } => {
                visit(MIRInstrOperand::Value(value));
                for (_, target) in cases {
                    visit_target_operands(target, &mut visit);
                }
                if let Some(default) = default {
                    visit_target_operands(default, &mut visit);
                }
            }
            MIRInstrKind::VariantSwitch {
                subject,
                cases,
                default,
                ..
            } => {
                for (_, target) in cases {
                    visit_target_operands(target, &mut visit);
                }
                if let Some(default) = default {
                    visit_target_operands(default, &mut visit);
                }
                visit(MIRInstrOperand::Value(subject));
            }
            MIRInstrKind::Leak { place } | MIRInstrKind::AddressOf { place, .. } => {
                visit(MIRInstrOperand::Place(*place));
            }
            MIRInstrKind::Initialize { .. }
            | MIRInstrKind::Create { .. }
            | MIRInstrKind::ScopeEnter { .. }
            | MIRInstrKind::ScopeExit { .. }
            | MIRInstrKind::Unreachable => {}
        }
    }
}

fn visit_target_operands<'a>(
    target: &'a MIRBlockTarget,
    visit: &mut impl FnMut(MIRInstrOperand<'a>),
) {
    for arg in &target.args {
        visit(MIRInstrOperand::Value(arg));
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
        place: MIRPlace,
    },
    Leak {
        place: MIRPlace,
    },

    Create {
        out: MIRPlace,
        ty: MIRTypeID,
    },
    Assign {
        target: MIRAssignTarget,
        value: MIRValue,
        ty: MIRTypeID,
    },
    AddressOf {
        out: MIRRegister,
        place: MIRPlace,
    },
    Dereference {
        out: MIRPlace,
        pointer: MIRValue,
        pointee_type: MIRTypeID,
    },

    AggregateOp(MIRAggregateOp),

    Call {
        out: Option<MIRRegister>,
        kind: MIRCallKind,
        callee: MIRValue,
        args: Vec<MIRValue>,
    },
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

    // Comptime-only nodes
    Emit {
        value: MIRValue,
    },
}

impl MIRInstrKind {
    pub fn is_terminator(&self) -> bool {
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
