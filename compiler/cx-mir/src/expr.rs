use std::slice;

use cx_thir::thir::r#type::{THIRFloatType, THIRIntType};
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{
    global::{MIRFunctionID, MIRGlobalID},
    op::{MIRBinaryOp, MIRCoercion, MIRUnaryOp},
    ty::MIRType,
};

macro_rules! dense_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub usize);

        impl $name {
            pub const fn new(index: usize) -> Self {
                Self(index)
            }

            pub const fn index(self) -> usize {
                self.0
            }
        }
    };
}

dense_id!(MIRPlace);
dense_id!(MIRRegister);
dense_id!(MIRBasicBlockID);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRConstant {
    Unit,
    Bool(bool),
    Integer {
        value: i128,
        ty: THIRIntType,
        signed: bool,
    },
    Float {
        value: FloatWrapper,
        ty: THIRFloatType,
    },
    String(String),
    Null,
    Undefined,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIROperand {
    Place(MIRPlace),
    Register(MIRRegister),

    Parameter(usize),
    Constant(MIRConstant),
    Function(MIRFunctionID),
    Global(MIRGlobalID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRDestination {
    Place(MIRPlace),
    Register(MIRRegister),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRAggregateKind {
    Array,
    Struct,
}

#[derive(Debug, Clone)]
pub struct MIRBasicBlock {
    pub id: MIRBasicBlockID,
    pub debug_name: Option<CXIdent>,
    pub instrs: Vec<MIRInstr>,
}

impl MIRBasicBlock {
    pub fn new(id: MIRBasicBlockID) -> Self {
        Self {
            id,
            debug_name: None,
            instrs: Vec::new(),
        }
    }

    pub fn push(&mut self, kind: MIRInstrKind) -> &mut MIRInstr {
        self.instrs.push(MIRInstr::new(kind));
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
}

impl MIRInstr {
    pub fn new(kind: MIRInstrKind) -> Self {
        Self { kind }
    }

    pub fn is_terminator(&self) -> bool {
        self.kind.is_terminator()
    }

    pub fn successors(&self) -> MIRSuccessors<'_> {
        self.kind.successors()
    }

    pub fn for_each_referenced_place(&self, f: impl FnMut(MIRPlace)) {
        self.kind.for_each_referenced_place(f);
    }

    pub fn for_each_defined_place(&self, f: impl FnMut(MIRPlace)) {
        self.kind.for_each_defined_place(f);
    }

    pub fn for_each_referenced_register(&self, f: impl FnMut(MIRRegister)) {
        self.kind.for_each_referenced_register(f);
    }

    pub fn for_each_defined_register(&self, f: impl FnMut(MIRRegister)) {
        self.kind.for_each_defined_register(f);
    }
}

#[derive(Debug, Clone)]
pub enum MIRInstrKind {
    LivenessStart(MIRPlace),
    LivenessEnd(MIRPlace),
    LeakPlace(MIRPlace),

    CreatePlace {
        out: MIRPlace,
        ty: MIRType,
    },
    CopyInto {
        dest: MIRPlace,
        src: MIROperand,
        ty: MIRType,
    },
    MoveInto {
        dest: MIRPlace,
        src: MIRPlace,
        ty: MIRType,
    },
    Assign {
        out: MIRDestination,
        value: MIROperand,
    },
    Load {
        out: MIRDestination,
        source: MIRPlace,
    },
    Store {
        dest: MIRPlace,
        value: MIROperand,
    },
    AddressOf {
        out: MIRDestination,
        place: MIRPlace,
    },
    Dereference {
        out: MIRPlace,
        pointer: MIROperand,
        pointee_type: MIRType,
    },

    MemberAccess {
        out: MIRPlace,
        base: MIRPlace,
        member_index: usize,
        aggregate_type: MIRType,
    },
    ArrayAccess {
        out: MIRPlace,
        base: MIRPlace,
        index: MIROperand,
        element_type: MIRType,
    },
    SumTag {
        out: MIRDestination,
        base: MIROperand,
        sum_type: MIRType,
    },
    SumVariant {
        out: MIRPlace,
        base: MIRPlace,
        variant_index: usize,
        sum_type: MIRType,
    },
    ConstructAggregate {
        out: MIRDestination,
        kind: MIRAggregateKind,
        ty: MIRType,
        fields: Vec<(usize, MIROperand)>,
    },
    UpdateAggregate {
        out: MIRDestination,
        base: MIROperand,
        ty: MIRType,
        fields: Vec<(usize, MIROperand)>,
    },
    ConstructSum {
        out: MIRDestination,
        variant_index: usize,
        value: MIROperand,
        sum_type: MIRType,
    },
    SetSumVariant {
        target: MIRPlace,
        variant_index: usize,
        value: MIROperand,
        sum_type: MIRType,
    },

    DirectCall {
        out: Option<MIRDestination>,
        function: MIRFunctionID,
        args: Vec<MIROperand>,
    },
    IndirectCall {
        out: Option<MIRDestination>,
        callee: MIROperand,
        args: Vec<MIROperand>,
    },

    BinOp {
        out: MIRDestination,
        op: MIRBinaryOp,
        lhs: MIROperand,
        rhs: MIROperand,
    },
    UnOp {
        out: MIRDestination,
        op: MIRUnaryOp,
        operand: MIROperand,
    },
    Coerce {
        out: MIRDestination,
        operand: MIROperand,
        coercion: MIRCoercion,
        to_type: MIRType,
    },
    Phi {
        out: MIRRegister,
        incoming: Vec<(MIRBasicBlockID, MIROperand)>,
    },
    Assert {
        condition: MIROperand,
        message: Option<String>,
    },
    Assume {
        condition: MIROperand,
    },

    Return {
        value: Option<MIROperand>,
    },
    Jump {
        target: MIRBasicBlockID,
    },
    Branch {
        cond: MIROperand,
        true_target: MIRBasicBlockID,
        false_target: MIRBasicBlockID,
    },
    IntSwitch {
        value: MIROperand,
        cases: Vec<(MIRConstant, MIRBasicBlockID)>,
        default: Option<MIRBasicBlockID>,
    },
    Unreachable,

    // Comptime-only nodes
    
    Emit {
        value: MIROperand,
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
                | Self::Unreachable
        )
    }

    pub fn successors(&self) -> MIRSuccessors<'_> {
        match self {
            Self::Jump { target } => MIRSuccessors::One(Some(*target)),
            Self::Branch {
                true_target,
                false_target,
                ..
            } => MIRSuccessors::Two {
                values: [*true_target, *false_target],
                index: 0,
            },
            Self::IntSwitch { cases, default, .. } => MIRSuccessors::Switch {
                cases: cases.iter(),
                default: *default,
            },
            _ => MIRSuccessors::Empty,
        }
    }

    pub fn for_each_referenced_place(&self, mut f: impl FnMut(MIRPlace)) {
        self.for_each_operand(|operand| {
            if let MIROperand::Place(place) = operand {
                f(*place);
            }
        });

        match self {
            Self::LivenessStart(place)
            | Self::LivenessEnd(place)
            | Self::LeakPlace(place)
            | Self::Load { source: place, .. }
            | Self::AddressOf { place, .. } => f(*place),
            Self::MoveInto { src, .. } => f(*src),
            Self::MemberAccess { base, .. }
            | Self::ArrayAccess { base, .. }
            | Self::SumVariant { base, .. } => f(*base),
            _ => {}
        }
    }

    pub fn for_each_defined_place(&self, mut f: impl FnMut(MIRPlace)) {
        match self {
            Self::CreatePlace { out, .. }
            | Self::CopyInto { dest: out, .. }
            | Self::MoveInto { dest: out, .. }
            | Self::Store { dest: out, .. }
            | Self::Dereference { out, .. }
            | Self::MemberAccess { out, .. }
            | Self::ArrayAccess { out, .. }
            | Self::SumVariant { out, .. }
            | Self::SetSumVariant { target: out, .. } => f(*out),
            _ => self.for_each_destination(|destination| {
                if let MIRDestination::Place(place) = destination {
                    f(place);
                }
            }),
        }
    }

    pub fn for_each_referenced_register(&self, mut f: impl FnMut(MIRRegister)) {
        self.for_each_operand(|operand| {
            if let MIROperand::Register(register) = operand {
                f(*register);
            }
        });
    }

    pub fn for_each_defined_register(&self, mut f: impl FnMut(MIRRegister)) {
        if let Self::Phi { out, .. } = self {
            f(*out);
            return;
        }
        self.for_each_destination(|destination| {
            if let MIRDestination::Register(register) = destination {
                f(register);
            }
        });
    }

    pub fn for_each_referenced_function(&self, mut f: impl FnMut(MIRFunctionID)) {
        if let Self::DirectCall { function, .. } = self {
            f(*function);
        }
        self.for_each_operand(|operand| {
            if let MIROperand::Function(function) = operand {
                f(*function);
            }
        });
    }

    pub fn for_each_referenced_global(&self, mut f: impl FnMut(MIRGlobalID)) {
        self.for_each_operand(|operand| {
            if let MIROperand::Global(global) = operand {
                f(*global);
            }
        });
    }

    pub fn for_each_phi_predecessor(&self, mut f: impl FnMut(MIRBasicBlockID)) {
        if let Self::Phi { incoming, .. } = self {
            for (block, _) in incoming {
                f(*block);
            }
        }
    }

    fn for_each_operand(&self, mut f: impl FnMut(&MIROperand)) {
        match self {
            Self::CopyInto { src, .. }
            | Self::Assign { value: src, .. }
            | Self::Store { value: src, .. }
            | Self::Dereference { pointer: src, .. }
            | Self::SumTag { base: src, .. }
            | Self::ConstructSum { value: src, .. }
            | Self::SetSumVariant { value: src, .. }
            | Self::Emit { value: src }
            | Self::UnOp { operand: src, .. }
            | Self::Coerce { operand: src, .. }
            | Self::Assert { condition: src, .. }
            | Self::Assume { condition: src }
            | Self::Branch { cond: src, .. }
            | Self::IntSwitch { value: src, .. } => f(src),
            Self::ArrayAccess { index, .. } => f(index),
            Self::ConstructAggregate { fields, .. } => {
                for (_, value) in fields {
                    f(value);
                }
            }
            Self::UpdateAggregate { base, fields, .. } => {
                f(base);
                for (_, value) in fields {
                    f(value);
                }
            }
            Self::DirectCall { args, .. } => {
                for argument in args {
                    f(argument);
                }
            }
            Self::IndirectCall { callee, args, .. } => {
                f(callee);
                for argument in args {
                    f(argument);
                }
            }
            Self::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Self::Phi { incoming, .. } => {
                for (_, value) in incoming {
                    f(value);
                }
            }
            Self::Return { value } => {
                if let Some(value) = value {
                    f(value);
                }
            }
            _ => {}
        }
    }

    fn for_each_destination(&self, mut f: impl FnMut(MIRDestination)) {
        match self {
            Self::Assign { out, .. }
            | Self::Load { out, .. }
            | Self::AddressOf { out, .. }
            | Self::SumTag { out, .. }
            | Self::ConstructAggregate { out, .. }
            | Self::UpdateAggregate { out, .. }
            | Self::ConstructSum { out, .. }
            | Self::BinOp { out, .. }
            | Self::UnOp { out, .. }
            | Self::Coerce { out, .. } => f(*out),
            Self::DirectCall { out, .. } | Self::IndirectCall { out, .. } => {
                if let Some(out) = out {
                    f(*out);
                }
            }
            _ => {}
        }
    }
}

pub enum MIRSuccessors<'a> {
    Empty,
    One(Option<MIRBasicBlockID>),
    Two {
        values: [MIRBasicBlockID; 2],
        index: usize,
    },
    Switch {
        cases: slice::Iter<'a, (MIRConstant, MIRBasicBlockID)>,
        default: Option<MIRBasicBlockID>,
    },
}

impl Iterator for MIRSuccessors<'_> {
    type Item = MIRBasicBlockID;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Empty => None,
            Self::One(value) => value.take(),
            Self::Two { values, index } => {
                let value = values.get(*index).copied();
                *index += usize::from(value.is_some());
                value
            }
            Self::Switch { cases, default } => cases
                .next()
                .map(|(_, target)| *target)
                .or_else(|| default.take()),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = match self {
            Self::Empty => 0,
            Self::One(value) => usize::from(value.is_some()),
            Self::Two { index, .. } => 2usize.saturating_sub(*index),
            Self::Switch { cases, default } => cases.len() + usize::from(default.is_some()),
        };
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for MIRSuccessors<'_> {}
