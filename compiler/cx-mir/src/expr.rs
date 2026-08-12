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

dense_id!(MIRPlaceID);
dense_id!(MIRParameterID);
dense_id!(MIRRegister);
dense_id!(MIRBasicBlockID);

/// An abstract lvalue. Function-local places include source locals, anonymous
/// storage, dereference results, and aggregate projections.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MIRPlace {
    FunctionLocal(MIRPlaceID),
    Parameter(MIRParameterID),
    Global(MIRGlobalID),
}

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
    Null,
    Function(MIRFunctionID),
    Undefined,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRValue {
    Register(MIRRegister),
    Place(MIRPlace),
    Constant(MIRConstant),
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
    // Transitional lifetime kernels. Their abstract semantics will move into
    // the operations that create, consume, drop, or leak places.
    LifetimeStart(MIRPlace),
    LifetimeEnd(MIRPlace),
    Leak(MIRPlace),

    Create {
        out: MIRPlace,
        ty: MIRType,
    },
    Copy {
        dest: MIRPlace,
        src: MIRValue,
        ty: MIRType,
    },
    Move {
        dest: MIRPlace,
        src: MIRPlace,
        ty: MIRType,
    },
    AddressOf {
        out: MIRRegister,
        place: MIRPlace,
    },

    // Place-producing projection kernels. They preserve an abstract lvalue;
    // physical addresses and offsets are selected during MIR -> LMIR lowering.
    ProjectDeref {
        out: MIRPlace,
        pointer: MIRValue,
        pointee_type: MIRType,
    },
    ProjectField {
        out: MIRPlace,
        base: MIRPlace,
        field: usize,
        aggregate_type: MIRType,
    },
    ProjectIndex {
        out: MIRPlace,
        base: MIRPlace,
        index: MIRValue,
        element_type: MIRType,
    },
    Discriminant {
        out: MIRRegister,
        value: MIRValue,
        sum_type: MIRType,
    },
    ProjectVariant {
        out: MIRPlace,
        base: MIRPlace,
        variant: usize,
        sum_type: MIRType,
    },
    ConstructAggregate {
        out: MIRRegister,
        kind: MIRAggregateKind,
        ty: MIRType,
        fields: Vec<(usize, MIRValue)>,
    },
    ConstructVariant {
        out: MIRRegister,
        variant: usize,
        value: MIRValue,
        sum_type: MIRType,
    },
    SetVariant {
        target: MIRPlace,
        variant: usize,
        value: MIRValue,
        sum_type: MIRType,
    },

    DirectCall {
        out: Option<MIRRegister>,
        function: MIRFunctionID,
        args: Vec<MIRValue>,
    },
    IndirectCall {
        out: Option<MIRRegister>,
        callee: MIRValue,
        args: Vec<MIRValue>,
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
        to_type: MIRType,
    },
    Phi {
        out: MIRRegister,
        incoming: Vec<(MIRBasicBlockID, MIRValue)>,
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
        target: MIRBasicBlockID,
    },
    Branch {
        cond: MIRValue,
        true_target: MIRBasicBlockID,
        false_target: MIRBasicBlockID,
    },
    IntSwitch {
        value: MIRValue,
        cases: Vec<(MIRConstant, MIRBasicBlockID)>,
        default: Option<MIRBasicBlockID>,
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
        self.for_each_value(|value| {
            if let MIRValue::Place(place) = value {
                f(*place);
            }
        });

        match self {
            Self::LifetimeStart(place)
            | Self::LifetimeEnd(place)
            | Self::Leak(place)
            | Self::AddressOf { place, .. } => f(*place),
            Self::Move { src, .. } => f(*src),
            Self::ProjectField { base, .. }
            | Self::ProjectIndex { base, .. }
            | Self::ProjectVariant { base, .. } => f(*base),
            _ => {}
        }
    }

    pub fn for_each_defined_place(&self, mut f: impl FnMut(MIRPlace)) {
        match self {
            Self::Create { out, .. }
            | Self::Copy { dest: out, .. }
            | Self::Move { dest: out, .. }
            | Self::ProjectDeref { out, .. }
            | Self::ProjectField { out, .. }
            | Self::ProjectIndex { out, .. }
            | Self::ProjectVariant { out, .. }
            | Self::SetVariant { target: out, .. } => f(*out),
            _ => {}
        }
    }

    pub fn for_each_referenced_register(&self, mut f: impl FnMut(MIRRegister)) {
        self.for_each_value(|value| {
            if let MIRValue::Register(register) = value {
                f(*register);
            }
        });
    }

    pub fn for_each_defined_register(&self, mut f: impl FnMut(MIRRegister)) {
        match self {
            Self::AddressOf { out, .. }
            | Self::Discriminant { out, .. }
            | Self::ConstructAggregate { out, .. }
            | Self::ConstructVariant { out, .. }
            | Self::BinOp { out, .. }
            | Self::UnOp { out, .. }
            | Self::Coerce { out, .. }
            | Self::Phi { out, .. } => f(*out),
            Self::DirectCall { out, .. } | Self::IndirectCall { out, .. } => {
                if let Some(out) = out {
                    f(*out);
                }
            }
            _ => {}
        }
    }

    pub fn for_each_referenced_function(&self, mut f: impl FnMut(MIRFunctionID)) {
        if let Self::DirectCall { function, .. } = self {
            f(*function);
        }
        self.for_each_value(|value| {
            if let MIRValue::Constant(MIRConstant::Function(function)) = value {
                f(*function);
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

    fn for_each_value(&self, mut f: impl FnMut(&MIRValue)) {
        match self {
            Self::Copy { src, .. }
            | Self::ProjectDeref { pointer: src, .. }
            | Self::Discriminant { value: src, .. }
            | Self::ConstructVariant { value: src, .. }
            | Self::SetVariant { value: src, .. }
            | Self::Emit { value: src }
            | Self::UnOp { operand: src, .. }
            | Self::Coerce { operand: src, .. }
            | Self::Assert { condition: src, .. }
            | Self::Assume { condition: src }
            | Self::Branch { cond: src, .. }
            | Self::IntSwitch { value: src, .. } => f(src),
            Self::ProjectIndex { index, .. } => f(index),
            Self::ConstructAggregate { fields, .. } => {
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
