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
    Move(MIRPlace),
    Constant(MIRConstant),
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
    Dereference {
        pointer: MIRValue,
        pointee_type: MIRType,
    },
    Field {
        base: MIRPlace,
        field: usize,
        aggregate_type: MIRType,
    },
    Index {
        base: MIRPlace,
        index: MIRValue,
        element_type: MIRType,
    },
    Variant {
        base: MIRPlace,
        variant: usize,
        sum_type: MIRType,
    },
}

#[derive(Debug, Clone)]
pub enum MIRValueAggregateOp {
    Discriminant {
        value: MIRValue,
        sum_type: MIRType,
    },
    Construct {
        ty: MIRType,
        fields: Vec<(usize, MIRValue)>,
    },
    Variant {
        variant: usize,
        value: MIRValue,
        sum_type: MIRType,
    },
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

    pub fn successors(&self) -> impl ExactSizeIterator<Item = MIRBasicBlockID> + '_ {
        self.kind.successors()
    }

    pub fn for_each_target(&self, f: impl FnMut(&MIRBlockTarget)) {
        self.kind.for_each_target(f);
    }

    pub fn for_each_referenced_place(&self, f: impl FnMut(MIRPlace)) {
        self.kind.for_each_referenced_place(f);
    }

    pub fn for_each_moved_place(&self, f: impl FnMut(MIRPlace)) {
        self.kind.for_each_moved_place(f);
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
    // Explicit storage effects. Create allocates abstract storage, while
    // Initialize and Leak describe semantic transitions over that storage.
    Initialize {
        place: MIRPlace,
    },
    Leak {
        place: MIRPlace,
    },

    Create {
        out: MIRPlace,
        ty: MIRType,
    },
    Assign {
        dest: MIRPlace,
        value: MIRValue,
        ty: MIRType,
    },
    AddressOf {
        out: MIRRegister,
        place: MIRPlace,
    },

    AggregateOp(MIRAggregateOp),

    Call {
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
        subject: MIRPlace,
        sum_type: MIRType,
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

    pub fn successors(&self) -> impl ExactSizeIterator<Item = MIRBasicBlockID> + '_ {
        let mut successors = Vec::new();
        self.for_each_target(|target| successors.push(target.block));
        successors.into_iter()
    }

    pub fn for_each_target(&self, mut f: impl FnMut(&MIRBlockTarget)) {
        match self {
            Self::Jump { target } => f(target),
            Self::Branch {
                true_target,
                false_target,
                ..
            } => {
                f(true_target);
                f(false_target);
            }
            Self::IntSwitch { cases, default, .. } => {
                for (_, target) in cases {
                    f(target);
                }
                if let Some(default) = default {
                    f(default);
                }
            }
            Self::VariantSwitch { cases, default, .. } => {
                for (_, target) in cases {
                    f(target);
                }
                if let Some(default) = default {
                    f(default);
                }
            }
            _ => {}
        }
    }

    pub fn for_each_referenced_place(&self, mut f: impl FnMut(MIRPlace)) {
        self.for_each_value(|value| match value {
            MIRValue::Place(place) | MIRValue::Move(place) => f(*place),
            _ => {}
        });

        match self {
            Self::Leak { place }
            | Self::AddressOf { place, .. }
            | Self::VariantSwitch { subject: place, .. } => f(*place),
            Self::AggregateOp(op) => op.for_each_referenced_place(f),
            _ => {}
        }
    }

    pub fn for_each_moved_place(&self, mut f: impl FnMut(MIRPlace)) {
        self.for_each_value(|value| {
            if let MIRValue::Move(place) = value {
                f(*place);
            }
        });
    }

    pub fn for_each_defined_place(&self, mut f: impl FnMut(MIRPlace)) {
        match self {
            Self::Initialize { place: out }
            | Self::Create { out, .. }
            | Self::Assign { dest: out, .. } => {
                f(*out);
            }
            Self::AggregateOp(MIRAggregateOp::Place { out, .. }) => f(*out),
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
            | Self::BinOp { out, .. }
            | Self::UnOp { out, .. }
            | Self::Coerce { out, .. } => f(*out),
            Self::AggregateOp(MIRAggregateOp::Value { out, .. }) => f(*out),
            Self::Call { out, .. } => {
                if let Some(out) = out {
                    f(*out);
                }
            }
            _ => {}
        }
    }

    pub fn for_each_referenced_function(&self, mut f: impl FnMut(MIRFunctionID)) {
        self.for_each_value(|value| {
            if let MIRValue::Constant(MIRConstant::Function(function)) = value {
                f(*function);
            }
        });
    }

    fn for_each_value(&self, mut f: impl FnMut(&MIRValue)) {
        match self {
            Self::Assign { value: src, .. }
            | Self::Emit { value: src }
            | Self::UnOp { operand: src, .. }
            | Self::Coerce { operand: src, .. }
            | Self::Assert { condition: src, .. }
            | Self::Assume { condition: src } => f(src),
            Self::AggregateOp(op) => op.for_each_value(f),
            Self::Call { callee, args, .. } => {
                f(callee);
                for argument in args {
                    f(argument);
                }
            }
            Self::BinOp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Self::Return { value } => {
                if let Some(value) = value {
                    f(value);
                }
            }
            Self::Branch {
                cond,
                true_target,
                false_target,
            } => {
                f(cond);
                true_target.for_each_value(&mut f);
                false_target.for_each_value(&mut f);
            }
            Self::IntSwitch {
                value,
                cases,
                default,
            } => {
                f(value);
                for (_, target) in cases {
                    target.for_each_value(&mut f);
                }
                if let Some(default) = default {
                    default.for_each_value(&mut f);
                }
            }
            Self::VariantSwitch { cases, default, .. } => {
                for (_, target) in cases {
                    target.for_each_value(&mut f);
                }
                if let Some(default) = default {
                    default.for_each_value(&mut f);
                }
            }
            Self::Jump { target } => target.for_each_value(f),
            _ => {}
        }
    }
}

impl MIRBlockTarget {
    fn for_each_value(&self, mut f: impl FnMut(&MIRValue)) {
        for argument in &self.args {
            f(argument);
        }
    }
}

impl MIRAggregateOp {
    fn for_each_referenced_place(&self, mut f: impl FnMut(MIRPlace)) {
        match self {
            Self::Place {
                op:
                    MIRPlaceAggregateOp::Field { base, .. }
                    | MIRPlaceAggregateOp::Index { base, .. }
                    | MIRPlaceAggregateOp::Variant { base, .. },
                ..
            } => f(*base),
            _ => {}
        }
    }

    fn for_each_value(&self, mut f: impl FnMut(&MIRValue)) {
        match self {
            Self::Place {
                op: MIRPlaceAggregateOp::Dereference { pointer, .. },
                ..
            } => f(pointer),
            Self::Place {
                op: MIRPlaceAggregateOp::Index { index, .. },
                ..
            } => f(index),
            Self::Value {
                op:
                    MIRValueAggregateOp::Discriminant { value, .. }
                    | MIRValueAggregateOp::Variant { value, .. },
                ..
            } => f(value),
            Self::Value {
                op: MIRValueAggregateOp::Construct { fields, .. },
                ..
            } => {
                for (_, value) in fields {
                    f(value);
                }
            }
            _ => {}
        }
    }
}
