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

enum InstrOperand<'a> {
    Value(&'a MIRValue),
    Place(MIRPlace),
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

    /// Iterates over the successor blocks of this instruction's targets.
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

    /// Iterates over the places this instruction reads, including move
    /// sources and non-value place operands such as leak subjects and
    /// aggregate projection bases.
    pub fn referenced_places(&self) -> impl Iterator<Item = MIRPlace> + '_ {
        self.operands()
            .into_iter()
            .filter_map(|operand| match operand {
                InstrOperand::Value(MIRValue::Place(place) | MIRValue::Move(place)) => Some(*place),
                InstrOperand::Place(place) => Some(place),
                _ => None,
            })
    }

    /// Iterates over the places this instruction moves out of.
    pub fn moved_places(&self) -> impl Iterator<Item = MIRPlace> + '_ {
        self.operands()
            .into_iter()
            .filter_map(|operand| match operand {
                InstrOperand::Value(MIRValue::Move(place)) => Some(*place),
                _ => None,
            })
    }

    /// Iterates over the places this instruction defines.
    pub fn defined_places(&self) -> impl Iterator<Item = MIRPlace> + '_ {
        let place = match &self.kind {
            MIRInstrKind::Initialize { place }
            | MIRInstrKind::Create { out: place, .. }
            | MIRInstrKind::Assign { dest: place, .. } => Some(*place),
            MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, .. }) => Some(*out),
            _ => None,
        };
        place.into_iter()
    }

    /// Iterates over the registers this instruction reads.
    pub fn referenced_registers(&self) -> impl Iterator<Item = MIRRegister> + '_ {
        self.operands()
            .into_iter()
            .filter_map(|operand| match operand {
                InstrOperand::Value(MIRValue::Register(register)) => Some(*register),
                _ => None,
            })
    }

    /// Iterates over the registers this instruction defines.
    pub fn defined_registers(&self) -> impl Iterator<Item = MIRRegister> + '_ {
        let register = match &self.kind {
            MIRInstrKind::AddressOf { out, .. }
            | MIRInstrKind::BinOp { out, .. }
            | MIRInstrKind::UnOp { out, .. }
            | MIRInstrKind::Coerce { out, .. } => Some(*out),
            MIRInstrKind::AggregateOp(MIRAggregateOp::Value { out, .. }) => Some(*out),
            MIRInstrKind::Call { out, .. } => *out,
            _ => None,
        };
        register.into_iter()
    }

    /// Iterates over the functions referenced by this instruction's values.
    pub fn referenced_functions(&self) -> impl Iterator<Item = MIRFunctionID> + '_ {
        self.operands()
            .into_iter()
            .filter_map(|operand| match operand {
                InstrOperand::Value(MIRValue::Constant(MIRConstant::Function(function))) => {
                    Some(*function)
                }
                _ => None,
            })
    }

    /// Collects this instruction's value operands in read order, followed by
    /// directly referenced places that are not value operands, such as leak
    /// subjects and aggregate projection bases.
    fn operands(&self) -> Vec<InstrOperand<'_>> {
        match &self.kind {
            MIRInstrKind::Assign { value, .. }
            | MIRInstrKind::Emit { value }
            | MIRInstrKind::UnOp { operand: value, .. }
            | MIRInstrKind::Coerce { operand: value, .. }
            | MIRInstrKind::Assert {
                condition: value, ..
            }
            | MIRInstrKind::Assume { condition: value } => vec![InstrOperand::Value(value)],

            MIRInstrKind::AggregateOp(operation) => match operation {
                MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Dereference { pointer, .. },
                    ..
                } => vec![InstrOperand::Value(pointer)],
                MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Field { base, .. },
                    ..
                }
                | MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Variant { base, .. },
                    ..
                } => vec![InstrOperand::Place(*base)],
                MIRAggregateOp::Place {
                    op: MIRPlaceAggregateOp::Index { base, index, .. },
                    ..
                } => {
                    vec![
                        InstrOperand::Value(index),
                        InstrOperand::Place(*base),
                    ]
                }
                MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::Discriminant { value, .. },
                    ..
                }
                | MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::Variant { value, .. },
                    ..
                } => vec![InstrOperand::Value(value)]
                MIRAggregateOp::Value {
                    op: MIRValueAggregateOp::Construct { fields, .. },
                    ..
                } => fields.iter().map(|(_, value)| InstrOperand::Value(value)).collect()
            },

            MIRInstrKind::Call { callee, args, .. } => {
                let mut operands = vec![];
                operands.push(InstrOperand::Value(callee));
                operands.extend(args.iter().map(InstrOperand::Value));
                operands
            }
            MIRInstrKind::BinOp { lhs, rhs, .. } => {
                vec![InstrOperand::Value(lhs), InstrOperand::Value(rhs)]
            }
            MIRInstrKind::Return { value: Some(value) } => {
                vec![InstrOperand::Value(value)]
            }
            MIRInstrKind::Return { value: None } => vec![],
            MIRInstrKind::Jump { target } => target.args.iter().map(InstrOperand::Value).collect(),
            MIRInstrKind::Branch {
                cond,
                true_target,
                false_target,
            } => {
                let mut operands = vec![];

                operands.push(InstrOperand::Value(cond));
                operands.extend(true_target.args.iter().map(InstrOperand::Value));
                operands.extend(false_target.args.iter().map(InstrOperand::Value));

                operands
            }
            MIRInstrKind::IntSwitch {
                value,
                cases,
                default,
            } => {
                let mut operands = vec![];
                operands.push(InstrOperand::Value(value));
                operands.push(InstrOperand::Value(value));
                for (_, target) in cases {
                    operands.extend(target.args.iter().map(InstrOperand::Value));
                }
                if let Some(default) = default {
                    operands.extend(default.args.iter().map(InstrOperand::Value));
                }
                operands
            }
            MIRInstrKind::VariantSwitch {
                subject,
                cases,
                default,
                ..
            } => {
                let mut operands = vec![];
                for (_, target) in cases {
                    operands.extend(target.args.iter().map(InstrOperand::Value));
                }
                if let Some(default) = default {
                    operands.extend(default.args.iter().map(InstrOperand::Value));
                }
                operands.push(InstrOperand::Place(*subject));
                operands
            }
            MIRInstrKind::Leak { place } | MIRInstrKind::AddressOf { place, .. } => {
                vec![InstrOperand::Place(*place)]
            }
            MIRInstrKind::Initialize { .. }
            | MIRInstrKind::Create { .. }
            | MIRInstrKind::Unreachable => vec![],
        }
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
}