use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

use cx_hmir::{HMIRBinaryOp, HMIRDefID, HMIRExprID, HMIRIntWidth, HMIRLocalID};
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    program::{DefKey, UnitID},
    ty::{TypeID, TypeKind, TypeTable},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum StaticValue {
    Unit,
    Int {
        value: i128,
        ty: TypeID,
    },
    Float {
        value: FloatWrapper,
        ty: TypeID,
    },
    Str(String),
    Null(TypeID),
    Type(TypeID),
    // A function def with its leading comptime arguments applied
    Function {
        def: DefKey,
        args: Vec<StaticValue>,
    },
    Quote(QuoteRef),
    Aggregate {
        ty: TypeID,
        fields: Vec<(usize, StaticValue)>,
    },
    // Designates a runtime global; only its address can be taken statically
    Global(DefKey),
    GlobalAddress {
        def: DefKey,
        offset: i64,
        ty: TypeID,
    },
}

// A runtime frame inside the function lowering identified by 'lowering'
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct FrameRef {
    lowering: u64,
    frame: usize,
}

#[derive(Debug)]
pub(crate) struct Quote {
    unit: UnitID,
    def: HMIRDefID,
    owner: (DefKey, Vec<StaticValue>),
    params: Vec<HMIRLocalID>,
    body: HMIRExprID,
    env: HashMap<HMIRLocalID, StaticValue>,
    runtime_types: HashMap<HMIRLocalID, TypeID>,
    origin: Option<FrameRef>,
}

#[derive(Debug, Clone)]
pub(crate) struct QuoteRef(Rc<Quote>);

impl FrameRef {
    pub(crate) fn new(lowering: u64, frame: usize) -> Self {
        Self { lowering, frame }
    }

    pub(crate) fn lowering(self) -> u64 {
        self.lowering
    }

    pub(crate) fn frame(self) -> usize {
        self.frame
    }
}

impl Quote {
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn new(
        unit: UnitID,
        def: HMIRDefID,
        owner: (DefKey, Vec<StaticValue>),
        params: Vec<HMIRLocalID>,
        body: HMIRExprID,
        env: HashMap<HMIRLocalID, StaticValue>,
        runtime_types: HashMap<HMIRLocalID, TypeID>,
        origin: Option<FrameRef>,
    ) -> Self {
        Self {
            unit,
            def,
            owner,
            params,
            body,
            env,
            runtime_types,
            origin,
        }
    }

    pub(crate) fn unit(&self) -> UnitID {
        self.unit
    }

    pub(crate) fn def(&self) -> HMIRDefID {
        self.def
    }

    pub(crate) fn owner(&self) -> &(DefKey, Vec<StaticValue>) {
        &self.owner
    }

    pub(crate) fn params(&self) -> &[HMIRLocalID] {
        &self.params
    }

    pub(crate) fn body(&self) -> HMIRExprID {
        self.body
    }

    pub(crate) fn env(&self) -> &HashMap<HMIRLocalID, StaticValue> {
        &self.env
    }

    pub(crate) fn runtime_types(&self) -> &HashMap<HMIRLocalID, TypeID> {
        &self.runtime_types
    }

    pub(crate) fn origin(&self) -> Option<FrameRef> {
        self.origin
    }
}

impl QuoteRef {
    pub(crate) fn new(quote: Quote) -> Self {
        Self(Rc::new(quote))
    }

    pub(crate) fn get(&self) -> &Quote {
        &self.0
    }
}

impl PartialEq for QuoteRef {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for QuoteRef {}

impl Hash for QuoteRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.0) as usize).hash(state);
    }
}

impl StaticValue {
    pub(crate) fn int(value: i128, ty: TypeID) -> Self {
        Self::Int { value, ty }
    }

    pub(crate) fn bool(value: bool, types: &mut TypeTable) -> Self {
        Self::Int {
            value: value as i128,
            ty: types.bool(),
        }
    }

    pub(crate) fn as_int(&self) -> Option<i128> {
        match self {
            Self::Int { value, .. } => Some(*value),
            _ => None,
        }
    }

    pub(crate) fn is_truthy(&self) -> Option<bool> {
        match self {
            Self::Int { value, .. } => Some(*value != 0),
            Self::Float { value, .. } => Some(f64::from(value) != 0.0),
            Self::Null(_) => Some(false),
            Self::Str(_) | Self::GlobalAddress { .. } | Self::Function { .. } => Some(true),
            _ => None,
        }
    }

    // Function values and quotes have no plain type; callers resolve those through the program
    pub(crate) fn simple_type(&self, types: &mut TypeTable) -> Option<TypeID> {
        Some(match self {
            Self::Unit => types.void(),
            Self::Int { ty, .. } | Self::Float { ty, .. } | Self::Null(ty) => *ty,
            Self::Str(_) => types.str(),
            Self::Type(_) => types.type_of_types(),
            Self::Aggregate { ty, .. } | Self::GlobalAddress { ty, .. } => *ty,
            Self::Function { .. } | Self::Quote(_) | Self::Global(_) => return None,
        })
    }
}

// Wraps an integer into the range of 'ty'
pub(crate) fn normalize_int(value: i128, ty: TypeID, types: &TypeTable) -> i128 {
    let Some((width, signed)) = types.int_info(ty) else {
        return value;
    };
    let bits = width.bits() as u32;
    if bits >= 128 {
        return value;
    }
    let mask = (1i128 << bits) - 1;
    let truncated = value & mask;
    if signed && bits > 1 && truncated & (1i128 << (bits - 1)) != 0 {
        truncated - (1i128 << bits)
    } else {
        truncated
    }
}

pub(crate) fn fold_int(op: HMIRBinaryOp, lhs: i128, rhs: i128, signed: bool) -> Option<i128> {
    Some(match op {
        HMIRBinaryOp::Add => lhs.wrapping_add(rhs),
        HMIRBinaryOp::Sub => lhs.wrapping_sub(rhs),
        HMIRBinaryOp::Mul => lhs.wrapping_mul(rhs),
        HMIRBinaryOp::Div => lhs.checked_div(rhs)?,
        HMIRBinaryOp::Mod => lhs.checked_rem(rhs)?,
        HMIRBinaryOp::Eq => (lhs == rhs) as i128,
        HMIRBinaryOp::Neq => (lhs != rhs) as i128,
        HMIRBinaryOp::Lt => (lhs < rhs) as i128,
        HMIRBinaryOp::Le => (lhs <= rhs) as i128,
        HMIRBinaryOp::Gt => (lhs > rhs) as i128,
        HMIRBinaryOp::Ge => (lhs >= rhs) as i128,
        HMIRBinaryOp::LAnd => (lhs != 0 && rhs != 0) as i128,
        HMIRBinaryOp::LOr => (lhs != 0 || rhs != 0) as i128,
        HMIRBinaryOp::BAnd => lhs & rhs,
        HMIRBinaryOp::BOr => lhs | rhs,
        HMIRBinaryOp::BXor => lhs ^ rhs,
        HMIRBinaryOp::LShift => lhs.checked_shl(rhs as u32)?,
        HMIRBinaryOp::RShift if signed => lhs.checked_shr(rhs as u32)?,
        HMIRBinaryOp::RShift => ((lhs as u128).checked_shr(rhs as u32)?) as i128,
    })
}

pub(crate) fn fold_float(op: HMIRBinaryOp, lhs: f64, rhs: f64) -> Option<FloatResult> {
    Some(match op {
        HMIRBinaryOp::Add => FloatResult::Float(lhs + rhs),
        HMIRBinaryOp::Sub => FloatResult::Float(lhs - rhs),
        HMIRBinaryOp::Mul => FloatResult::Float(lhs * rhs),
        HMIRBinaryOp::Div => FloatResult::Float(lhs / rhs),
        HMIRBinaryOp::Eq => FloatResult::Bool(lhs == rhs),
        HMIRBinaryOp::Neq => FloatResult::Bool(lhs != rhs),
        HMIRBinaryOp::Lt => FloatResult::Bool(lhs < rhs),
        HMIRBinaryOp::Le => FloatResult::Bool(lhs <= rhs),
        HMIRBinaryOp::Gt => FloatResult::Bool(lhs > rhs),
        HMIRBinaryOp::Ge => FloatResult::Bool(lhs >= rhs),
        _ => return None,
    })
}

pub(crate) enum FloatResult {
    Float(f64),
    Bool(bool),
}

pub(crate) fn is_comparison(op: HMIRBinaryOp) -> bool {
    matches!(
        op,
        HMIRBinaryOp::Eq
            | HMIRBinaryOp::Neq
            | HMIRBinaryOp::Lt
            | HMIRBinaryOp::Le
            | HMIRBinaryOp::Gt
            | HMIRBinaryOp::Ge
    )
}

pub(crate) fn is_logical(op: HMIRBinaryOp) -> bool {
    matches!(op, HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr)
}

pub(crate) fn promote_integer_type(types: &mut TypeTable, ty: TypeID) -> TypeID {
    match types.int_info(ty) {
        Some((width, _)) if width < HMIRIntWidth::I32 => types.int(HMIRIntWidth::I32, true),
        _ => ty,
    }
}

// C integer promotion followed by the usual arithmetic conversions
pub(crate) fn arithmetic_type(types: &mut TypeTable, lhs: TypeID, rhs: TypeID) -> Option<TypeID> {
    let lhs = promote_integer_type(types, lhs);
    let rhs = promote_integer_type(types, rhs);
    match (types.kind(lhs).clone(), types.kind(rhs).clone()) {
        (TypeKind::Float { width: left }, TypeKind::Float { width: right }) => {
            Some(types.intern(TypeKind::Float {
                width: left.max(right),
            }))
        }
        (TypeKind::Float { .. }, TypeKind::Int { .. }) => Some(lhs),
        (TypeKind::Int { .. }, TypeKind::Float { .. }) => Some(rhs),
        (
            TypeKind::Int {
                width: left,
                signed: left_signed,
            },
            TypeKind::Int {
                width: right,
                signed: right_signed,
            },
        ) => {
            let (width, signed) = if left == right {
                (left, left_signed && right_signed)
            } else if left > right {
                (left, left_signed)
            } else {
                (right, right_signed)
            };
            Some(types.int(width, signed))
        }
        _ => None,
    }
}

pub(crate) fn float_value(value: f64, ty: TypeID) -> StaticValue {
    StaticValue::Float {
        value: FloatWrapper::from(value),
        ty,
    }
}
