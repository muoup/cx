use cx_log::CXResult;
use cx_mir::visit::{MIRVisitRole, MIRVisitor};
use cx_mir::{
    MIRBinaryOp, MIRCoercion, MIRConstant, MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRPlace,
    MIRPointerBinaryOp, MIRRegister, MIRTarget, MIRUnaryOp, MIRValue,
};

use crate::framework::environment::{Analysis, Context, Environment, Location};
use crate::framework::instruction::Instruction;
use crate::framework::state::{Fact, State, Table};

#[derive(Clone, Debug)]
pub struct ValueTracking;

#[derive(Clone, Debug, Default)]
pub struct ValueEnvironment {
    registers: Table<ConstValue>,
    places: Table<ConstValue>,
}

#[derive(Clone, Debug)]
pub struct ValueState {
    registers: Table<ConstValue>,
    places: Table<ConstValue>,
}

impl Default for ValueState {
    fn default() -> Self {
        Self {
            registers: Table::default(),
            places: Table::default(),
        }
    }
}

impl State for ValueState {
    fn merge(&mut self, incoming: &Self) -> bool {
        self.registers.merge(&incoming.registers) | self.places.merge(&incoming.places)
    }
}

impl Analysis for ValueTracking {
    type Environment = ValueEnvironment;

    fn create(&self, context: &Context<'_>) -> Option<Self::Environment> {
        context
            .function
            .prototype()
            .signature
            .safe
            .then(ValueEnvironment::default)
    }
}

impl Environment for ValueEnvironment {
    type State = ValueState;

    fn snapshot(&self) -> Self::State {
        ValueState {
            registers: self.registers.clone(),
            places: self.places.clone(),
        }
    }

    fn restore(&mut self, state: &Self::State) {
        self.registers.clone_from(&state.registers);
        self.places.clone_from(&state.places);
    }

    fn instruction(
        &mut self,
        context: &Context<'_>,
        location: Location,
        instruction: Instruction<'_>,
        diagnose: bool,
    ) -> CXResult<()> {
        if diagnose
            && let Some(MIRInstrKind::Assert { condition, message }) = instruction.standard()
            && is_false(self.value(context, condition))
        {
            return Err(crate::log::analysis_error(
                context.function,
                location,
                None,
                (
                    &cx_log::catalogue::analysis::PROVEN_FALSE_ASSERTION,
                    (
                        context
                            .function
                            .prototype()
                            .signature
                            .display_name()
                            .to_string(),
                        message.clone(),
                    ),
                ),
            ));
        }

        self.transfer(context, instruction);
        Ok(())
    }

    fn edge(
        &mut self,
        context: &Context<'_>,
        _location: Location,
        args: &[MIRValue],
        params: &[MIRRegister],
        _diagnose: bool,
    ) -> CXResult<()> {
        let values = args
            .iter()
            .map(|value| self.value(context, value))
            .collect::<Vec<_>>();
        for (parameter, value) in params.iter().zip(values) {
            self.registers.insert(parameter.index(), value);
        }
        for argument in args {
            if let MIRValue::Move(place) = argument {
                self.places.insert(context.place_index(*place), Fact::Top);
            }
        }
        Ok(())
    }
}

impl ValueEnvironment {
    fn value(&self, context: &Context<'_>, value: &MIRValue) -> Fact<ConstValue> {
        match value {
            MIRValue::Register(register) => self.registers.get(register.index()).clone().into_top(),
            MIRValue::PlaceRef(place) | MIRValue::Copy(place) | MIRValue::Move(place) => self
                .places
                .get(context.place_index(*place))
                .clone()
                .into_top(),
            MIRValue::Constant(constant) => constant_value(constant),
        }
    }

    fn transfer(&mut self, context: &Context<'_>, instruction: Instruction<'_>) {
        let result = instruction
            .standard()
            .and_then(|kind| self.evaluate(context, kind));

        struct Invalidate<'a> {
            registers: &'a mut Table<ConstValue>,
            places: &'a mut Table<ConstValue>,
            context: &'a Context<'a>,
        }

        impl MIRVisitor<'_> for Invalidate<'_> {
            type Error = std::convert::Infallible;

            fn target(&mut self, _target: &cx_mir::MIRBlockTarget) -> Result<(), Self::Error> {
                Ok(())
            }

            fn register(
                &mut self,
                register: &MIRRegister,
                role: MIRVisitRole,
            ) -> Result<(), Self::Error> {
                if role == MIRVisitRole::Define {
                    self.registers.insert(register.index(), Fact::Top);
                }
                Ok(())
            }

            fn place(&mut self, place: &MIRPlace, role: MIRVisitRole) -> Result<(), Self::Error> {
                if matches!(
                    role,
                    MIRVisitRole::Define
                        | MIRVisitRole::Write
                        | MIRVisitRole::Invalidate
                        | MIRVisitRole::Move
                        | MIRVisitRole::Address
                ) {
                    self.places
                        .insert(self.context.place_index(*place), Fact::Top);
                }
                Ok(())
            }
        }

        let mut invalidation = Invalidate {
            registers: &mut self.registers,
            places: &mut self.places,
            context,
        };
        let _ = instruction.visit(&mut invalidation);

        if instruction.standard().is_none()
            || matches!(
                instruction.standard(),
                Some(
                    MIRInstrKind::Call { .. }
                        | MIRInstrKind::Assign {
                            target: MIRTarget::Place(_),
                            ..
                        }
                )
            )
        {
            self.places.invalidate();
        }

        if let Some((target, value)) = result {
            match target {
                MIRTarget::Register(register) => self.registers.insert(register.index(), value),
                MIRTarget::Place(place) => self.places.insert(context.place_index(place), value),
            }
        }
    }

    fn evaluate(
        &self,
        context: &Context<'_>,
        kind: &MIRInstrKind,
    ) -> Option<(MIRTarget, Fact<ConstValue>)> {
        match kind {
            MIRInstrKind::Assign { target, value, .. } => {
                Some((*target, self.value(context, value)))
            }
            MIRInstrKind::BinOp { out, op, lhs, rhs } => Some((
                MIRTarget::Register(*out),
                eval_binary(op, self.value(context, lhs), self.value(context, rhs)),
            )),
            MIRInstrKind::UnOp { out, op, operand } => Some((
                MIRTarget::Register(*out),
                eval_unary(op, self.value(context, operand)),
            )),
            MIRInstrKind::Coerce {
                out,
                coercion,
                operand,
                ..
            } => Some((
                MIRTarget::Register(*out),
                eval_coercion(coercion, self.value(context, operand)),
            )),
            _ => None,
        }
    }
}

trait FactExt<T> {
    fn into_top(self) -> Fact<T>;
}

impl<T> FactExt<T> for Fact<T> {
    fn into_top(self) -> Fact<T> {
        match self {
            Fact::Known(value) => Fact::Known(value),
            Fact::Bottom | Fact::Top => Fact::Top,
        }
    }
}

#[derive(Clone, Debug)]
enum ConstValue {
    Unit,
    Bool(bool),
    Int(i128),
    Float(f64),
}

impl PartialEq for ConstValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Bool(left), Self::Bool(right)) => left == right,
            (Self::Int(left), Self::Int(right)) => left == right,
            (Self::Float(left), Self::Float(right)) => left.to_bits() == right.to_bits(),
            _ => false,
        }
    }
}

fn is_false(value: Fact<ConstValue>) -> bool {
    matches!(
        value,
        Fact::Known(ConstValue::Bool(false)) | Fact::Known(ConstValue::Int(0))
    )
}

fn constant_value(constant: &MIRConstant) -> Fact<ConstValue> {
    match constant {
        MIRConstant::Unit => Fact::Known(ConstValue::Unit),
        MIRConstant::Integer { value, .. } => Fact::Known(ConstValue::Int(*value)),
        MIRConstant::Float { value, .. } => Fact::Known(ConstValue::Float(value.into())),
        MIRConstant::Nullptr { .. } => Fact::Known(ConstValue::Int(0)),
        MIRConstant::Aggregate { .. }
        | MIRConstant::Global { .. }
        | MIRConstant::Function(_)
        | MIRConstant::Undefined => Fact::Top,
    }
}

fn known_pair(lhs: Fact<ConstValue>, rhs: Fact<ConstValue>) -> Option<(ConstValue, ConstValue)> {
    match (lhs, rhs) {
        (Fact::Known(lhs), Fact::Known(rhs)) => Some((lhs, rhs)),
        _ => None,
    }
}

fn eval_binary(op: &MIRBinaryOp, lhs: Fact<ConstValue>, rhs: Fact<ConstValue>) -> Fact<ConstValue> {
    let Some((lhs, rhs)) = known_pair(lhs, rhs) else {
        return Fact::Top;
    };
    let value = match op {
        MIRBinaryOp::Integer { op, .. } => {
            let (Some(lhs), Some(rhs)) = (as_int(lhs), as_int(rhs)) else {
                return Fact::Top;
            };
            eval_integer_binary(*op, lhs, rhs)
        }
        MIRBinaryOp::Float { op, .. } => {
            let (Some(lhs), Some(rhs)) = (as_float(lhs), as_float(rhs)) else {
                return Fact::Top;
            };
            Some(match op {
                cx_mir::MIRFloatBinaryOp::Add => ConstValue::Float(lhs + rhs),
                cx_mir::MIRFloatBinaryOp::Sub => ConstValue::Float(lhs - rhs),
                cx_mir::MIRFloatBinaryOp::Mul => ConstValue::Float(lhs * rhs),
                cx_mir::MIRFloatBinaryOp::Div => ConstValue::Float(lhs / rhs),
                cx_mir::MIRFloatBinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                cx_mir::MIRFloatBinaryOp::Ne => ConstValue::Bool(lhs != rhs),
                cx_mir::MIRFloatBinaryOp::Lt => ConstValue::Bool(lhs < rhs),
                cx_mir::MIRFloatBinaryOp::Le => ConstValue::Bool(lhs <= rhs),
                cx_mir::MIRFloatBinaryOp::Gt => ConstValue::Bool(lhs > rhs),
                cx_mir::MIRFloatBinaryOp::Ge => ConstValue::Bool(lhs >= rhs),
            })
        }
        MIRBinaryOp::PointerOffset { op, .. } => {
            let (Some(lhs), Some(rhs)) = (as_int(lhs), as_int(rhs)) else {
                return Fact::Top;
            };
            match op {
                cx_mir::MIRPointerOffsetOp::Add => lhs.checked_add(rhs).map(ConstValue::Int),
                cx_mir::MIRPointerOffsetOp::Sub => lhs.checked_sub(rhs).map(ConstValue::Int),
            }
        }
        MIRBinaryOp::Pointer(op) => {
            let (Some(lhs), Some(rhs)) = (as_int(lhs), as_int(rhs)) else {
                return Fact::Top;
            };
            Some(match op {
                MIRPointerBinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                MIRPointerBinaryOp::Ne => ConstValue::Bool(lhs != rhs),
                MIRPointerBinaryOp::Lt => ConstValue::Bool((lhs as u128) < (rhs as u128)),
                MIRPointerBinaryOp::Le => ConstValue::Bool((lhs as u128) <= (rhs as u128)),
                MIRPointerBinaryOp::Gt => ConstValue::Bool((lhs as u128) > (rhs as u128)),
                MIRPointerBinaryOp::Ge => ConstValue::Bool((lhs as u128) >= (rhs as u128)),
            })
        }
    };
    value.map_or(Fact::Top, Fact::Known)
}

fn as_int(value: ConstValue) -> Option<i128> {
    match value {
        ConstValue::Bool(value) => Some(i128::from(value)),
        ConstValue::Int(value) => Some(value),
        _ => None,
    }
}

fn as_float(value: ConstValue) -> Option<f64> {
    match value {
        ConstValue::Float(value) => Some(value),
        _ => None,
    }
}

fn eval_integer_binary(op: MIRIntBinaryOp, lhs: i128, rhs: i128) -> Option<ConstValue> {
    use MIRIntBinaryOp::*;
    match op {
        Add => lhs.checked_add(rhs).map(ConstValue::Int),
        Sub => lhs.checked_sub(rhs).map(ConstValue::Int),
        Mul | SignedMul => lhs.checked_mul(rhs).map(ConstValue::Int),
        Div | SignedDiv => (rhs != 0)
            .then(|| lhs.checked_div(rhs))
            .flatten()
            .map(ConstValue::Int),
        Mod | SignedMod => (rhs != 0)
            .then(|| lhs.checked_rem(rhs))
            .flatten()
            .map(ConstValue::Int),
        Eq => Some(ConstValue::Bool(lhs == rhs)),
        Ne => Some(ConstValue::Bool(lhs != rhs)),
        Lt => Some(ConstValue::Bool((lhs as u128) < (rhs as u128))),
        Le => Some(ConstValue::Bool((lhs as u128) <= (rhs as u128))),
        Gt => Some(ConstValue::Bool((lhs as u128) > (rhs as u128))),
        Ge => Some(ConstValue::Bool((lhs as u128) >= (rhs as u128))),
        SignedLt => Some(ConstValue::Bool(lhs < rhs)),
        SignedLe => Some(ConstValue::Bool(lhs <= rhs)),
        SignedGt => Some(ConstValue::Bool(lhs > rhs)),
        SignedGe => Some(ConstValue::Bool(lhs >= rhs)),
        LogicalAnd => Some(ConstValue::Bool(lhs != 0 && rhs != 0)),
        LogicalOr => Some(ConstValue::Bool(lhs != 0 || rhs != 0)),
        BitAnd => Some(ConstValue::Int(lhs & rhs)),
        BitOr => Some(ConstValue::Int(lhs | rhs)),
        BitXor => Some(ConstValue::Int(lhs ^ rhs)),
        ShiftLeft => (0..128)
            .contains(&rhs)
            .then(|| lhs.checked_shl(rhs as u32))
            .flatten()
            .map(ConstValue::Int),
        ArithmeticShiftRight | LogicalShiftRight => (0..128)
            .contains(&rhs)
            .then(|| Some(ConstValue::Int(lhs >> rhs as u32)))
            .flatten(),
    }
}

fn eval_unary(op: &MIRUnaryOp, operand: Fact<ConstValue>) -> Fact<ConstValue> {
    let Fact::Known(operand) = operand else {
        return Fact::Top;
    };
    let value = match op {
        MIRUnaryOp::IntegerNeg { .. } => as_int(operand)
            .and_then(|value| value.checked_neg())
            .map(ConstValue::Int),
        MIRUnaryOp::FloatNeg(_) => as_float(operand).map(|value| ConstValue::Float(-value)),
        MIRUnaryOp::BitNot(_) => as_int(operand).map(|value| ConstValue::Int(!value)),
        MIRUnaryOp::LogicalNot => match operand {
            ConstValue::Bool(value) => Some(ConstValue::Bool(!value)),
            ConstValue::Int(value) => Some(ConstValue::Bool(value == 0)),
            _ => None,
        },
        MIRUnaryOp::Increment { amount, post } => as_int(operand)
            .and_then(|value| {
                if *post {
                    Some(value)
                } else {
                    value.checked_add(i128::from(*amount))
                }
            })
            .map(ConstValue::Int),
    };
    value.map_or(Fact::Top, Fact::Known)
}

fn eval_coercion(coercion: &MIRCoercion, operand: Fact<ConstValue>) -> Fact<ConstValue> {
    let Fact::Known(operand) = operand else {
        return Fact::Top;
    };
    let value = match coercion {
        MIRCoercion::Integral {
            sign_extend, to, ..
        } => as_int(operand)
            .map(|value| {
                if *sign_extend {
                    value
                } else {
                    let bits = match to {
                        MIRIntType::I1 => 1,
                        MIRIntType::I8 => 8,
                        MIRIntType::I16 => 16,
                        MIRIntType::I32 => 32,
                        MIRIntType::I64 => 64,
                        MIRIntType::I128 => 128,
                    };
                    if bits == 128 {
                        value
                    } else {
                        value & ((1i128 << bits) - 1)
                    }
                }
            })
            .map(ConstValue::Int),
        MIRCoercion::FloatCast { .. } => as_float(operand).map(ConstValue::Float),
        MIRCoercion::IntToFloat { .. } => {
            as_int(operand).map(|value| ConstValue::Float(value as f64))
        }
        MIRCoercion::FloatToInt { .. } => {
            as_float(operand).map(|value| ConstValue::Int(value as i128))
        }
        MIRCoercion::PointerToInt { .. } | MIRCoercion::IntToPointer { .. } => Some(operand),
        MIRCoercion::FunctionToPointer => None,
        MIRCoercion::TypeChange | MIRCoercion::ReinterpretBits => Some(operand),
    };
    value.map_or(Fact::Top, Fact::Known)
}
