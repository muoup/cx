use std::collections::{BTreeMap, BTreeSet};

use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBasicBlock, MIRBinaryOp, MIRBlockTarget, MIRCoercion,
    MIRConstant, MIRFunction, MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRPlace,
    MIRPointerBinaryOp, MIRRegister, MIRUnaryOp, MIRUnit, MIRValue,
};

use crate::types::MIRAnalysisError;

#[derive(Clone, Debug, PartialEq)]
enum ConstValue {
    Unknown,
    Unit,
    Bool(bool),
    Int(i128),
    Float(f64),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ConstEnvironment {
    registers: BTreeMap<MIRRegister, ConstValue>,
    places: BTreeMap<MIRPlace, ConstValue>,
}

impl ConstEnvironment {
    fn value(&self, value: &MIRValue) -> ConstValue {
        match value {
            MIRValue::Register(register) => self
                .registers
                .get(register)
                .cloned()
                .unwrap_or(ConstValue::Unknown),
            MIRValue::Place(place) | MIRValue::Copy(place) | MIRValue::Move(place) => self
                .places
                .get(place)
                .cloned()
                .unwrap_or(ConstValue::Unknown),
            MIRValue::Constant(constant) => constant_value(constant),
        }
    }

    fn merge_from(&mut self, incoming: &Self) -> bool {
        let mut changed = false;
        changed |= merge_map(&mut self.registers, &incoming.registers);
        changed |= merge_map(&mut self.places, &incoming.places);
        changed
    }
}

fn merge_map<K: Ord + Copy>(
    target: &mut BTreeMap<K, ConstValue>,
    incoming: &BTreeMap<K, ConstValue>,
) -> bool {
    let keys = target
        .keys()
        .chain(incoming.keys())
        .copied()
        .collect::<BTreeSet<_>>();
    let mut changed = false;

    for key in keys {
        let value = match (target.get(&key), incoming.get(&key)) {
            (Some(left), Some(right)) if left == right => left.clone(),
            _ => ConstValue::Unknown,
        };
        if target.get(&key) != Some(&value) {
            target.insert(key, value);
            changed = true;
        }
    }

    changed
}

pub(crate) fn check(unit: &MIRUnit) -> Result<(), MIRAnalysisError> {
    for function in unit.functions() {
        if function.prototype.signature.safe {
            check_function(function)?;
        }
    }
    Ok(())
}

fn check_function(function: &MIRFunction) -> Result<(), MIRAnalysisError> {
    let Some(entry) = function.entry else {
        return Ok(());
    };
    if entry.index() >= function.blocks.len() {
        return Ok(());
    }

    let mut entries = vec![None; function.blocks.len()];
    entries[entry.index()] = Some(ConstEnvironment::default());

    loop {
        let mut changed = false;
        for block in &function.blocks {
            let Some(environment) = entries[block.id.index()].clone() else {
                continue;
            };
            let (environment, targets) = transfer_block(block, environment);
            for target in targets {
                if target.block.index() >= entries.len() {
                    continue;
                }
                let Some(target_block) = function.block(target.block) else {
                    continue;
                };
                let mut incoming = environment.clone();
                for (parameter, argument) in target_block.params.iter().zip(&target.args) {
                    incoming
                        .registers
                        .insert(*parameter, environment.value(argument));
                }

                let target_entry = &mut entries[target.block.index()];
                changed |= match target_entry {
                    Some(existing) => existing.merge_from(&incoming),
                    slot @ None => {
                        *slot = Some(incoming);
                        true
                    }
                };
            }
        }
        if !changed {
            break;
        }
    }

    for block in &function.blocks {
        let Some(mut environment) = entries[block.id.index()].clone() else {
            continue;
        };
        for (instruction_index, instruction) in block.instrs.iter().enumerate() {
            if let MIRInstrKind::Assert { condition, message } = &instruction.kind
                && is_false(&environment.value(condition))
            {
                return Err(MIRAnalysisError::ProvenFalseAssertion {
                    function: function.id,
                    block: block.id,
                    instruction: instruction_index,
                    message: message.clone(),
                });
            }
            transfer_instruction(&mut environment, &instruction.kind);
        }
    }

    Ok(())
}

fn transfer_block(
    block: &MIRBasicBlock,
    mut environment: ConstEnvironment,
) -> (ConstEnvironment, Vec<&MIRBlockTarget>) {
    for instruction in &block.instrs {
        transfer_instruction(&mut environment, &instruction.kind);
    }

    let targets = block
        .instrs
        .last()
        .map(|instruction| instruction_targets(&instruction.kind))
        .unwrap_or_default();
    (environment, targets)
}

fn instruction_targets(kind: &MIRInstrKind) -> Vec<&MIRBlockTarget> {
    match kind {
        MIRInstrKind::Jump { target } => vec![target],
        MIRInstrKind::Branch {
            true_target,
            false_target,
            ..
        } => vec![true_target, false_target],
        MIRInstrKind::IntSwitch { cases, default, .. } => cases
            .iter()
            .map(|(_, target)| target)
            .chain(default.iter())
            .collect(),
        MIRInstrKind::VariantSwitch { cases, default, .. } => cases
            .iter()
            .map(|(_, target)| target)
            .chain(default.iter())
            .collect(),
        _ => Vec::new(),
    }
}

fn transfer_instruction(environment: &mut ConstEnvironment, kind: &MIRInstrKind) {
    match kind {
        MIRInstrKind::Initialize { place }
        | MIRInstrKind::Create { out: place, .. }
        | MIRInstrKind::Dereference { out: place, .. } => {
            environment.places.insert(*place, ConstValue::Unknown);
        }
        MIRInstrKind::Assign { target, value, .. } => {
            let value = environment.value(value);
            match target {
                MIRAssignTarget::Place(dest) => {
                    environment.places.insert(*dest, value);
                }
                MIRAssignTarget::Register(out) => {
                    environment.registers.insert(*out, value);
                }
            }
        }
        MIRInstrKind::AddressOf { out, .. } => {
            environment.registers.insert(*out, ConstValue::Unknown);
        }
        MIRInstrKind::AggregateOp(operation) => match operation {
            MIRAggregateOp::Place { out, .. } => {
                environment.places.insert(*out, ConstValue::Unknown);
            }
            MIRAggregateOp::Value { out, .. } => {
                environment.registers.insert(*out, ConstValue::Unknown);
            }
        },
        MIRInstrKind::Call { out, .. } => {
            if let Some(out) = out {
                environment.registers.insert(*out, ConstValue::Unknown);
            }
        }
        MIRInstrKind::VaArg { out, .. } => {
            environment.registers.insert(*out, ConstValue::Unknown);
        }
        MIRInstrKind::BinOp { out, op, lhs, rhs } => {
            let lhs = environment.value(lhs);
            let rhs = environment.value(rhs);
            environment
                .registers
                .insert(*out, eval_binary(op, lhs, rhs));
        }
        MIRInstrKind::UnOp { out, op, operand } => {
            let operand = environment.value(operand);
            environment.registers.insert(*out, eval_unary(op, operand));
        }
        MIRInstrKind::Coerce {
            out,
            operand,
            coercion,
            ..
        } => {
            let operand = environment.value(operand);
            environment
                .registers
                .insert(*out, eval_coercion(coercion, operand));
        }
        MIRInstrKind::ScopeEnter { .. }
        | MIRInstrKind::ScopeExit { .. }
        | MIRInstrKind::Leak { .. }
        | MIRInstrKind::Assert { .. }
        | MIRInstrKind::Assume { .. }
        | MIRInstrKind::Return { .. }
        | MIRInstrKind::Jump { .. }
        | MIRInstrKind::Branch { .. }
        | MIRInstrKind::IntSwitch { .. }
        | MIRInstrKind::VariantSwitch { .. }
        | MIRInstrKind::VaStart { .. }
        | MIRInstrKind::VaEnd { .. }
        | MIRInstrKind::Unreachable
        | MIRInstrKind::Emit { .. } => {}
    }
}

fn constant_value(constant: &MIRConstant) -> ConstValue {
    match constant {
        MIRConstant::Unit => ConstValue::Unit,
        MIRConstant::Bool(value) => ConstValue::Bool(*value),
        MIRConstant::Integer { value, .. } => ConstValue::Int(*value),
        MIRConstant::Float { value, .. } => ConstValue::Float(value.into()),
        MIRConstant::Null { .. } => ConstValue::Int(0),
        MIRConstant::Aggregate { .. } => ConstValue::Unknown,
        MIRConstant::String(_)
        | MIRConstant::Global { .. }
        | MIRConstant::GlobalOffset { .. }
        | MIRConstant::Function(_)
        | MIRConstant::Undefined => ConstValue::Unknown,
    }
}

fn is_false(value: &ConstValue) -> bool {
    matches!(value, ConstValue::Bool(false) | ConstValue::Int(0))
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

fn eval_binary(op: &MIRBinaryOp, lhs: ConstValue, rhs: ConstValue) -> ConstValue {
    match op {
        MIRBinaryOp::Integer { op, .. } => {
            let (Some(lhs), Some(rhs)) = (as_int(lhs), as_int(rhs)) else {
                return ConstValue::Unknown;
            };
            eval_integer_binary(*op, lhs, rhs)
        }
        MIRBinaryOp::Float { op, .. } => {
            let (Some(lhs), Some(rhs)) = (as_float(lhs), as_float(rhs)) else {
                return ConstValue::Unknown;
            };
            match op {
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
            }
        }
        MIRBinaryOp::PointerOffset { op, .. } => {
            let (Some(lhs), Some(rhs)) = (as_int(lhs), as_int(rhs)) else {
                return ConstValue::Unknown;
            };
            match op {
                cx_mir::MIRPointerOffsetOp::Add => lhs
                    .checked_add(rhs)
                    .map(ConstValue::Int)
                    .unwrap_or(ConstValue::Unknown),
                cx_mir::MIRPointerOffsetOp::Sub => lhs
                    .checked_sub(rhs)
                    .map(ConstValue::Int)
                    .unwrap_or(ConstValue::Unknown),
            }
        }
        MIRBinaryOp::Pointer(op) => {
            let (Some(lhs), Some(rhs)) = (as_int(lhs), as_int(rhs)) else {
                return ConstValue::Unknown;
            };
            match op {
                MIRPointerBinaryOp::Eq => ConstValue::Bool(lhs == rhs),
                MIRPointerBinaryOp::Ne => ConstValue::Bool(lhs != rhs),
                MIRPointerBinaryOp::Lt => ConstValue::Bool((lhs as u128) < (rhs as u128)),
                MIRPointerBinaryOp::Le => ConstValue::Bool((lhs as u128) <= (rhs as u128)),
                MIRPointerBinaryOp::Gt => ConstValue::Bool((lhs as u128) > (rhs as u128)),
                MIRPointerBinaryOp::Ge => ConstValue::Bool((lhs as u128) >= (rhs as u128)),
            }
        }
    }
}

fn eval_integer_binary(op: MIRIntBinaryOp, lhs: i128, rhs: i128) -> ConstValue {
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
    .unwrap_or(ConstValue::Unknown)
}

fn eval_unary(op: &MIRUnaryOp, operand: ConstValue) -> ConstValue {
    match op {
        MIRUnaryOp::IntegerNeg { .. } => as_int(operand)
            .and_then(|value| value.checked_neg())
            .map(ConstValue::Int)
            .unwrap_or(ConstValue::Unknown),
        MIRUnaryOp::FloatNeg(_) => as_float(operand)
            .map(|value| ConstValue::Float(-value))
            .unwrap_or(ConstValue::Unknown),
        MIRUnaryOp::BitNot(_) => as_int(operand)
            .map(|value| ConstValue::Int(!value))
            .unwrap_or(ConstValue::Unknown),
        MIRUnaryOp::LogicalNot => match operand {
            ConstValue::Bool(value) => ConstValue::Bool(!value),
            ConstValue::Int(value) => ConstValue::Bool(value == 0),
            _ => ConstValue::Unknown,
        },
        MIRUnaryOp::Increment { amount, post } => as_int(operand)
            .and_then(|value| {
                if *post {
                    Some(value)
                } else {
                    value.checked_add(i128::from(*amount))
                }
            })
            .map(ConstValue::Int)
            .unwrap_or(ConstValue::Unknown),
    }
}

fn eval_coercion(coercion: &MIRCoercion, operand: ConstValue) -> ConstValue {
    match coercion {
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
            .map(ConstValue::Int)
            .unwrap_or(ConstValue::Unknown),
        MIRCoercion::FloatCast { .. } => as_float(operand)
            .map(ConstValue::Float)
            .unwrap_or(ConstValue::Unknown),
        MIRCoercion::IntToFloat { .. } => as_int(operand)
            .map(|value| ConstValue::Float(value as f64))
            .unwrap_or(ConstValue::Unknown),
        MIRCoercion::FloatToInt { .. } => as_float(operand)
            .map(|value| ConstValue::Int(value as i128))
            .unwrap_or(ConstValue::Unknown),
        MIRCoercion::PointerToInt { .. } | MIRCoercion::IntToPointer { .. } => operand,
        MIRCoercion::FunctionToPointer => ConstValue::Unknown,
        MIRCoercion::TypeChange | MIRCoercion::ReinterpretBits => operand,
    }
}
