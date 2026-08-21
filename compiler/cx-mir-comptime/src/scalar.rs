use cx_mir::{
    MIRBinaryOp, MIRCoercion, MIRConstant, MIRFloatBinaryOp, MIRFloatType, MIRFunctionID,
    MIRGlobalID, MIRIntBinaryOp, MIRIntType, MIRPointerBinaryOp, MIRPointerOffsetOp, MIRTypeID,
    MIRTypeKind, MIRUnaryOp, MIRUnit, ty::interface::MTRegistry,
};

pub(crate) fn binary(
    unit: &MIRUnit,
    op: &MIRBinaryOp,
    lhs: &MIRConstant,
    rhs: &MIRConstant,
    result_type: MIRTypeID,
) -> Result<MIRConstant, String> {
    match op {
        MIRBinaryOp::Integer { ty, signed, op } => integer(*ty, *signed, *op, lhs, rhs),
        MIRBinaryOp::Float { ty, op } => float(*ty, *op, lhs, rhs),
        MIRBinaryOp::PointerOffset { op, pointee } => {
            pointer_offset(unit, *op, *pointee, result_type, lhs, rhs)
        }
        MIRBinaryOp::Pointer(op) => pointer(*op, lhs, rhs),
    }
}

pub(crate) fn unary(op: &MIRUnaryOp, operand: &MIRConstant) -> Result<MIRConstant, String> {
    match op {
        MIRUnaryOp::IntegerNeg { ty, signed } => {
            let value = integer_bits(operand)?;
            Ok(integer_constant(0_u128.wrapping_sub(value), *ty, *signed))
        }
        MIRUnaryOp::FloatNeg(ty) => Ok(MIRConstant::Float {
            value: float_value(operand).map(|value| (-value).into())?,
            ty: *ty,
        }),
        MIRUnaryOp::BitNot(ty) => Ok(integer_constant(
            !integer_bits(operand)? & mask(*ty),
            *ty,
            false,
        )),
        MIRUnaryOp::LogicalNot => Ok(MIRConstant::Integer {
            value: i128::from(integer_bits(operand)? == 0),
            ty: MIRIntType::I1,
            signed: false,
        }),
        MIRUnaryOp::Increment { amount, post } => {
            let value = integer_bits(operand)?;
            let updated = if *amount >= 0 {
                value.wrapping_add(u128::from(*amount as u8))
            } else {
                value.wrapping_sub(u128::from(amount.unsigned_abs()))
            };
            let MIRConstant::Integer { ty, signed, .. } = operand else {
                return Err(format!("increment operand {operand:?} is not an integer"));
            };
            Ok(integer_constant(
                if *post { value } else { updated },
                *ty,
                *signed,
            ))
        }
    }
}

pub(crate) fn coerce(
    unit: &MIRUnit,
    coercion: MIRCoercion,
    operand: &MIRConstant,
    to_type: MIRTypeID,
) -> Result<MIRConstant, String> {
    match coercion {
        MIRCoercion::Integral {
            sign_extend, to, ..
        } => Ok(integer_constant(integer_bits(operand)?, to, sign_extend)),
        MIRCoercion::FloatCast { to, .. } => Ok(MIRConstant::Float {
            value: float_cast(float_value(operand)?, to),
            ty: to,
        }),
        MIRCoercion::IntToFloat { to, signed, .. } => {
            let bits = integer_bits(operand)?;
            let value = if signed {
                signed_value(bits, integer_type(operand)?) as f64
            } else {
                bits as f64
            };
            Ok(MIRConstant::Float {
                value: float_cast(value, to),
                ty: to,
            })
        }
        MIRCoercion::FloatToInt { to, signed, .. } => Ok(integer_constant(
            float_value(operand)? as i128 as u128,
            to,
            signed,
        )),
        MIRCoercion::PointerToInt { to } => {
            let value = match operand {
                MIRConstant::Null { .. } => 0,
                _ => {
                    return Err(format!(
                        "cannot convert symbolic pointer {operand:?} to integer"
                    ));
                }
            };
            Ok(integer_constant(value, to, false))
        }
        MIRCoercion::IntToPointer { .. } => {
            if integer_bits(operand)? == 0 {
                Ok(MIRConstant::Null { ty: to_type })
            } else {
                Err(
                    "non-null integer-to-pointer conversion is not representable in MIR comptime"
                        .to_owned(),
                )
            }
        }
        MIRCoercion::FunctionToPointer | MIRCoercion::TypeChange | MIRCoercion::ReinterpretBits => {
            retag(unit, operand, to_type)
        }
    }
}

fn integer(
    ty: MIRIntType,
    signed: bool,
    op: MIRIntBinaryOp,
    lhs: &MIRConstant,
    rhs: &MIRConstant,
) -> Result<MIRConstant, String> {
    let left = integer_bits(lhs)?;
    let right = integer_bits(rhs)?;
    let signed_operation = signed
        || matches!(
            op,
            MIRIntBinaryOp::SignedMul | MIRIntBinaryOp::SignedDiv | MIRIntBinaryOp::SignedMod
        );
    let value = match op {
        MIRIntBinaryOp::Add => integer_constant(left.wrapping_add(right), ty, signed),
        MIRIntBinaryOp::Sub => integer_constant(left.wrapping_sub(right), ty, signed),
        MIRIntBinaryOp::Mul | MIRIntBinaryOp::SignedMul => {
            integer_constant(left.wrapping_mul(right), ty, signed_operation)
        }
        MIRIntBinaryOp::Div | MIRIntBinaryOp::SignedDiv => {
            if right == 0 {
                return Err("integer division by zero".to_owned());
            }
            if signed_operation {
                let left = signed_value(left, ty);
                let right = signed_value(right, ty);
                integer_constant(
                    left.checked_div(right)
                        .ok_or_else(|| "signed integer division overflow".to_owned())?
                        as u128,
                    ty,
                    signed_operation,
                )
            } else {
                integer_constant(left / right, ty, false)
            }
        }
        MIRIntBinaryOp::Mod | MIRIntBinaryOp::SignedMod => {
            if right == 0 {
                return Err("integer remainder by zero".to_owned());
            }
            if signed_operation {
                let left = signed_value(left, ty);
                let right = signed_value(right, ty);
                integer_constant(
                    left.checked_rem(right)
                        .ok_or_else(|| "signed integer remainder overflow".to_owned())?
                        as u128,
                    ty,
                    signed_operation,
                )
            } else {
                integer_constant(left % right, ty, false)
            }
        }
        MIRIntBinaryOp::Eq => comparison(left == right),
        MIRIntBinaryOp::Ne => comparison(left != right),
        MIRIntBinaryOp::Lt => comparison(compare(left, right, ty, signed_operation).is_lt()),
        MIRIntBinaryOp::Le => comparison(compare(left, right, ty, signed_operation).is_le()),
        MIRIntBinaryOp::Gt => comparison(compare(left, right, ty, signed_operation).is_gt()),
        MIRIntBinaryOp::Ge => comparison(compare(left, right, ty, signed_operation).is_ge()),
        MIRIntBinaryOp::SignedLt => comparison(signed_value(left, ty) < signed_value(right, ty)),
        MIRIntBinaryOp::SignedLe => comparison(signed_value(left, ty) <= signed_value(right, ty)),
        MIRIntBinaryOp::SignedGt => comparison(signed_value(left, ty) > signed_value(right, ty)),
        MIRIntBinaryOp::SignedGe => comparison(signed_value(left, ty) >= signed_value(right, ty)),
        MIRIntBinaryOp::LogicalAnd => comparison(left != 0 && right != 0),
        MIRIntBinaryOp::LogicalOr => comparison(left != 0 || right != 0),
        MIRIntBinaryOp::BitAnd => integer_constant(left & right, ty, signed),
        MIRIntBinaryOp::BitOr => integer_constant(left | right, ty, signed),
        MIRIntBinaryOp::BitXor => integer_constant(left ^ right, ty, signed),
        MIRIntBinaryOp::ShiftLeft => shift(left, right, ty, signed, true, false)?,
        MIRIntBinaryOp::ArithmeticShiftRight => shift(left, right, ty, signed, false, true)?,
        MIRIntBinaryOp::LogicalShiftRight => shift(left, right, ty, false, false, false)?,
    };
    Ok(value)
}

fn float(
    ty: MIRFloatType,
    op: MIRFloatBinaryOp,
    lhs: &MIRConstant,
    rhs: &MIRConstant,
) -> Result<MIRConstant, String> {
    let left = float_value(lhs)?;
    let right = float_value(rhs)?;
    let value = match op {
        MIRFloatBinaryOp::Add => left + right,
        MIRFloatBinaryOp::Sub => left - right,
        MIRFloatBinaryOp::Mul => left * right,
        MIRFloatBinaryOp::Div => left / right,
        MIRFloatBinaryOp::Eq => return Ok(comparison(left == right)),
        MIRFloatBinaryOp::Ne => return Ok(comparison(left != right)),
        MIRFloatBinaryOp::Lt => return Ok(comparison(left < right)),
        MIRFloatBinaryOp::Le => return Ok(comparison(left <= right)),
        MIRFloatBinaryOp::Gt => return Ok(comparison(left > right)),
        MIRFloatBinaryOp::Ge => return Ok(comparison(left >= right)),
    };
    Ok(MIRConstant::Float {
        value: float_cast(value, ty),
        ty,
    })
}

fn pointer_offset(
    unit: &MIRUnit,
    op: MIRPointerOffsetOp,
    pointee: MIRTypeID,
    result_type: MIRTypeID,
    lhs: &MIRConstant,
    rhs: &MIRConstant,
) -> Result<MIRConstant, String> {
    let index = match rhs {
        MIRConstant::Bool(value) => i128::from(*value),
        MIRConstant::Integer {
            value, ty, signed, ..
        } => {
            let bits = *value as u128 & mask(*ty);
            if *signed {
                signed_value(bits, *ty)
            } else {
                bits as i128
            }
        }
        _ => return Err(format!("pointer offset index {rhs:?} is not an integer")),
    };
    let size = cx_mir::ty::layout::layout_of(unit.types(), pointee)
        .map_err(|error| format!("invalid pointer offset pointee {pointee}: {error}"))?
        .size;
    let offset = index
        .checked_mul(i128::try_from(size).map_err(|_| "pointer offset is too large".to_owned())?)
        .ok_or_else(|| "pointer offset overflows".to_owned())?;
    let offset = match op {
        MIRPointerOffsetOp::Add => offset,
        MIRPointerOffsetOp::Sub => -offset,
    };
    match lhs {
        MIRConstant::Global { global, .. } => Ok(MIRConstant::GlobalOffset {
            global: *global,
            offset: i64::try_from(offset).map_err(|_| "pointer offset overflows i64".to_owned())?,
            ty: result_type,
        }),
        MIRConstant::GlobalOffset {
            global,
            offset: base,
            ..
        } => Ok(MIRConstant::GlobalOffset {
            global: *global,
            offset: base
                .checked_add(
                    i64::try_from(offset).map_err(|_| "pointer offset overflows i64".to_owned())?,
                )
                .ok_or_else(|| "pointer offset overflows i64".to_owned())?,
            ty: result_type,
        }),
        _ => Err(format!(
            "pointer offset base {lhs:?} is not a global address"
        )),
    }
}

fn pointer(
    op: MIRPointerBinaryOp,
    lhs: &MIRConstant,
    rhs: &MIRConstant,
) -> Result<MIRConstant, String> {
    let equal = pointer_identity(lhs)? == pointer_identity(rhs)?;
    let value = match op {
        MIRPointerBinaryOp::Eq => equal,
        MIRPointerBinaryOp::Ne => !equal,
        _ => return Err("ordered comparison of symbolic pointers is not supported".to_owned()),
    };
    Ok(comparison(value))
}

#[derive(Debug, PartialEq, Eq)]
enum PointerIdentity {
    Null,
    Global(MIRGlobalID, i64),
    Function(MIRFunctionID),
}

fn pointer_identity(value: &MIRConstant) -> Result<PointerIdentity, String> {
    match value {
        MIRConstant::Null { .. } => Ok(PointerIdentity::Null),
        MIRConstant::Global { global, .. } => Ok(PointerIdentity::Global(*global, 0)),
        MIRConstant::GlobalOffset { global, offset, .. } => {
            Ok(PointerIdentity::Global(*global, *offset))
        }
        MIRConstant::Function(function) => Ok(PointerIdentity::Function(*function)),
        _ => Err(format!("value {value:?} is not a symbolic pointer")),
    }
}

fn retag(unit: &MIRUnit, operand: &MIRConstant, to_type: MIRTypeID) -> Result<MIRConstant, String> {
    let kind = unit
        .types()
        .kind(to_type)
        .map_err(|error| format!("invalid coercion target {to_type}: {error}"))?;
    match (operand, kind) {
        (MIRConstant::Integer { value, .. }, MIRTypeKind::Integer { ty, signed }) => {
            Ok(integer_constant(*value as u128, *ty, *signed))
        }
        (MIRConstant::Bool(value), MIRTypeKind::Integer { ty, signed }) => {
            Ok(integer_constant(u128::from(*value), *ty, *signed))
        }
        (MIRConstant::Float { value, .. }, MIRTypeKind::Float { ty }) => Ok(MIRConstant::Float {
            value: float_cast(f64::from(value), *ty),
            ty: *ty,
        }),
        (
            MIRConstant::Null { .. },
            MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. },
        ) => Ok(MIRConstant::Null { ty: to_type }),
        (MIRConstant::Global { global, .. }, _) => Ok(MIRConstant::Global {
            global: *global,
            ty: to_type,
        }),
        (MIRConstant::GlobalOffset { global, offset, .. }, _) => Ok(MIRConstant::GlobalOffset {
            global: *global,
            offset: *offset,
            ty: to_type,
        }),
        (MIRConstant::Aggregate { fields, .. }, _) => Ok(MIRConstant::Aggregate {
            ty: to_type,
            fields: fields.clone(),
        }),
        (MIRConstant::Function(function), _) => Ok(MIRConstant::Function(*function)),
        (MIRConstant::Unit, _) => Ok(MIRConstant::Unit),
        _ => Err(format!("cannot retag {operand:?} as {to_type}")),
    }
}

fn integer_bits(value: &MIRConstant) -> Result<u128, String> {
    match value {
        MIRConstant::Bool(value) => Ok(u128::from(*value)),
        MIRConstant::Integer { value, ty, .. } => Ok(*value as u128 & mask(*ty)),
        _ => Err(format!("value {value:?} is not an integer")),
    }
}

fn integer_type(value: &MIRConstant) -> Result<MIRIntType, String> {
    match value {
        MIRConstant::Integer { ty, .. } => Ok(*ty),
        MIRConstant::Bool(_) => Ok(MIRIntType::I1),
        _ => Err(format!("value {value:?} is not an integer")),
    }
}

fn float_value(value: &MIRConstant) -> Result<f64, String> {
    match value {
        MIRConstant::Float { value, .. } => Ok(f64::from(value)),
        _ => Err(format!("value {value:?} is not a float")),
    }
}

fn integer_constant(value: u128, ty: MIRIntType, signed: bool) -> MIRConstant {
    let value = value & mask(ty);
    MIRConstant::Integer {
        value: if signed {
            signed_value(value, ty)
        } else {
            value as i128
        },
        ty,
        signed,
    }
}

fn comparison(value: bool) -> MIRConstant {
    MIRConstant::Integer {
        value: i128::from(value),
        ty: MIRIntType::I1,
        signed: false,
    }
}

fn mask(ty: MIRIntType) -> u128 {
    match ty {
        MIRIntType::I1 => 1,
        MIRIntType::I8 => u8::MAX.into(),
        MIRIntType::I16 => u16::MAX.into(),
        MIRIntType::I32 => u32::MAX.into(),
        MIRIntType::I64 => u64::MAX.into(),
        MIRIntType::I128 => u128::MAX,
    }
}

fn signed_value(value: u128, ty: MIRIntType) -> i128 {
    let value = value & mask(ty);
    let bits = match ty {
        MIRIntType::I1 => 1,
        MIRIntType::I8 => 8,
        MIRIntType::I16 => 16,
        MIRIntType::I32 => 32,
        MIRIntType::I64 => 64,
        MIRIntType::I128 => 128,
    };
    if bits == 128 {
        value as i128
    } else if value & (1_u128 << (bits - 1)) != 0 {
        (value | (!mask(ty))) as i128
    } else {
        value as i128
    }
}

fn compare(left: u128, right: u128, ty: MIRIntType, signed: bool) -> std::cmp::Ordering {
    if signed {
        signed_value(left, ty).cmp(&signed_value(right, ty))
    } else {
        left.cmp(&right)
    }
}

fn shift(
    left: u128,
    right: u128,
    ty: MIRIntType,
    signed: bool,
    left_shift: bool,
    arithmetic: bool,
) -> Result<MIRConstant, String> {
    let bits = match ty {
        MIRIntType::I1 => 1,
        _ => ty.bytes() * 8,
    };
    if right >= u128::from(bits as u64) {
        return Err("integer shift amount is out of range".to_owned());
    }
    let value = if left_shift {
        left << right as u32
    } else if arithmetic {
        (signed_value(left, ty) >> right as u32) as u128
    } else {
        left >> right as u32
    };
    Ok(integer_constant(value, ty, signed))
}

fn float_cast(value: f64, ty: MIRFloatType) -> cx_util::unsafe_float::FloatWrapper {
    match ty {
        MIRFloatType::F32 => (value as f32 as f64).into(),
        MIRFloatType::F64 => value.into(),
    }
}
