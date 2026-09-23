use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRConstant, MIRFloatType, MIRIntType, MIRTarget, MIRTypeKind, MIRValue,
    expr::intrinsic::{MIRFloatIntrinsic, MIRIntIntrinsic, MIRIntrinsic},
    ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    engine::{Engine, ExecutionFrame},
    log::comptime_error,
};

pub fn truthy(value: &MIRConstant) -> bool {
    match value {
        MIRConstant::Integer { value, .. } => *value != 0,
        MIRConstant::Float { value, .. } => f64::from(value) != 0.0,
        MIRConstant::Nullptr { .. } | MIRConstant::Undefined => false,
        _ => true,
    }
}

fn bits(ty: MIRIntType) -> u32 {
    match ty {
        MIRIntType::I1 => 1,
        _ => (ty.bytes() * 8) as u32,
    }
}

fn mask(value: u128, width: u32) -> u128 {
    if width == 128 {
        value
    } else {
        value & ((1u128 << width) - 1)
    }
}

fn signed(value: u128, width: u32) -> i128 {
    if width == 128 {
        value as i128
    } else {
        let shift = 128 - width;
        ((value << shift) as i128) >> shift
    }
}

fn integer(value: &MIRConstant, range: &TokenRange) -> CXResult<(u128, MIRIntType)> {
    match value {
        MIRConstant::Integer { value, ty } => Ok((mask(*value as u128, bits(*ty)), *ty)),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-integer arithmetic operand".into(),
            ),
        ),
    }
}

fn float(value: &MIRConstant, range: &TokenRange) -> CXResult<f64> {
    match value {
        MIRConstant::Float { value, .. } => Ok(f64::from(value)),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-float arithmetic operand".into(),
            ),
        ),
    }
}

fn target_kind<'a, R: MTRegistry>(
    body: &MIRComptimeBody<'_>,
    registry: &'a R,
    target: MIRTarget,
    _range: &TokenRange,
) -> CXResult<&'a MIRTypeKind> {
    let ty = match target {
        MIRTarget::Place(id) => body.place(id).map(|place| place.ty),
        MIRTarget::Register(id) => body.register(id).map(|register| register.ty),
        MIRTarget::Global(_) | MIRTarget::Indirect(_) => None,
    };
    ty.and_then(|ty| registry.definition(ty))
        .map(|ty| ty.kind())
        .ok_or_else(|| {
            crate::log::internal_error(
                &mir::COMPTIME_INVALID_OPERATION,
                "unknown arithmetic output type".into(),
                "comptime arithmetic",
            )
        })
}

fn result_int<R: MTRegistry>(
    body: &MIRComptimeBody<'_>,
    registry: &R,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRIntType> {
    match target_kind(body, registry, target, range)? {
        MIRTypeKind::Integer { ty, .. } => Ok(*ty),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-integer arithmetic result".into(),
            ),
        ),
    }
}

fn result_float<R: MTRegistry>(
    body: &MIRComptimeBody<'_>,
    registry: &R,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRFloatType> {
    match target_kind(body, registry, target, range)? {
        MIRTypeKind::Float { ty } => Ok(*ty),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-float arithmetic result".into(),
            ),
        ),
    }
}

fn int_const(value: u128, ty: MIRIntType) -> MIRConstant {
    MIRConstant::Integer {
        value: mask(value, bits(ty)) as i128,
        ty,
    }
}

fn bool_const(value: bool) -> MIRConstant {
    MIRConstant::Integer {
        value: value as i128,
        ty: MIRIntType::I1,
    }
}

fn float_const(value: f64, ty: MIRFloatType) -> MIRConstant {
    let value = if ty == MIRFloatType::F32 {
        (value as f32) as f64
    } else {
        value
    };
    MIRConstant::Float {
        value: value.into(),
        ty,
    }
}

pub(crate) fn execute_integer_op<'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    op: &MIRIntIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRIntIntrinsic as I;

    match op {
        I::Neg { out, value } | I::BNot { out, value } | I::LNot { out, value } => {
            let (value, _) = engine.read(frame, value, range)
                .and_then(|v| integer(&v, range))?;
            let ty = result_int(body, engine.context().types(), *out, range)?;
            
            let result = match op {
                I::Neg { .. } => int_const(value.wrapping_neg(), ty),
                I::BNot { .. } => int_const(!value, ty),
                _ => bool_const(value == 0),
            };

            engine.write(frame, out, result)?;
        }
        
        I::IntCast {
            out,
            value,
            target,
            sign_extend,
        } => {
            let (value, source) = engine.read(frame, value, range)
                .and_then(|v| integer(&v, range))?;
            let result = if *sign_extend {
                signed(value, bits(source)) as u128
            } else {
                value
            };
            
            engine.write(frame, out, int_const(result, *target))?;
        }
        I::ToFloat {
            out,
            value,
            target,
            signed: is_signed,
        } => {
            let (value, source) = engine.read(frame, value, range)
                .and_then(|v| integer(&v, range))?;
            let float_value = if *is_signed {
                signed(value, bits(source)) as f64
            } else {
                value as f64
            };
            
            engine.write(frame, out, float_const(float_value, *target))?;
        }
        I::ToPtr { .. } => return comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "pointer arithmetic is not supported".into(),
            ),
        ),
        _ => {
            let (out, lhs, rhs) = match op {
                I::Add { out, lhs, rhs }
                | I::Sub { out, lhs, rhs }
                | I::UMul { out, lhs, rhs }
                | I::SMul { out, lhs, rhs }
                | I::UDiv { out, lhs, rhs }
                | I::SDiv { out, lhs, rhs }
                | I::UMod { out, lhs, rhs }
                | I::SMod { out, lhs, rhs }
                | I::Eq { out, lhs, rhs }
                | I::Neq { out, lhs, rhs }
                | I::ULt { out, lhs, rhs }
                | I::SLt { out, lhs, rhs }
                | I::ULe { out, lhs, rhs }
                | I::SLe { out, lhs, rhs }
                | I::UGt { out, lhs, rhs }
                | I::SGt { out, lhs, rhs }
                | I::UGe { out, lhs, rhs }
                | I::SGe { out, lhs, rhs }
                | I::LAnd { out, lhs, rhs }
                | I::LOr { out, lhs, rhs }
                | I::BAnd { out, lhs, rhs }
                | I::BOr { out, lhs, rhs }
                | I::BXor { out, lhs, rhs }
                | I::LShift { out, lhs, rhs }
                | I::ARShift { out, lhs, rhs }
                | I::LRShift { out, lhs, rhs } => (*out, lhs, rhs),
                _ => unreachable!(),
            };
            let (lhs, source_ty) = engine.read(frame, lhs, range)
                .and_then(|v| integer(&v, range))?;
            let (rhs, _) = engine.read(frame, rhs, range)
                .and_then(|v| integer(&v, range))?;
            let width = bits(source_ty);
            let result_ty = result_int(body, engine.context().types(), out, range)?;
            let result = match op {
                I::Add { .. } => int_const(lhs.wrapping_add(rhs), result_ty),
                I::Sub { .. } => int_const(lhs.wrapping_sub(rhs), result_ty),
                I::UMul { .. } | I::SMul { .. } => int_const(lhs.wrapping_mul(rhs), result_ty),
                I::UDiv { .. } | I::UMod { .. } | I::SDiv { .. } | I::SMod { .. } => {
                    if rhs == 0 {
                        return comptime_error(
                            range.clone(),
                            (&mir::COMPTIME_ZERO_DIVISOR, "integer arithmetic".into()),
                        );
                    }
                    let value = match op {
                        I::UDiv { .. } => lhs / rhs,
                        I::UMod { .. } => lhs % rhs,
                        I::SDiv { .. } => {
                            signed(lhs, width).wrapping_div(signed(rhs, width)) as u128
                        }
                        _ => signed(lhs, width).wrapping_rem(signed(rhs, width)) as u128,
                    };
                    int_const(value, result_ty)
                }
                I::Eq { .. } => bool_const(lhs == rhs),
                I::Neq { .. } => bool_const(lhs != rhs),
                I::ULt { .. } => bool_const(lhs < rhs),
                I::SLt { .. } => bool_const(signed(lhs, width) < signed(rhs, width)),
                I::ULe { .. } => bool_const(lhs <= rhs),
                I::SLe { .. } => bool_const(signed(lhs, width) <= signed(rhs, width)),
                I::UGt { .. } => bool_const(lhs > rhs),
                I::SGt { .. } => bool_const(signed(lhs, width) > signed(rhs, width)),
                I::UGe { .. } => bool_const(lhs >= rhs),
                I::SGe { .. } => bool_const(signed(lhs, width) >= signed(rhs, width)),
                I::LAnd { .. } => bool_const(lhs != 0 && rhs != 0),
                I::LOr { .. } => bool_const(lhs != 0 || rhs != 0),
                I::BAnd { .. } => int_const(lhs & rhs, result_ty),
                I::BOr { .. } => int_const(lhs | rhs, result_ty),
                I::BXor { .. } => int_const(lhs ^ rhs, result_ty),
                I::LShift { .. } | I::ARShift { .. } | I::LRShift { .. } => {
                    if rhs >= width as u128 {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "shift count exceeds width".into(),
                            ),
                        );
                    }
                    let value = match op {
                        I::LShift { .. } => lhs << rhs,
                        I::ARShift { .. } => (signed(lhs, width) >> rhs) as u128,
                        _ => lhs >> rhs,
                    };
                    int_const(value, result_ty)
                }
                _ => unreachable!(),
            };

            engine.write(frame, &out, result)?;
        }
    }

    Ok(())
}