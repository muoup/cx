use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRConstant, MIRTypeKind, expr::intrinsic::MIRIntIntrinsic,
    ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        engine::{Engine, ExecutionFrame},
        scalar::{bits, bool_const, float_const, int_const, integer, signed},
        typing::{integer_type, target_type},
    },
    log::comptime_error,
};

pub(crate) fn execute_integer_op<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    op: &MIRIntIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRIntIntrinsic as I;

    match op {
        I::Neg { out, value } | I::BNot { out, value } | I::LNot { out, value } => {
            let (value, _) = engine
                .read(frame, value, range)
                .and_then(|v| integer(&v, range))?;
            let ty = integer_type(engine.context().types(), body, *out, range)?;

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
            let (value, source) = engine
                .read(frame, value, range)
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
            let (value, source) = engine
                .read(frame, value, range)
                .and_then(|v| integer(&v, range))?;
            let float_value = if *is_signed {
                signed(value, bits(source)) as f64
            } else {
                value as f64
            };

            engine.write(frame, out, float_const(float_value, *target))?;
        }
        I::ToPtr { out, value } => {
            let (value, _) = engine
                .read(frame, value, range)
                .and_then(|v| integer(&v, range))?;
            if value != 0 {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "convert a nonzero integer to a pointer".into(),
                    ),
                );
            }
            let ty = target_type(body, *out, range)?;
            if !matches!(
                engine
                    .context()
                    .types()
                    .definition(ty)
                    .map(|definition| definition.kind()),
                Some(MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. })
            ) {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "integer conversion to a non-pointer type".into(),
                    ),
                );
            }
            engine.write(frame, out, MIRConstant::Nullptr { ty })?;
        }
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
            let (lhs, source_ty) = engine
                .read(frame, lhs, range)
                .and_then(|v| integer(&v, range))?;
            let (rhs, _) = engine
                .read(frame, rhs, range)
                .and_then(|v| integer(&v, range))?;
            let width = bits(source_ty);
            let result_ty = integer_type(engine.context().types(), body, out, range)?;
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
