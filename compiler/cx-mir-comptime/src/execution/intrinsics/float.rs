use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRConstant, MIRFloatIntrinsic, MIRTypeKind, MIRValue,
    ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        engine::{Engine, ExecutionFrame},
        scalar::{bool_const, float_const, float_value, mask_integer},
        typing::float_type,
    },
    log::comptime_error,
};

pub(super) fn execute<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    op: &MIRFloatIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRFloatIntrinsic as F;
    let read = |value: &MIRValue| engine.read(frame, body, value, range);
    let (out, value) = match op {
        F::Neg { out, value } => {
            let value = -read(value).and_then(|value| float_value(value, range))?;
            engine.write(
                frame,
                out,
                float_const(
                    value,
                    float_type(engine.context().types(), body, *out, range)?,
                ),
            )?;
            return Ok(());
        }
        F::ToInt {
            out,
            value,
            target_ty,
        } => {
            let value = read(value).and_then(|value| float_value(value, range))?;
            let Some(MIRTypeKind::Integer { ty, signed }) = engine
                .context()
                .types()
                .definition(*target_ty)
                .map(|ty| ty.kind())
            else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "float conversion to a non-integer type".into(),
                    ),
                );
            };
            let value = if *signed {
                value.trunc() as i128
            } else {
                value.trunc() as u128 as i128
            };
            let value = mask_integer(value, *ty);
            engine.write(frame, out, MIRConstant::Integer { value, ty: *ty })?;
            return Ok(());
        }
        F::FloatCast {
            out,
            value,
            float_ty,
        } => {
            let value = read(value).and_then(|value| float_value(value, range))?;
            engine.write(frame, out, float_const(value, *float_ty))?;
            return Ok(());
        }
        F::Add { out, lhs, rhs }
        | F::Sub { out, lhs, rhs }
        | F::Mul { out, lhs, rhs }
        | F::Div { out, lhs, rhs }
        | F::Eq { out, lhs, rhs }
        | F::Neq { out, lhs, rhs }
        | F::Lt { out, lhs, rhs }
        | F::Le { out, lhs, rhs }
        | F::Gt { out, lhs, rhs }
        | F::Geq { out, lhs, rhs } => (*out, (lhs, rhs)),
    };
    let lhs = read(value.0).and_then(|value| float_value(value, range))?;
    let rhs = read(value.1).and_then(|value| float_value(value, range))?;
    let result = match op {
        F::Add { .. } => float_const(
            lhs + rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Sub { .. } => float_const(
            lhs - rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Mul { .. } => float_const(
            lhs * rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Div { .. } => float_const(
            lhs / rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Eq { .. } => bool_const(lhs == rhs),
        F::Neq { .. } => bool_const(lhs != rhs),
        F::Lt { .. } => bool_const(lhs < rhs),
        F::Le { .. } => bool_const(lhs <= rhs),
        F::Gt { .. } => bool_const(lhs > rhs),
        F::Geq { .. } => bool_const(lhs >= rhs),
        _ => unreachable!(),
    };
    engine.write(frame, &out, result)
}
