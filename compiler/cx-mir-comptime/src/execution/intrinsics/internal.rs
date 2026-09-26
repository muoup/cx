use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRConstant, MIRInternalIntrinsic, MIRTypeKind, ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        engine::{Engine, ExecutionFrame},
        scalar,
        typing::target_type,
    },
    log::comptime_error,
};

pub(super) fn execute<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    op: &MIRInternalIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRInternalIntrinsic as I;
    match op {
        I::GlobalAddress { out, global } => {
            engine.write(frame, out, MIRConstant::GlobalRef(*global))
        }
        I::ArrayAddress { out, array } => {
            let value = engine.read(frame, body, array, range)?;
            let value = match value {
                MIRConstant::GlobalRef(mut reference) => {
                    let types = engine.context().types();
                    let result_ty = target_type(body, *out, range)?;
                    let Some(inner) = types.definition(result_ty).and_then(|ty| {
                        types
                            .pointer_inner(ty)
                            .or_else(|| types.reference_inner(ty))
                    }) else {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "array address has a non-pointer result".into(),
                            ),
                        );
                    };
                    reference.ty = inner;
                    MIRConstant::GlobalRef(reference)
                }
                value => value,
            };
            engine.write(frame, out, value)
        }
        I::ReferenceAddress {
            out,
            reference: array,
        } => {
            let value = engine.read(frame, body, array, range)?;
            engine.write(frame, out, value)
        }
        I::StringAddress { out, string } => {
            engine.write(frame, out, MIRConstant::String(string.clone()))
        }
        I::GetFnPtr { out, fn_id } => engine.write(frame, out, MIRConstant::Function(*fn_id)),
        I::Bitcast {
            out,
            value,
            target_ty,
        } => {
            let value = engine.read(frame, body, value, range)?;
            let value = match value {
                MIRConstant::GlobalRef(mut reference) => {
                    reference.ty = match engine
                        .context()
                        .types()
                        .definition(*target_ty)
                        .map(|ty| ty.kind())
                    {
                        Some(
                            MIRTypeKind::PointerTo { inner }
                            | MIRTypeKind::MemoryReference { inner, .. },
                        ) => *inner,
                        _ => *target_ty,
                    };
                    MIRConstant::GlobalRef(reference)
                }
                value => value,
            };
            engine.write(frame, out, value)
        }
        I::PlaceAddress { .. } | I::AdoptPlace { .. } => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "take the address of a local comptime place".into(),
            ),
        ),
        I::Assert { condition, message } => {
            let condition = engine.read(frame, body, condition, range)?;
            if !scalar::truthy(&condition) {
                return comptime_error(range.clone(), (&mir::COMPTIME_ASSERTION, message.clone()));
            }
            Ok(())
        }
        I::Assume { condition } => {
            let _ = engine.read(frame, body, condition, range)?;
            Ok(())
        }
    }
}
