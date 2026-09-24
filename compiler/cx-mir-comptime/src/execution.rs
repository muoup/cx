use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBindable, MIRComptimeBody, MIRComptimeOp, MIRComptimeOutput, MIRComptimeValue, MIRConstant, MIRInstruction, MIRInstructionKind, MIRIntrinsic, MIRStagedExpression, MIRValue, expr::instruction::MIRInvalidationKind
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext, arithmetic::{self, execute_integer_op}, engine::{Engine, ExecutionFrame}, log::comptime_error,
};

pub(crate) fn execute_runtime_instruction<'c, 'thir, Context: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, Context>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    instruction: &MIRInstruction,
) -> CXResult<Option<MIRComptimeValue>> {
    let range = &instruction.token_range;
    match &instruction.kind {
        MIRInstructionKind::Initialize { .. } | MIRInstructionKind::BindLifetime { .. } => {}
        MIRInstructionKind::Invalidate { place, kind } => {
            if *kind != MIRInvalidationKind::Drop {
                engine.read_bindable(frame, place, range)?;
            }
            match place {
                MIRBindable::Place(id) => {
                    frame.places_mut().remove(id);
                }
                MIRBindable::Register(id) => {
                    frame.registers_mut().remove(id);
                }
            }
        }
        MIRInstructionKind::LiftPlace { out, place } => {
            let value = engine.read(frame, &MIRValue::PlaceRef(*place), range)?;
            frame.registers_mut().insert(*out, value);
        }
        MIRInstructionKind::Store { target, value, .. } => {
            let value = engine.read(frame, value, range)?;
            engine.write(frame, target, value)?;
        }
        MIRInstructionKind::Call { .. } => {
            return comptime_error(
                range.clone(),
                (&mir::COMPTIME_INVALID_OPERATION, "runtime call".into()),
            );
        }
        MIRInstructionKind::IntrinsicOp(intrinsic) => {
            execute_intrinsic(engine, frame, body, intrinsic, range)?;
        }
        MIRInstructionKind::Return { value } => {
            return Ok(Some(match value {
                Some(value) => MIRComptimeValue::Constant(engine.read(frame, value, range)?),
                None => MIRComptimeValue::Constant(MIRConstant::Unit),
            }));
        }
        MIRInstructionKind::Jump { target } => engine.jump(body, frame, target, range)?,
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        } => {
            let cond = engine.read(frame, cond, range)?;
            let target = if arithmetic::truthy(&cond) {
                true_target
            } else {
                false_target
            };
            engine.jump(body, frame, target, range)?;
        }
        MIRInstructionKind::CaseBranch {
            value,
            cases,
            default,
        } => {
            let value = engine.read(frame, value, range)?;
            let target = cases.iter().find(|(case, _)|
                    matches!(&value, MIRConstant::Integer { value, .. } if value == case))
                    .map(|(_, target)| target)
                    .or(default.as_ref());
            let Some(target) = target else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "nonexhaustive comptime switch".into(),
                    ),
                );
            };
            engine.jump(body, frame, target, range)?;
        }
        MIRInstructionKind::Unreachable => {
            return comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "unreachable instruction".into(),
                ),
            );
        }
    }
    Ok(None)
}

pub(crate) fn execute_comptime_instruction<'c, 'thir, Context: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, Context>,
    frame: &mut ExecutionFrame,
    op: &MIRComptimeOp<'thir>,
    range: &TokenRange,
) -> CXResult<Option<MIRComptimeValue>> {
    match op {
        MIRComptimeOp::Call { out, callee, args } => {
            let function = engine.context().function(*callee).ok_or_else(|| {
                crate::log::internal_error(
                    &mir::COMPTIME_INVALID_OPERATION,
                    "unknown comptime function".into(),
                    "comptime execution",
                )
            })?;

            let body = function.body().ok_or_else(|| {
                crate::log::internal_error(
                    &mir::COMPTIME_INVALID_OPERATION,
                    "undefined comptime function".into(),
                    "comptime execution",
                )
            })?;

            let args = args
                .iter()
                .zip(function.prototype().signature().params())
                .map(|(operand, _)| engine.read_comptime(frame, operand, range))
                .collect::<CXResult<Vec<_>>>()?;

            let result = engine.run(body, &args)?;
            match out {
                Some(MIRComptimeOutput::Runtime(out)) => {
                    let MIRComptimeValue::Constant(value) = result else {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "non-concrete comptime call result".into(),
                            ),
                        );
                    };
                    frame.registers_mut().insert(*out, value);
                }
                Some(MIRComptimeOutput::Comptime(out)) => {
                    frame.comptime_registers_mut().insert(*out, result);
                }
                None => {}
            }
            Ok(None)
        }

        MIRComptimeOp::Emit {
            out,
            expression,
            parameters,
            captures,
        } => {
            let captures = captures
                .iter()
                .map(|(id, operand)| {
                    engine
                        .read_comptime(frame, operand, range)
                        .map(|value| (*id, value))
                })
                .collect::<CXResult<_>>()?;
            let staged = engine.context().add_staged_expression(MIRStagedExpression {
                expression,
                parameters,
                captures,
            });
            frame
                .comptime_registers_mut()
                .insert(*out, MIRComptimeValue::Staged(staged));
            Ok(None)
        }
        MIRComptimeOp::Return { value } => Ok(Some(match value {
            Some(value) => engine.read_comptime(frame, value, range)?,
            None => MIRComptimeValue::Constant(MIRConstant::Unit),
        })),
    }
}

pub(crate) fn execute_intrinsic<'c, 'thir, Context: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, Context>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    intrinsic: &MIRIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    match intrinsic {
        MIRIntrinsic::Int(op) => execute_integer_op(engine, frame, body, op, range),

        _ => todo!(),
    }
}
