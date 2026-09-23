use std::collections::HashMap;

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRComptimeBody, MIRComptimeInstruction,
    MIRComptimeOp, MIRConstant, MIRGlobalID, MIRGlobalState, MIRInstruction, MIRInstructionKind,
    MIRPlaceID, MIRRegisterID, MIRTarget, MIRTypeKind, MIRValue,
    expr::{
        instruction::MIRInvalidationKind,
        intrinsic::{MIRAggregateIntrinsic, MIRInternalIntrinsic, MIRIntrinsic},
    },
    ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;

use crate::{ComptimeContext, arithmetic, log::comptime_error};

const DEFAULT_STEP_BUDGET: u64 = 1_000_000;
const DEFAULT_MAX_DEPTH: usize = 128;

#[derive(Debug, Clone, Copy)]
pub struct EngineLimits {
    pub max_steps: u64,
    pub max_call_depth: usize,
}

impl Default for EngineLimits {
    fn default() -> Self {
        Self {
            max_steps: DEFAULT_STEP_BUDGET,
            max_call_depth: DEFAULT_MAX_DEPTH,
        }
    }
}

struct Frame {
    block: MIRBasicBlockID,
    instruction: usize,
    places: HashMap<MIRPlaceID, MIRConstant>,
    registers: HashMap<MIRRegisterID, MIRConstant>,
}

impl Frame {
    fn new(body: &MIRComptimeBody<'_>, args: &[MIRConstant]) -> Self {
        let places = body
            .parameters()
            .iter()
            .copied()
            .zip(args.iter().cloned())
            .collect();
        Self {
            block: body.entry(),
            instruction: 0,
            places,
            registers: HashMap::new(),
        }
    }
}

pub struct Engine<'a, C: ComptimeContext> {
    context: &'a C,
    limits: EngineLimits,
    steps: u64,
    depth: usize,
}

impl<'a, C: ComptimeContext> Engine<'a, C> {
    pub fn new(context: &'a C) -> Self {
        Self {
            context,
            limits: EngineLimits::default(),
            steps: 0,
            depth: 0,
        }
    }

    pub fn run(
        &mut self,
        body: &MIRComptimeBody<'_>,
        args: &[MIRConstant],
    ) -> CXResult<MIRConstant> {
        if self.depth >= self.limits.max_call_depth {
            return comptime_error(
                TokenRange::internal(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "call depth exceeded".into(),
                ),
            );
        }
        if body.parameters().len() != args.len() {
            return comptime_error(
                TokenRange::internal(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "argument count mismatch".into(),
                ),
            );
        }
        self.depth += 1;
        let result = self.run_frame(body, Frame::new(body, args));
        self.depth -= 1;
        result
    }

    fn run_frame(&mut self, body: &MIRComptimeBody<'_>, mut frame: Frame) -> CXResult<MIRConstant> {
        loop {
            self.steps += 1;
            if self.steps > self.limits.max_steps {
                return comptime_error(
                    TokenRange::internal(),
                    (&mir::COMPTIME_STEP_LIMIT, self.limits.max_steps),
                );
            }
            let instruction = body
                .block(frame.block)
                .and_then(|block| block.instruction(frame.instruction))
                .ok_or_else(|| {
                    crate::log::internal_error(
                        &mir::COMPTIME_INVALID_OPERATION,
                        "unterminated comptime block".into(),
                        "comptime execution",
                    )
                })?;
            frame.instruction += 1;

            match instruction {
                MIRComptimeInstruction::Runtime(instruction) => {
                    if let Some(value) = self.execute_runtime(body, &mut frame, instruction)? {
                        return Ok(value);
                    }
                }
                MIRComptimeInstruction::Comptime { op, token_range } => {
                    self.execute_comptime(&mut frame, op, token_range)?;
                }
            }
        }
    }

    fn execute_comptime(
        &mut self,
        frame: &mut Frame,
        op: &MIRComptimeOp<'_>,
        range: &TokenRange,
    ) -> CXResult<()> {
        match op {
            MIRComptimeOp::Call { out, callee, args } => {
                let args = args
                    .iter()
                    .map(|value| self.read(frame, value, range))
                    .collect::<CXResult<Vec<_>>>()?;
                let function = self.context.function(*callee).ok_or_else(|| {
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
                let result = self.run(body, &args)?;
                if let Some(out) = out {
                    frame.registers.insert(*out, result);
                }
                Ok(())
            }
            MIRComptimeOp::Emit { .. } => comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "staged expressions".into(),
                ),
            ),
        }
    }

    fn execute_runtime(
        &mut self,
        body: &MIRComptimeBody<'_>,
        frame: &mut Frame,
        instruction: &MIRInstruction,
    ) -> CXResult<Option<MIRConstant>> {
        let range = &instruction.token_range;
        match &instruction.kind {
            MIRInstructionKind::Initialize { .. } | MIRInstructionKind::BindLifetime { .. } => {}
            MIRInstructionKind::Invalidate { place, kind } => {
                if *kind != MIRInvalidationKind::Drop {
                    self.read_bindable(frame, place, range)?;
                }
                match place {
                    MIRBindable::Place(id) => {
                        frame.places.remove(id);
                    }
                    MIRBindable::Register(id) => {
                        frame.registers.remove(id);
                    }
                }
            }
            MIRInstructionKind::LiftPlace { out, place } => {
                let value = self.read(frame, &MIRValue::PlaceRef(*place), range)?;
                frame.registers.insert(*out, value);
            }
            MIRInstructionKind::Forward { out, source } => {
                let value = self.read(frame, &MIRValue::Register(*source), range)?;
                frame.registers.insert(*out, value);
            }
            MIRInstructionKind::Store { target, value, .. } => {
                let value = self.read(frame, value, range)?;
                frame.places.insert(*target, value);
            }
            MIRInstructionKind::Call { .. } => {
                return comptime_error(
                    range.clone(),
                    (&mir::COMPTIME_INVALID_OPERATION, "runtime call".into()),
                );
            }
            MIRInstructionKind::IntrinsicOp(intrinsic) => {
                self.execute_intrinsic(body, frame, intrinsic, range)?;
            }
            MIRInstructionKind::Return { value } => {
                return Ok(Some(match value {
                    Some(value) => self.read(frame, value, range)?,
                    None => MIRConstant::Unit,
                }));
            }
            MIRInstructionKind::Jump { target } => self.jump(body, frame, target, range)?,
            MIRInstructionKind::Branch {
                cond,
                true_target,
                false_target,
            } => {
                let cond = self.read(frame, cond, range)?;
                let target = if arithmetic::truthy(&cond) {
                    true_target
                } else {
                    false_target
                };
                self.jump(body, frame, target, range)?;
            }
            MIRInstructionKind::CaseBranch {
                value,
                cases,
                default,
            } => {
                let value = self.read(frame, value, range)?;
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
                self.jump(body, frame, target, range)?;
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

    fn execute_intrinsic(
        &self,
        body: &MIRComptimeBody<'_>,
        frame: &mut Frame,
        intrinsic: &MIRIntrinsic,
        range: &TokenRange,
    ) -> CXResult<()> {
        if let Some((target, value)) = arithmetic::evaluate(
            intrinsic,
            body,
            self.context.types(),
            |value| self.read(frame, value, range),
            range,
        )? {
            return self.write(frame, target, value, range);
        }
        match intrinsic {
            MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::StructInit { out, ty, fields }) => {
                let fields = fields
                    .iter()
                    .map(|(index, value)| {
                        self.read(frame, value, range).map(|value| (*index, value))
                    })
                    .collect::<CXResult<Vec<_>>>()?;
                self.write(
                    frame,
                    *out,
                    MIRConstant::Aggregate { ty: *ty, fields },
                    range,
                )
            }
            MIRIntrinsic::Internal(MIRInternalIntrinsic::Assert { condition, .. }) => {
                if arithmetic::truthy(&self.read(frame, condition, range)?) {
                    Ok(())
                } else {
                    comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "failed compile-time assertion".into(),
                        ),
                    )
                }
            }
            MIRIntrinsic::Internal(MIRInternalIntrinsic::Assume { condition }) => {
                self.read(frame, condition, range)?;
                Ok(())
            }
            _ => comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "unsupported intrinsic".into(),
                ),
            ),
        }
    }

    fn read(&self, frame: &Frame, value: &MIRValue, range: &TokenRange) -> CXResult<MIRConstant> {
        match value {
            MIRValue::Constant(value) => Ok(value.clone()),
            MIRValue::Register(id) => match frame.registers.get(id) {
                Some(value) => Ok(value.clone()),
                None => comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "read of uninitialized comptime register".into(),
                    ),
                ),
            },
            MIRValue::PlaceRef(id) => match frame.places.get(id) {
                Some(value) => Ok(value.clone()),
                None => comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "read of uninitialized comptime place".into(),
                    ),
                ),
            },
            MIRValue::Global(id) => self.global(*id, range),
        }
    }

    fn read_bindable(
        &self,
        frame: &Frame,
        value: &MIRBindable,
        range: &TokenRange,
    ) -> CXResult<MIRConstant> {
        match value {
            MIRBindable::Place(id) => self.read(frame, &MIRValue::PlaceRef(*id), range),
            MIRBindable::Register(id) => self.read(frame, &MIRValue::Register(*id), range),
        }
    }

    fn global(&self, id: MIRGlobalID, range: &TokenRange) -> CXResult<MIRConstant> {
        let Some(global) = self.context.global(id) else {
            return comptime_error(
                range.clone(),
                (&mir::COMPTIME_INVALID_OPERATION, "unknown global".into()),
            );
        };
        match global.state() {
            MIRGlobalState::Initialized(value) => Ok(value.clone()),
            MIRGlobalState::ZeroInitialized => {
                let Some(ty) = self.context.types().definition(global.ty()) else {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "unknown global type".into(),
                        ),
                    );
                };
                match ty.kind() {
                    MIRTypeKind::Integer { ty, .. } => {
                        Ok(MIRConstant::Integer { value: 0, ty: *ty })
                    }
                    MIRTypeKind::Float { ty } => Ok(MIRConstant::Float {
                        value: 0.0.into(),
                        ty: *ty,
                    }),
                    _ => comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "unsupported zero-initialized global".into(),
                        ),
                    ),
                }
            }
            MIRGlobalState::External => comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "external global read".into(),
                ),
            ),
        }
    }

    fn write(
        &self,
        frame: &mut Frame,
        target: MIRTarget,
        value: MIRConstant,
        range: &TokenRange,
    ) -> CXResult<()> {
        match target {
            MIRTarget::Place(id) => {
                frame.places.insert(id, value);
                Ok(())
            }
            MIRTarget::Register(id) => {
                frame.registers.insert(id, value);
                Ok(())
            }
            MIRTarget::Global(_) | MIRTarget::Indirect(_) => comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "indirect compile-time write".into(),
                ),
            ),
        }
    }

    fn jump(
        &self,
        body: &MIRComptimeBody<'_>,
        frame: &mut Frame,
        target: &MIRBlockTarget,
        range: &TokenRange,
    ) -> CXResult<()> {
        let Some(block) = body.block(target.block) else {
            return comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "invalid comptime jump".into(),
                ),
            );
        };
        if block.params().len() != target.args.len() {
            return comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "block argument count mismatch".into(),
                ),
            );
        }
        let values = target
            .args
            .iter()
            .map(|value| self.read(frame, value, range))
            .collect::<CXResult<Vec<_>>>()?;
        for (param, value) in block.params().iter().zip(values) {
            frame.registers.insert(*param, value);
        }
        frame.block = target.block;
        frame.instruction = 0;
        Ok(())
    }
}
