use std::collections::HashMap;

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRComptimeBody, MIRComptimeInstruction,
    MIRComptimeOp, MIRComptimeOperand, MIRComptimeOutput, MIRComptimeParameter,
    MIRComptimeRegisterID, MIRComptimeType, MIRComptimeValue, MIRConstant, MIRGlobalID,
    MIRGlobalState, MIRInstruction, MIRInstructionKind, MIRPlaceID, MIRRegisterID,
    MIRStagedExpression, MIRTarget, MIRTypeKind, MIRValue,
    expr::{
        instruction::MIRInvalidationKind,
        intrinsic::{MIRAggregateIntrinsic, MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic},
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
    comptime_registers: HashMap<MIRComptimeRegisterID, MIRComptimeValue>,
}

impl Frame {
    fn new(body: &MIRComptimeBody<'_>, args: &[MIRComptimeValue]) -> Self {
        let mut frame = Self {
            block: body.entry(),
            instruction: 0,
            places: HashMap::new(),
            registers: HashMap::new(),
            comptime_registers: HashMap::new(),
        };
        for (parameter, value) in body.comptime_parameters().iter().zip(args) {
            match (parameter, value) {
                (MIRComptimeParameter::Runtime(place), MIRComptimeValue::Constant(value)) => {
                    frame.places.insert(*place, value.clone());
                }
                (MIRComptimeParameter::Comptime(register), value) => {
                    frame.comptime_registers.insert(*register, value.clone());
                }
                _ => unreachable!("standard comptime parameter requires a concrete value"),
            }
        }
        frame
    }
}

pub struct Engine<'a, 'thir, C: ComptimeContext<'thir>> {
    context: &'a C,
    thir: std::marker::PhantomData<&'thir ()>,
    limits: EngineLimits,
    steps: u64,
    depth: usize,
}

impl<'a, 'thir, C: ComptimeContext<'thir>> Engine<'a, 'thir, C> {
    pub fn new(context: &'a C) -> Self {
        Self {
            context,
            thir: std::marker::PhantomData,
            limits: EngineLimits::default(),
            steps: 0,
            depth: 0,
        }
    }

    pub fn run(
        &mut self,
        body: &MIRComptimeBody<'thir>,
        args: &[MIRComptimeValue],
    ) -> CXResult<MIRComptimeValue> {
        if self.depth >= self.limits.max_call_depth {
            return comptime_error(
                TokenRange::internal(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "call depth exceeded".into(),
                ),
            );
        }
        if body.comptime_parameters().len() != args.len() {
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

    fn run_frame(
        &mut self,
        body: &MIRComptimeBody<'thir>,
        mut frame: Frame,
    ) -> CXResult<MIRComptimeValue> {
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
                    if let Some(value) = self.execute_comptime(&mut frame, op, token_range)? {
                        return Ok(value);
                    }
                }
            }
        }
    }

    fn execute_comptime(
        &mut self,
        frame: &mut Frame,
        op: &MIRComptimeOp<'thir>,
        range: &TokenRange,
    ) -> CXResult<Option<MIRComptimeValue>> {
        match op {
            MIRComptimeOp::Call { out, callee, args } => {
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
                let args = args
                    .iter()
                    .zip(function.prototype().signature().params())
                    .map(|(operand, parameter)| {
                        let value = self.read_comptime(frame, operand, range)?;
                        match (&parameter.ty, value) {
                            (
                                MIRComptimeType::Standard(_),
                                MIRComptimeValue::GlobalRef(reference),
                            ) => self
                                .read(frame, &MIRValue::GlobalRef(reference), range)
                                .map(MIRComptimeValue::Constant),
                            (_, value) => Ok(value),
                        }
                    })
                    .collect::<CXResult<Vec<_>>>()?;
                let result = self.run(body, &args)?;
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
                        frame.registers.insert(*out, value);
                    }
                    Some(MIRComptimeOutput::Comptime(out)) => {
                        frame.comptime_registers.insert(*out, result);
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
                        self.read_comptime(frame, operand, range)
                            .map(|value| (*id, value))
                    })
                    .collect::<CXResult<_>>()?;
                let staged = self.context.add_staged_expression(MIRStagedExpression {
                    expression,
                    parameters,
                    captures,
                });
                frame
                    .comptime_registers
                    .insert(*out, MIRComptimeValue::Staged(staged));
                Ok(None)
            }
            MIRComptimeOp::Return { value } => Ok(Some(match value {
                Some(value) => self.read_comptime(frame, value, range)?,
                None => MIRComptimeValue::Constant(MIRConstant::Unit),
            })),
        }
    }

    fn read_comptime(
        &self,
        frame: &Frame,
        operand: &MIRComptimeOperand,
        range: &TokenRange,
    ) -> CXResult<MIRComptimeValue> {
        match operand {
            MIRComptimeOperand::Known(value) => Ok(value.clone()),
            MIRComptimeOperand::Runtime(MIRValue::GlobalRef(reference)) => {
                Ok(MIRComptimeValue::GlobalRef(*reference))
            }
            MIRComptimeOperand::Runtime(value) => self
                .read(frame, value, range)
                .map(MIRComptimeValue::Constant),
            MIRComptimeOperand::Comptime(register) => frame
                .comptime_registers
                .get(register)
                .cloned()
                .ok_or_else(|| {
                    crate::log::internal_error(
                        &mir::COMPTIME_INVALID_OPERATION,
                        "read of uninitialized staged register".into(),
                        "comptime execution",
                    )
                }),
        }
    }

    fn execute_runtime(
        &mut self,
        body: &MIRComptimeBody<'_>,
        frame: &mut Frame,
        instruction: &MIRInstruction,
    ) -> CXResult<Option<MIRComptimeValue>> {
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
                    Some(value) => MIRComptimeValue::Constant(self.read(frame, value, range)?),
                    None => MIRComptimeValue::Constant(MIRConstant::Unit),
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
            MIRIntrinsic::Pointer(MIRPtrIntrinsic::Add { out, ptr, offset })
            | MIRIntrinsic::Pointer(MIRPtrIntrinsic::Sub { out, ptr, offset }) => {
                let pointer = self.read(frame, ptr, range)?;
                let offset = self.read(frame, offset, range)?;
                let MIRConstant::Integer { value: offset, .. } = offset else {
                    return comptime_error(
                        range.clone(),
                        (&mir::COMPTIME_INVALID_OPERATION, "non-integer pointer offset".into()),
                    );
                };
                let MIRConstant::GlobalAddress(mut reference) = pointer else {
                    return comptime_error(
                        range.clone(),
                        (&mir::COMPTIME_INVALID_OPERATION, "non-global pointer arithmetic".into()),
                    );
                };
                let offset = i64::try_from(offset).ok();
                let next = offset.and_then(|offset| match intrinsic {
                    MIRIntrinsic::Pointer(MIRPtrIntrinsic::Add { .. }) => {
                        reference.offset.checked_add(offset)
                    }
                    _ => reference.offset.checked_sub(offset),
                });
                let Some(next) = next else {
                    return comptime_error(
                        range.clone(),
                        (&mir::COMPTIME_INVALID_OPERATION, "pointer offset overflow".into()),
                    );
                };
                reference.offset = next;
                self.write(frame, *out, MIRConstant::GlobalAddress(reference), range)
            }
            MIRIntrinsic::Internal(MIRInternalIntrinsic::GlobalAddress { out, global }) => {
                self.write(frame, *out, MIRConstant::GlobalAddress(*global), range)
            }
            MIRIntrinsic::Internal(MIRInternalIntrinsic::GetFnPtr { out, fn_id }) => {
                self.write(frame, *out, MIRConstant::Function(*fn_id), range)
            }
            MIRIntrinsic::Internal(MIRInternalIntrinsic::StringAddress { out, string }) => self
                .write(
                    frame,
                    *out,
                    MIRConstant::StringAddress(string.clone()),
                    range,
                ),
            MIRIntrinsic::Internal(MIRInternalIntrinsic::ArrayAddress { out, array }) => {
                let source = match array {
                    MIRValue::GlobalRef(reference) => MIRConstant::GlobalAddress(*reference),
                    value => self.read(frame, value, range)?,
                };
                self.write(
                    frame,
                    *out,
                    MIRConstant::ArrayAddress(Box::new(source)),
                    range,
                )
            }
            MIRIntrinsic::Internal(MIRInternalIntrinsic::ReferenceAddress { out, reference }) => {
                let address = match reference {
                    MIRValue::GlobalRef(global) => MIRConstant::GlobalAddress(*global),
                    MIRValue::PlaceRef(_) => {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "local place address".into(),
                            ),
                        );
                    }
                    value => self.read(frame, value, range)?,
                };
                self.write(frame, *out, address, range)
            }
            MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::AggregateInit { out, ty, fields }) => {
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
            MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::StructField {
                out,
                base,
                field,
                ..
            })
            | MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::SumVariantL {
                out,
                base,
                variant: field,
                ..
            }) => {
                let aggregate = self.read(frame, base, range)?;
                self.write(
                    frame,
                    *out,
                    Self::aggregate_field(aggregate, *field, range)?,
                    range,
                )
            }
            MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::SumVariant {
                out,
                base,
                variant,
                ..
            }) => {
                let aggregate = self.read(frame, &MIRValue::PlaceRef(*base), range)?;
                self.write(
                    frame,
                    *out,
                    Self::aggregate_field(aggregate, *variant, range)?,
                    range,
                )
            }
            MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::SumIndex { out, value, .. }) => {
                let MIRConstant::Aggregate { fields, .. } = self.read(frame, value, range)? else {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "tag of non-aggregate value".into(),
                        ),
                    );
                };
                let Some((variant, _)) = fields.first() else {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "tag of empty aggregate".into(),
                        ),
                    );
                };
                self.write(
                    frame,
                    *out,
                    MIRConstant::Integer {
                        ty: cx_mir::MIRIntType::I8,
                        value: *variant as i128,
                    },
                    range,
                )
            }
            MIRIntrinsic::Aggregate(MIRAggregateIntrinsic::ArrayIndex {
                out, base, index, ..
            }) => {
                let index = self.read(frame, index, range)?;
                let MIRConstant::Integer { value, .. } = index else {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "non-integer array index".into(),
                        ),
                    );
                };
                let Ok(index) = usize::try_from(value) else {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "invalid array index".into(),
                        ),
                    );
                };
                let aggregate = self.read(frame, base, range)?;
                self.write(
                    frame,
                    *out,
                    Self::aggregate_field(aggregate, index, range)?,
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

    fn aggregate_field(
        aggregate: MIRConstant,
        field: usize,
        range: &TokenRange,
    ) -> CXResult<MIRConstant> {
        let MIRConstant::Aggregate { fields, .. } = aggregate else {
            return comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "field of non-aggregate value".into(),
                ),
            );
        };
        fields
            .into_iter()
            .find_map(|(index, value)| (index == field).then_some(value))
            .ok_or_else(|| {
                crate::log::internal_error(
                    &mir::COMPTIME_INVALID_OPERATION,
                    "missing aggregate field".into(),
                    "comptime execution",
                )
            })
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
            MIRValue::GlobalRef(reference) => {
                if reference.offset != 0 {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "offset global read".into(),
                        ),
                    );
                }
                self.global(reference.global, range)
            }
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
