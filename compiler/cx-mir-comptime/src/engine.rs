use std::{collections::HashMap, marker::PhantomData};

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRComptimeBody, MIRComptimeInstruction, MIRComptimeOperand, MIRComptimeParameter, MIRComptimeRegisterID, MIRComptimeValue, MIRConstant, MIRPlaceID, MIRRegisterID, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{execute_comptime_instruction, execute_runtime_instruction},
    log::comptime_error,
};

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

pub struct Engine<'c, 'thir, C: ComptimeContext<'thir>> {
    context: &'c C,
    limits: EngineLimits,
    steps: u64,
    depth: usize,

    _phantom: PhantomData<&'thir ()>,
}

impl<'c, 'thir, C: ComptimeContext<'thir>> Engine<'c, 'thir, C> {
    pub fn new(context: &'c C) -> Self {
        Self {
            context,
            limits: EngineLimits::default(),
            steps: 0,
            depth: 0,

            _phantom: PhantomData,
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
        let result = self.run_frame(body, ExecutionFrame::new(body, args));
        self.depth -= 1;

        result
    }

    pub(crate) fn run_frame(
        &mut self,
        body: &MIRComptimeBody<'thir>,
        mut frame: ExecutionFrame,
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
                    if let Some(value) =
                        execute_runtime_instruction(self, &mut frame, body, instruction)?
                    {
                        return Ok(value);
                    }
                }

                MIRComptimeInstruction::Comptime { op, token_range } => {
                    if let Some(value) =
                        execute_comptime_instruction(self, &mut frame, op, token_range)?
                    {
                        return Ok(value);
                    }
                }
            }
        }
    }

    pub(crate) fn read_comptime(
        &self,
        frame: &ExecutionFrame,
        operand: &MIRComptimeOperand,
        range: &TokenRange,
    ) -> CXResult<MIRComptimeValue> {
        match operand {
            MIRComptimeOperand::Known(value) => Ok(value.clone()),
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

    pub(crate) fn read(
        &self,
        frame: &ExecutionFrame,
        value: &MIRValue,
        range: &TokenRange,
    ) -> CXResult<MIRConstant> {
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
        }
    }

    pub(crate) fn read_bindable(
        &self,
        frame: &ExecutionFrame,
        value: &MIRBindable,
        range: &TokenRange,
    ) -> CXResult<MIRConstant> {
        match value {
            MIRBindable::Place(id) => self.read(frame, &MIRValue::PlaceRef(*id), range),
            MIRBindable::Register(id) => self.read(frame, &MIRValue::Register(*id), range),
        }
    }

    pub(crate) fn write(
        &self,
        frame: &mut ExecutionFrame,
        target: &MIRTarget,
        value: MIRConstant,
    ) -> CXResult<()> {
        match target {
            MIRTarget::Place(id) => {
                frame.places_mut().insert(*id, value);
                Ok(())
            }
            
            MIRTarget::Register(id) => {
                frame.registers_mut().insert(*id, value);
                Ok(())
            }

            _ => comptime_error(
                TokenRange::internal(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "invalid comptime write target".into(),
                ),
            ),
        }
    }

    pub(crate) fn jump(
        &self,
        body: &MIRComptimeBody<'_>,
        frame: &mut ExecutionFrame,
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

    pub(crate) fn context(&self) -> &'c C {
        self.context
    }
}

pub(crate) struct ExecutionFrame {
    block: MIRBasicBlockID,
    instruction: usize,
    places: HashMap<MIRPlaceID, MIRConstant>,
    registers: HashMap<MIRRegisterID, MIRConstant>,
    comptime_registers: HashMap<MIRComptimeRegisterID, MIRComptimeValue>,
}

impl ExecutionFrame {
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

    pub(crate) fn places(&self) -> &HashMap<MIRPlaceID, MIRConstant> {
        &self.places
    }

    pub(crate) fn places_mut(&mut self) -> &mut HashMap<MIRPlaceID, MIRConstant> {
        &mut self.places
    }

    pub(crate) fn registers(&self) -> &HashMap<MIRRegisterID, MIRConstant> {
        &self.registers
    }

    pub(crate) fn registers_mut(&mut self) -> &mut HashMap<MIRRegisterID, MIRConstant> {
        &mut self.registers
    }

    pub(crate) fn comptime_registers(&self) -> &HashMap<MIRComptimeRegisterID, MIRComptimeValue> {
        &self.comptime_registers
    }

    pub(crate) fn comptime_registers_mut(
        &mut self,
    ) -> &mut HashMap<MIRComptimeRegisterID, MIRComptimeValue> {
        &mut self.comptime_registers
    }
}