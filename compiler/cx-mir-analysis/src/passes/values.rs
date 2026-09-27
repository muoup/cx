use std::collections::HashMap;

use cx_log::{CXResult, catalogue::analysis};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRConstant, MIRInstruction, MIRInstructionKind,
    MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic, MIRTarget, MIRValue,
    expr::visit::successors,
};
use cx_tokens::TokenRange;

use crate::{
    framework::{environment::AnalysisEnvironment, pipeline::AnalysisPass},
    log::log_analysis_error,
};

pub struct Values {
    known: HashMap<MIRBindable, i128>,
    reachable: bool,
    blocks: HashMap<MIRBasicBlockID, Option<HashMap<MIRBindable, i128>>>,
    edges: Vec<(MIRBasicBlockID, Option<HashMap<MIRBindable, i128>>)>,
}

impl Values {
    pub fn new() -> Self {
        Self {
            known: HashMap::new(),
            reachable: true,
            blocks: HashMap::new(),
            edges: Vec::new(),
        }
    }

    fn value(&self, value: &MIRValue) -> Option<i128> {
        match value {
            MIRValue::Constant(MIRConstant::Integer { value, .. }) => Some(*value),
            MIRValue::Constant(MIRConstant::Nullptr { .. }) => Some(0),
            MIRValue::Register(register) => {
                self.known.get(&MIRBindable::Register(*register)).copied()
            }
            MIRValue::PlaceRef(place) => self.known.get(&MIRBindable::Place(*place)).copied(),
            _ => None,
        }
    }

    fn set(&mut self, target: MIRTarget, value: Option<i128>) {
        let key = match target {
            MIRTarget::Register(register) => Some(MIRBindable::Register(register)),
            MIRTarget::Place(place) => Some(MIRBindable::Place(place)),
            MIRTarget::Indirect(_) | MIRTarget::Global(_) => None,
        };
        if let Some(key) = key {
            if let Some(value) = value {
                self.known.insert(key, value);
            } else {
                self.known.remove(&key);
            }
        } else {
            self.known
                .retain(|key, _| !matches!(key, MIRBindable::Place(_)));
        }
    }

    fn integer(&self, op: &MIRIntIntrinsic) -> Option<i128> {
        use MIRIntIntrinsic as I;
        if let I::IntCast {
            value,
            sign_extend,
            ..
        } = op
        {
            return self
                .value(value)
                .filter(|value| *value == 0 || (*value == 1 && !sign_extend));
        }
        let (lhs, rhs, evaluate): (&MIRValue, &MIRValue, fn(i128, i128) -> i128) = match op {
            I::Eq { lhs, rhs, .. } => (lhs, rhs, |a, b| i128::from(a == b)),
            I::Neq { lhs, rhs, .. } => (lhs, rhs, |a, b| i128::from(a != b)),
            I::SLt { lhs, rhs, .. } => (lhs, rhs, |a, b| i128::from(a < b)),
            I::SLe { lhs, rhs, .. } => (lhs, rhs, |a, b| i128::from(a <= b)),
            I::SGt { lhs, rhs, .. } => (lhs, rhs, |a, b| i128::from(a > b)),
            I::SGe { lhs, rhs, .. } => (lhs, rhs, |a, b| i128::from(a >= b)),
            _ => return None,
        };
        Some(evaluate(self.value(lhs)?, self.value(rhs)?))
    }

    fn edge(
        &self,
        env: &AnalysisEnvironment,
        target: &MIRBlockTarget,
    ) -> HashMap<MIRBindable, i128> {
        let mut state = self.known.clone();
        let body = env.body();
        let block = body.block(target.block).expect("unknown target block");
        for (parameter, argument) in block.params().iter().zip(&target.args) {
            let value = self.value(argument);
            let key = MIRBindable::Register(*parameter);
            if let Some(value) = value {
                state.insert(key, value);
            } else {
                state.remove(&key);
            }
        }
        state
    }
}

impl AnalysisPass for Values {
    fn applies_to(&self, env: &AnalysisEnvironment) -> bool {
        env.function().prototype().signature.safe()
    }

    fn function_entry(&mut self, _: &AnalysisEnvironment) -> CXResult<()> {
        self.known.clear();
        self.reachable = true;
        self.blocks.clear();
        self.edges.clear();
        Ok(())
    }

    fn finish(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        let body = env.body();
        for block in body.blocks() {
            let state = if block.id() == body.entry() {
                Some(HashMap::new())
            } else {
                self.blocks.get(&block.id()).cloned().flatten()
            };
            let Some(state) = state else {
                continue;
            };
            self.known = state;
            self.reachable = true;
            for instruction in block.instructions() {
                if let MIRInstructionKind::IntrinsicOp(MIRIntrinsic::Internal(
                    MIRInternalIntrinsic::Assert { condition, message },
                )) = &instruction.kind
                    && self.value(condition) == Some(0)
                {
                    return log_analysis_error(
                        &instruction.token_range,
                        (
                            &analysis::PROVEN_FALSE_ASSERTION,
                            (
                                env.function().prototype().display_name().to_string(),
                                message.clone(),
                            ),
                        ),
                    );
                }
                self.analyze_instruction(env, instruction)?;
            }
        }
        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        if !self.reachable {
            self.edges = successors(instruction)
                .into_iter()
                .map(|target| (target.block, None))
                .collect();
            return Ok(());
        }
        match &instruction.kind {
            MIRInstructionKind::Invalidate { place, .. } => {
                self.known.remove(place);
            }
            MIRInstructionKind::Lift { out, source } => {
                let value = match source {
                    MIRTarget::Place(place) => self.known.get(&MIRBindable::Place(*place)).copied(),
                    _ => None,
                };
                self.set(MIRTarget::Register(*out), value);
            }
            MIRInstructionKind::Store { target, value, .. } => {
                let value = self.value(value);
                self.set(*target, value);
            }
            MIRInstructionKind::Call { out, .. } => {
                self.known
                    .retain(|key, _| !matches!(key, MIRBindable::Place(_)));
                if let Some(out) = out {
                    self.known.remove(&MIRBindable::Register(*out));
                }
            }
            MIRInstructionKind::IntrinsicOp(op) => {
                if let Some(target) = op.output_target() {
                    let value = match op {
                        MIRIntrinsic::Int(op) => self.integer(op),
                        _ => None,
                    };
                    self.set(target, value);
                }
            }
            _ => {}
        }

        self.edges = successors(instruction)
            .into_iter()
            .map(|target| (target.block, Some(self.edge(env, target))))
            .collect();
        if let MIRInstructionKind::Branch { cond, .. } = &instruction.kind
            && let Some(value) = self.value(cond)
        {
            self.edges[usize::from(value != 0)].1 = None;
        }
        Ok(())
    }

    fn merge(
        &mut self,
        _: &AnalysisEnvironment,
        other: MIRBasicBlockID,
        _: &TokenRange,
    ) -> CXResult<bool> {
        let mut changed = false;
        for (_, incoming) in self.edges.iter().filter(|(block, _)| *block == other) {
            match (self.blocks.get_mut(&other), incoming) {
                (Some(Some(current)), Some(incoming)) => {
                    let before = current.len();
                    current.retain(|key, value| incoming.get(key) == Some(value));
                    changed |= current.len() != before;
                }
                (Some(current @ None), Some(incoming)) => {
                    *current = Some(incoming.clone());
                    changed = true;
                }
                (None, incoming) => {
                    self.blocks.insert(other, incoming.clone());
                    changed |= incoming.is_some();
                }
                _ => {}
            }
        }
        Ok(changed)
    }

    fn reload_block(&mut self, _: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        let state = self.blocks.get(&block).cloned().flatten();
        self.reachable = state.is_some();
        self.known = state.unwrap_or_default();
        Ok(())
    }
}
