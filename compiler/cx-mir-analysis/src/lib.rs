//! Deterministic analyses over semantic MIR.
//!
//! Liveness v0 deliberately tracks [`MIRPlace`] values only. MIR registers are
//! SSA-like temporaries and are outside the scope of this first analysis.

use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt::{self, Display, Formatter},
};

use cx_mir::{
    MIRBasicBlockID, MIRFunction, MIRFunctionID, MIRInstrKind, MIROperand, MIRPlace, MIRUnit,
    MIRValidationError,
};

/// Controls optional checks performed before the independent dataflow analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRAnalysisOptions {
    /// Validate all MIR structural invariants before computing liveness.
    pub validate: bool,
}

impl Default for MIRAnalysisOptions {
    fn default() -> Self {
        Self { validate: true }
    }
}

/// Place liveness for every function in a MIR unit.
///
/// Ordered maps and sets make both programmatic iteration and textual dumps
/// deterministic. Register liveness is intentionally omitted in v0.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRAnalysis {
    pub functions: BTreeMap<MIRFunctionID, MIRFunctionAnalysis>,
}

impl MIRAnalysis {
    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunctionAnalysis> {
        self.functions.get(&id)
    }
}

/// Place liveness for each basic block in one function.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRFunctionAnalysis {
    pub blocks: BTreeMap<MIRBasicBlockID, MIRBlockLiveness>,
}

impl MIRFunctionAnalysis {
    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBlockLiveness> {
        self.blocks.get(&id)
    }
}

/// Fixed-point block boundaries and the corresponding instruction boundaries.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRBlockLiveness {
    pub live_in: BTreeSet<MIRPlace>,
    pub live_out: BTreeSet<MIRPlace>,
    pub instructions: Vec<MIRInstructionLiveness>,
}

/// Place liveness immediately before and after one MIR instruction.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRInstructionLiveness {
    pub live_before: BTreeSet<MIRPlace>,
    pub live_after: BTreeSet<MIRPlace>,
}

/// Failures that can prevent MIR analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRAnalysisError {
    Validation(MIRValidationError),
}

impl Display for MIRAnalysisError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Validation(error) => write!(f, "MIR validation failed: {error}"),
        }
    }
}

impl Error for MIRAnalysisError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Validation(error) => Some(error),
        }
    }
}

impl From<MIRValidationError> for MIRAnalysisError {
    fn from(value: MIRValidationError) -> Self {
        Self::Validation(value)
    }
}

/// Optionally validates `unit`, then always computes backward place liveness.
///
/// With validation disabled, malformed successor IDs are ignored rather than
/// indexed. This keeps analysis best-effort and non-panicking without mixing
/// structural validation policy into the dataflow transfer function.
pub fn analyze(
    unit: &MIRUnit,
    options: MIRAnalysisOptions,
) -> Result<MIRAnalysis, MIRAnalysisError> {
    if options.validate {
        unit.validate()?;
    }

    let functions = unit
        .functions
        .iter()
        .map(|function| (function.id, analyze_function(function)))
        .collect();

    Ok(MIRAnalysis { functions })
}

fn analyze_function(function: &MIRFunction) -> MIRFunctionAnalysis {
    let block_count = function.blocks.len();
    let mut block_uses = Vec::with_capacity(block_count);
    let mut block_defs = Vec::with_capacity(block_count);
    let mut successors = Vec::with_capacity(block_count);
    let mut phi_edge_uses = vec![BTreeMap::<usize, BTreeSet<MIRPlace>>::new(); block_count];

    for block in &function.blocks {
        let mut uses = BTreeSet::new();
        let mut defs = BTreeSet::new();

        for instruction in &block.instrs {
            if !matches!(&instruction.kind, MIRInstrKind::Phi { .. }) {
                instruction.for_each_referenced_place(|place| {
                    if !defs.contains(&place) {
                        uses.insert(place);
                    }
                });
            }
            instruction.for_each_defined_place(|place| {
                defs.insert(place);
            });
        }

        for instruction in &block.instrs {
            if let MIRInstrKind::Phi { incoming, .. } = &instruction.kind {
                for (predecessor, operand) in incoming {
                    if predecessor.index() < block_count {
                        if let MIROperand::Place(place) = operand {
                            phi_edge_uses[block.id.index()]
                                .entry(predecessor.index())
                                .or_default()
                                .insert(*place);
                        }
                    }
                }
            }
        }

        let block_successors = block
            .instrs
            .last()
            .into_iter()
            .flat_map(|instruction| instruction.successors())
            .map(MIRBasicBlockID::index)
            .filter(|successor| *successor < block_count)
            .collect::<BTreeSet<_>>();

        block_uses.push(uses);
        block_defs.push(defs);
        successors.push(block_successors);
    }

    let mut live_in = vec![BTreeSet::new(); block_count];
    let mut live_out = vec![BTreeSet::new(); block_count];

    loop {
        let mut changed = false;

        for block_index in (0..block_count).rev() {
            let mut new_live_out = BTreeSet::new();
            for successor in &successors[block_index] {
                new_live_out.extend(live_in[*successor].iter().copied());
                if let Some(phi_uses) = phi_edge_uses[*successor].get(&block_index) {
                    new_live_out.extend(phi_uses.iter().copied());
                }
            }

            let mut new_live_in = block_uses[block_index].clone();
            new_live_in.extend(new_live_out.difference(&block_defs[block_index]).copied());

            if new_live_out != live_out[block_index] {
                live_out[block_index] = new_live_out;
                changed = true;
            }
            if new_live_in != live_in[block_index] {
                live_in[block_index] = new_live_in;
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    let blocks = function
        .blocks
        .iter()
        .enumerate()
        .map(|(block_index, block)| {
            let mut live = live_out[block_index].clone();
            let mut instruction_liveness = Vec::with_capacity(block.instrs.len());

            for instruction in block.instrs.iter().rev() {
                let live_after = live.clone();
                instruction.for_each_defined_place(|place| {
                    live.remove(&place);
                });
                if !matches!(&instruction.kind, MIRInstrKind::Phi { .. }) {
                    instruction.for_each_referenced_place(|place| {
                        live.insert(place);
                    });
                }
                instruction_liveness.push(MIRInstructionLiveness {
                    live_before: live.clone(),
                    live_after,
                });
            }
            instruction_liveness.reverse();

            (
                block.id,
                MIRBlockLiveness {
                    live_in: live_in[block_index].clone(),
                    live_out: live_out[block_index].clone(),
                    instructions: instruction_liveness,
                },
            )
        })
        .collect();

    MIRFunctionAnalysis { blocks }
}

impl Display for MIRAnalysis {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "mir place liveness {{")?;
        for (function_id, function) in &self.functions {
            writeln!(f, "  function {function_id} {{")?;
            for (block_id, block) in &function.blocks {
                write!(f, "    {block_id}: live-in = ")?;
                write_place_set(f, &block.live_in)?;
                write!(f, ", live-out = ")?;
                write_place_set(f, &block.live_out)?;
                writeln!(f)?;
                for (instruction_index, instruction) in block.instructions.iter().enumerate() {
                    write!(f, "      {instruction_index}: before = ")?;
                    write_place_set(f, &instruction.live_before)?;
                    write!(f, ", after = ")?;
                    write_place_set(f, &instruction.live_after)?;
                    writeln!(f)?;
                }
            }
            writeln!(f, "  }}")?;
        }
        writeln!(f, "}}")
    }
}

fn write_place_set(f: &mut Formatter<'_>, places: &BTreeSet<MIRPlace>) -> fmt::Result {
    f.write_str("{")?;
    for (index, place) in places.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        Display::fmt(place, f)?;
    }
    f.write_str("}")
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use cx_ast::ast::modifiers::CXLinkageMode;
    use cx_mir::{
        MIRBasicBlockID, MIRConstant, MIRFnPrototype, MIRFnSignature, MIRInstrKind, MIROperand,
        MIRPlace, MIRType, MIRUnit,
    };
    use cx_util::identifier::CXIdent;

    use super::{MIRAnalysisError, MIRAnalysisOptions, analyze};

    fn set(places: &[MIRPlace]) -> BTreeSet<MIRPlace> {
        places.iter().copied().collect()
    }

    fn unit_with_function(name: &str) -> (MIRUnit, cx_mir::MIRFunctionID) {
        let prototype = MIRFnPrototype::new(
            MIRFnSignature::new(CXIdent::from(name), Vec::new(), None),
            CXLinkageMode::Standard,
        );
        let mut unit = MIRUnit::new();
        let function = unit.add_function(prototype);
        (unit, function)
    }

    #[test]
    fn branch_loop_liveness_reaches_fixed_point() {
        let (mut unit, function_id) = unit_with_function("branch_loop");
        let function = unit.function_mut(function_id).unwrap();
        let condition = function.add_place(MIRType::default(), None);
        let carried = function.add_place(MIRType::default(), None);
        let result = function.add_place(MIRType::default(), None);
        let entry = function.add_block();
        let loop_body = function.add_block();
        let exit = function.add_block();

        function.push_instr(
            entry,
            MIRInstrKind::Branch {
                cond: MIROperand::Place(condition),
                true_target: loop_body,
                false_target: exit,
            },
        );
        function.push_instr(
            loop_body,
            MIRInstrKind::CopyInto {
                dest: result,
                src: MIROperand::Place(carried),
                ty: MIRType::default(),
            },
        );
        function.push_instr(loop_body, MIRInstrKind::Jump { target: entry });
        function.push_instr(
            exit,
            MIRInstrKind::Return {
                value: Some(MIROperand::Place(result)),
            },
        );

        let analysis = analyze(&unit, MIRAnalysisOptions::default()).unwrap();
        let function = analysis.function(function_id).unwrap();
        let entry_liveness = function.block(entry).unwrap();
        let loop_liveness = function.block(loop_body).unwrap();
        let exit_liveness = function.block(exit).unwrap();

        assert_eq!(entry_liveness.live_in, set(&[condition, carried, result]));
        assert_eq!(entry_liveness.live_out, set(&[condition, carried, result]));
        assert_eq!(loop_liveness.live_in, set(&[condition, carried]));
        assert_eq!(loop_liveness.live_out, set(&[condition, carried, result]));
        assert_eq!(exit_liveness.live_in, set(&[result]));
        assert_eq!(exit_liveness.live_out, BTreeSet::new());

        assert_eq!(
            loop_liveness.instructions[0].live_before,
            set(&[condition, carried])
        );
        assert_eq!(
            loop_liveness.instructions[0].live_after,
            set(&[condition, carried, result])
        );
        assert_eq!(
            loop_liveness.instructions[1].live_before,
            set(&[condition, carried, result])
        );
        assert_eq!(
            loop_liveness.instructions[1].live_after,
            set(&[condition, carried, result])
        );

        let dump = analysis.to_string();
        assert!(dump.contains("bb0: live-in = {%p0, %p1, %p2}"));
        assert!(dump.contains("0: before = {%p0, %p1, %p2}"));
    }

    #[test]
    fn phi_operands_are_live_only_on_their_predecessor_edges() {
        let (mut unit, function_id) = unit_with_function("phi_edges");
        let function = unit.function_mut(function_id).unwrap();
        let left_value = function.add_place(MIRType::default(), None);
        let right_value = function.add_place(MIRType::default(), None);
        let result = function.add_register(MIRType::default(), None);
        let entry = function.add_block();
        let left = function.add_block();
        let right = function.add_block();
        let merge = function.add_block();

        function.push_instr(
            entry,
            MIRInstrKind::Branch {
                cond: MIROperand::Constant(MIRConstant::Bool(true)),
                true_target: left,
                false_target: right,
            },
        );
        function.push_instr(left, MIRInstrKind::Jump { target: merge });
        function.push_instr(right, MIRInstrKind::Jump { target: merge });
        function.push_instr(
            merge,
            MIRInstrKind::Phi {
                out: result,
                incoming: vec![
                    (left, MIROperand::Place(left_value)),
                    (right, MIROperand::Place(right_value)),
                ],
            },
        );
        function.push_instr(
            merge,
            MIRInstrKind::Return {
                value: Some(MIROperand::Register(result)),
            },
        );

        let analysis = analyze(&unit, MIRAnalysisOptions::default()).unwrap();
        let function = analysis.function(function_id).unwrap();
        assert_eq!(function.block(left).unwrap().live_out, set(&[left_value]));
        assert_eq!(function.block(right).unwrap().live_out, set(&[right_value]));
        assert!(function.block(merge).unwrap().live_in.is_empty());
    }

    #[test]
    fn validation_can_be_disabled_for_invalid_cfg() {
        let (mut unit, function_id) = unit_with_function("invalid_cfg");
        let function = unit.function_mut(function_id).unwrap();
        let entry = function.add_block();
        function.push_instr(
            entry,
            MIRInstrKind::Jump {
                target: MIRBasicBlockID::new(99),
            },
        );

        let analysis = analyze(&unit, MIRAnalysisOptions { validate: false }).unwrap();
        let block = analysis
            .function(function_id)
            .unwrap()
            .block(entry)
            .unwrap();
        assert!(block.live_in.is_empty());
        assert!(block.live_out.is_empty());

        let error = analyze(&unit, MIRAnalysisOptions { validate: true }).unwrap_err();
        assert!(matches!(error, MIRAnalysisError::Validation(_)));
    }
}
