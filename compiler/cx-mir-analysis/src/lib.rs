//! Deterministic analyses over semantic MIR.
//!
//! Liveness v0 deliberately tracks [`MIRPlace`] values only. MIR registers are
//! SSA-like temporaries and are outside the scope of this first analysis.

use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt::{self, Display, Formatter},
};

use cx_mir::{MIRBasicBlockID, MIRFunction, MIRFunctionID, MIRPlace, MIRUnit, MIRValidationError};

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

    for block in &function.blocks {
        let mut uses = BTreeSet::new();
        let mut defs = BTreeSet::new();

        for instruction in &block.instrs {
            instruction.visit_operands(|operand| {
                if let Some(place) = operand.place()
                    && !defs.contains(&place)
                {
                    uses.insert(place);
                }
            });
            for place in instruction.defined_places() {
                defs.insert(place);
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
                for place in instruction.defined_places() {
                    live.remove(&place);
                }
                instruction.visit_operands(|operand| {
                    if let Some(place) = operand.place() {
                        live.insert(place);
                    }
                });
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