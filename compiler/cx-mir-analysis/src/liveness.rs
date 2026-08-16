use std::collections::BTreeSet;

use cx_mir::{MIRBasicBlockID, MIRFunction};

use crate::types::{MIRBlockLiveness, MIRFunctionAnalysis, MIRInstructionLiveness};

pub(crate) fn analyze_function(function: &MIRFunction) -> MIRFunctionAnalysis {
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
