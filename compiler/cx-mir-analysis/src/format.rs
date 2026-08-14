use std::{collections::BTreeSet, fmt};

use cx_mir::MIRPlace;

use crate::MIRAnalysis;

impl fmt::Display for MIRAnalysis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

fn write_place_set(f: &mut fmt::Formatter<'_>, places: &BTreeSet<MIRPlace>) -> fmt::Result {
    f.write_str("{")?;
    for (index, place) in places.iter().enumerate() {
        if index != 0 {
            f.write_str(", ")?;
        }
        fmt::Display::fmt(place, f)?;
    }
    f.write_str("}")
}
