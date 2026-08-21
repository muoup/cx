use cx_mir::MIRUnit;

#[derive(Debug, Clone)]
pub struct MIRComptimeEngine<'unit> {
    unit: &'unit MIRUnit,
    pointer: MIRInstructionPointer,
}

#[derive(Debug, Clone)]
pub struct MIRInstructionPointer {
    block_idx: usize,
    instr_idx: usize,
}

impl<'unit> MIRComptimeEngine<'unit> {
    pub fn new(unit: &'unit MIRUnit) -> Self {
        Self {
            unit,
            pointer: MIRInstructionPointer {
                block_idx: 0,
                instr_idx: 0,
            },
        }
    }

    pub fn unit(&self) -> &'unit MIRUnit {
        self.unit
    }
}
