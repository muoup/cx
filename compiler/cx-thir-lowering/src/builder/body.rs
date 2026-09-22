use cx_mir::{MIRBasicBlockID, MIRBody, MIRComptimeBody, MIRComptimeInstruction, MIRInstruction};

#[derive(Debug)]
pub(crate) struct MIRBodyBuilder<'thir> {
    kind: MIRBodyKind<'thir>,
    current_block: MIRBasicBlockID,
}

#[derive(Debug)]
pub(crate) enum MIRBodyKind<'thir> {
    Runtime(MIRBody),
    Comptime(MIRComptimeBody<'thir>),
}

impl<'thir> MIRBodyBuilder<'thir> {
    pub fn new_runtime(body: MIRBody) -> Self {
        let current_block = body.entry();
        Self {
            kind: MIRBodyKind::Runtime(body),
            current_block,
        }
    }

    pub fn new_comptime(body: MIRComptimeBody<'thir>) -> Self {
        let current_block = body.entry();
        Self {
            kind: MIRBodyKind::Comptime(body),
            current_block,
        }
    }

    pub fn emit(&mut self, instruction: MIRInstruction) {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.push_instr_at(self.current_block, instruction),
            MIRBodyKind::Comptime(body) => body.push_instr_at(
                self.current_block,
                MIRComptimeInstruction::Runtime(instruction),
            ),
        }
    }

    pub fn set_current_block(&mut self, block: MIRBasicBlockID) {
        self.current_block = block;
    }

    pub fn finish(self) -> MIRBodyKind<'thir> {
        self.kind
    }
}
