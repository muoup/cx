use cx_mir::{MIRBody, MIRComptimeBody, MIRInstruction};

pub(crate) struct MIRBodyBuilder<'thir> {
    kind: MIRBodyKind<'thir>,
    current_block: usize,
}

#[derive(Debug)]
pub(crate) enum MIRBodyKind<'thir> {
    Runtime(MIRBody),
    Comptime(MIRComptimeBody<'thir>),
}

impl<'thir> MIRBodyBuilder<'thir> {
    pub fn new_runtime(body: MIRBody) -> Self {
        Self {
            kind: MIRBodyKind::Runtime(body),
            current_block: 0,
        }
    }

    pub fn new_comptime(body: MIRComptimeBody) -> Self {
        Self {
            kind: MIRBodyKind::Comptime(body),
            current_block: 0,
        }
    }
    
    pub fn emit(&mut self, instruction: MIRInstruction) {
        match self {
            MIRBodyBuilder::Runtime(body) => (),
            MIRBodyBuilder::Comptime(body) => ()
        }
    }

    pub fn set_current_block(&mut self, block: usize) {
        self.current_block = block;
    }

    pub fn finish(self) -> MIRBodyKind<'thir> {
        self.kind
    }
}