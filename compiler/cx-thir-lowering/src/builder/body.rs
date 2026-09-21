use cx_mir::MIRInstruction;

#[derive(Debug)]
pub(crate) enum MIRBodyBuilder {
    Runtime(MIRBody),
    Comptime(MIRComptimeBody),
}

impl MIRBodyBuilder {
    pub fn emit(&mut self, instruction: MIRInstruction) {
        match self {
            MIRBodyBuilder::Runtime(body) => body.emit(instr, range),
            MIRBodyBuilder::Comptime(body) => body.emit(instr, range),
        }
    }
}