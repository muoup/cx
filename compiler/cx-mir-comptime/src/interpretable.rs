use cx_mir::{
    MIRBasicBlockID, MIRComptimeInstr, MIRComptimeInstrKind, MIRFnPrototype, MIRFunction,
    MIRFunctionBody, MIRFunctionID, MIRRegister,
};

pub trait ComptimeInterpretable {
    fn prototype(&self) -> &MIRFnPrototype;

    fn function_id(&self) -> MIRFunctionID;

    fn current_block(&self) -> MIRBasicBlockID;

    fn next_instruction(&mut self) -> Option<MIRComptimeInstr>;

    fn jump_to_block(&mut self, block: MIRBasicBlockID);

    fn block_params(&self, block: MIRBasicBlockID) -> &[MIRRegister];
}

#[derive(Debug, Clone, Copy)]
pub struct InterpretedFunction<'code> {
    function: &'code MIRFunction,
    cursor: (MIRBasicBlockID, usize),
}

impl<'code> InterpretedFunction<'code> {
    pub fn new(function: &'code MIRFunction) -> Self {
        let body = function.body().expect("interpreted function has a definition");
        
        Self {
            function,
            cursor: (match body {
                MIRFunctionBody::Runtime(body) => body.entry(),
                MIRFunctionBody::Comptime(body) => body.entry(),
            }, 0),
        }
    }
}

impl ComptimeInterpretable for InterpretedFunction<'_> {
    fn prototype(&self) -> &MIRFnPrototype {
        self.function.prototype()
    }

    fn function_id(&self) -> MIRFunctionID {
        self.function.id()
    }

    fn current_block(&self) -> MIRBasicBlockID {
        self.cursor.0
    }

    fn next_instruction(&mut self) -> Option<MIRComptimeInstr> {
        let index = self.cursor.1;
        let instr = match self.function.body()? {
            MIRFunctionBody::Runtime(body) => body.block(self.cursor.0)?.instrs.get(index).map(|instr| {
                cx_mir::MIRInstruction::new(
                    MIRComptimeInstrKind::Standard(instr.kind.clone()),
                    instr.token_range.clone(),
                )
            }),
            MIRFunctionBody::Comptime(body) => body.block(self.cursor.0)?.instrs.get(index).cloned(),
        }?;
        self.cursor.1 = index + 1;
        Some(instr)
    }

    fn jump_to_block(&mut self, block: MIRBasicBlockID) {
        self.cursor = (block, 0);
    }

    fn block_params(&self, block: MIRBasicBlockID) -> &[MIRRegister] {
        self.function
            .body()
            .and_then(|body| match body {
                MIRFunctionBody::Runtime(body) => body.block(block).map(|block| block.params.as_slice()),
                MIRFunctionBody::Comptime(body) => body.block(block).map(|block| block.params.as_slice()),
            })
            .unwrap_or(&[])
    }
}
