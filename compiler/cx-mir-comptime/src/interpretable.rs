use cx_mir::{MIRBasicBlockID, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRInstr, MIRRegister};

pub trait ComptimeInterpretable {
    fn prototype(&self) -> &MIRFnPrototype;

    fn function_id(&self) -> MIRFunctionID;

    fn current_block(&self) -> MIRBasicBlockID;

    fn next_instruction(&mut self) -> Option<&MIRInstr>;

    fn jump_to_block(&mut self, block: MIRBasicBlockID);

    fn block_params(&self, block: MIRBasicBlockID) -> &[MIRRegister];
}

#[derive(Debug, Clone, Copy)]
pub struct InterpretedFunction<'code> {
    function: &'code MIRFunction,
    cursor: (MIRBasicBlockID, usize),
}

impl<'code> InterpretedFunction<'code> {
    pub fn new(function: &'code MIRFunction) -> Option<Self> {
        let definition = function
            .definition()
            .expect("interpreted function has a definition");
        Some(Self {
            function,
            cursor: (definition.entry(), 0),
        })
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

    fn next_instruction(&mut self) -> Option<&MIRInstr> {
        let index = self.cursor.1;
        let block = self
            .function
            .definition()?
            .block(self.cursor.0)?;
        let instr = block.instrs.get(index)?;
        self.cursor.1 = index + 1;
        Some(instr)
    }

    fn jump_to_block(&mut self, block: MIRBasicBlockID) {
        self.cursor = (block, 0);
    }

    fn block_params(&self, block: MIRBasicBlockID) -> &[MIRRegister] {
        self.function
            .definition()
            .and_then(|definition| definition.block(block))
            .map(|block| block.params.as_slice())
            .unwrap_or(&[])
    }
}
