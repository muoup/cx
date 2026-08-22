use std::collections::HashMap;

use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRFunctionMode, MIRInstr, MIRScopeID, MIRType, MIRValue
};
use cx_thir::thir::expression::{THIRExpression, THIRLocalID};
use cx_tokens::TokenRange;

#[derive(Debug)]
pub(crate) struct FunctionBuilder<'thir> {
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    mode: MIRFunctionMode,

    body: MIRBody,
    current_block: MIRBasicBlockID,

    scope_stack: Vec<ScopeContext<'thir>>,
    local_map: HashMap<THIRLocalID, MIRValue>,
}

#[derive(Debug)]
pub(crate) struct ScopeContext<'thir> {
    id: MIRScopeID,

    yield_target: Option<(MIRBasicBlockID, MIRType)>,
    continue_target: Option<MIRBasicBlockID>,
    break_target: Option<MIRBasicBlockID>,

    cleanups: Vec<&'thir THIRExpression>,
}

impl FunctionBuilder<'_> {
    pub(crate) fn new(func: MIRFunction) -> Self {
        Self {
            id: func.id(),
            mode: func.mode(),
            prototype: func.prototype().clone(),

            body: MIRBody::new(),

            current_block: MIRBasicBlockID::new(0),
            scope_stack: Vec::new(),
            local_map: HashMap::new(),
        }
    }

    pub(crate) fn finish(self) -> MIRFunction {
        MIRFunction::new(self.id, self.prototype, Some(self.body))
    }

    pub(crate) fn concise_finish(self) -> (MIRFunctionID, MIRBody) {
        (self.id, self.body)
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn mode(&self) -> MIRFunctionMode {
        self.mode
    }

    pub fn body(&self) -> &MIRBody {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut MIRBody {
        &mut self.body
    }

    // API
    fn current_block(&self) -> &MIRBasicBlock {
        self.body()
            .block(self.current_block)
            .expect("current block must exist")
    }

    fn current_block_mut(&mut self) -> &mut MIRBasicBlock {
        let block = self.current_block;
        
        self.body_mut()
            .block_mut(block)
            .expect("current block must exist")
    }
    
    pub fn emit(&mut self, instr: MIRInstrKind, range: TokenRange) {
        self.current_block_mut()
            .instrs
            .push(MIRInstr::new(instr, range));
    }

    pub fn current_block_terminated(&self) -> bool {
        if let Some(instr) = self.current_block().instrs.last() {
            instr.is_terminator()
        } else {
            false
        }
    }
}
