use cx_mir::{
    MIRBasicBlockID, MIRBody, MIRComptimeBody, MIRComptimeInstruction, MIRComptimeOp,
    MIRFnParam, MIRInstruction, MIRInstructionLike, MIRPlaceID, MIRRegisterDecl, MIRRegisterID,
    MIRScopeDecl, MIRScopeID, MIRTypeID,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

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
        assert!(!self.current_block_terminated(), "instruction follows a terminator");
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.push_instr_at(self.current_block, instruction),
            MIRBodyKind::Comptime(body) => body.push_instr_at(
                self.current_block,
                MIRComptimeInstruction::Runtime(instruction),
            ),
        }
    }

    pub fn emit_comptime(&mut self, op: MIRComptimeOp<'thir>, token_range: TokenRange) {
        assert!(!self.current_block_terminated(), "instruction follows a terminator");
        match &mut self.kind {
            MIRBodyKind::Runtime(_) => panic!("comptime operation emitted in a runtime body"),
            MIRBodyKind::Comptime(body) => body.push_instr_at(
                self.current_block,
                MIRComptimeInstruction::Comptime { op, token_range },
            ),
        }
    }

    pub fn current_block_terminated(&self) -> bool {
        match &self.kind {
            MIRBodyKind::Runtime(body) => body.block(self.current_block)
                .and_then(|block| block.last_instruction())
                .is_some_and(MIRInstructionLike::is_terminator),
            MIRBodyKind::Comptime(body) => body.block(self.current_block)
                .and_then(|block| block.last_instruction())
                .is_some_and(MIRInstructionLike::is_terminator),
        }
    }

    pub fn has_block(&self, block: MIRBasicBlockID) -> bool {
        match &self.kind {
            MIRBodyKind::Runtime(body) => body.block(block).is_some(),
            MIRBodyKind::Comptime(body) => body.block(block).is_some(),
        }
    }

    pub fn add_block(&mut self, name: Option<CXIdent>) -> MIRBasicBlockID {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => match name {
                Some(name) => body.add_block_named(name),
                None => body.add_block(),
            },
            MIRBodyKind::Comptime(body) => match name {
                Some(name) => body.add_block_named(name),
                None => body.add_block(),
            },
        }
    }

    pub fn add_scope(&mut self, range: TokenRange) -> MIRScopeID {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.add_scope(range),
            MIRBodyKind::Comptime(body) => body.add_scope(range),
        }
    }

    pub fn scope(&self, id: MIRScopeID) -> Option<&MIRScopeDecl> {
        match &self.kind {
            MIRBodyKind::Runtime(body) => body.scope(id),
            MIRBodyKind::Comptime(body) => body.scope(id),
        }
    }

    pub fn add_register(&mut self, ty: MIRTypeID, name: Option<CXIdent>) -> MIRRegisterID {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.add_register(ty, name),
            MIRBodyKind::Comptime(body) => body.add_register(ty, name),
        }
    }

    pub fn register(&self, id: MIRRegisterID) -> Option<&MIRRegisterDecl> {
        match &self.kind {
            MIRBodyKind::Runtime(body) => body.register(id),
            MIRBodyKind::Comptime(body) => body.register(id),
        }
    }

    pub fn add_place(
        &mut self,
        ty: MIRTypeID,
        name: Option<CXIdent>,
        nodrop: bool,
        scope: MIRScopeID,
    ) -> MIRPlaceID {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.add_place(ty, name, nodrop, scope),
            MIRBodyKind::Comptime(body) => body.add_place(ty, name, nodrop, scope),
        }
    }

    pub fn add_parameter(&mut self, parameter: &MIRFnParam, scope: MIRScopeID) -> MIRPlaceID {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.add_parameter(parameter, scope),
            MIRBodyKind::Comptime(body) => body.add_parameter(parameter, scope),
        }
    }

    pub fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        name: Option<CXIdent>,
    ) -> MIRRegisterID {
        match &mut self.kind {
            MIRBodyKind::Runtime(body) => body.add_block_param(block, ty, name),
            MIRBodyKind::Comptime(body) => body.add_block_param(block, ty, name),
        }
    }

    pub fn set_current_block(&mut self, block: MIRBasicBlockID) {
        self.current_block = block;
    }

    pub fn finish(self) -> MIRBodyKind<'thir> {
        self.kind
    }
}
