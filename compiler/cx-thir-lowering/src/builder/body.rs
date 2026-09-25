use cx_mir::{
    MIRBasicBlockID, MIRBody, MIRComptimeBody, MIRComptimeFnPrototype, MIRComptimeInstruction,
    MIRComptimeOp, MIRComptimeParameter, MIRComptimeRegisterID, MIRComptimeType, MIRFnParam,
    MIRFnPrototype, MIRInstruction, MIRInstructionLike, MIRPlaceID, MIRRegisterDecl, MIRRegisterID,
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
    Runtime {
        prototype: MIRFnPrototype,
        body: MIRBody,
    },
    Comptime {
        prototype: MIRComptimeFnPrototype,
        body: MIRComptimeBody<'thir>,
    },
    ComptimeScratch {
        body: MIRComptimeBody<'thir>,
    },
}

impl<'thir> MIRBodyBuilder<'thir> {
    pub fn new_runtime(prototype: MIRFnPrototype, body: MIRBody) -> Self {
        let current_block = body.entry();
        Self {
            kind: MIRBodyKind::Runtime { prototype, body },
            current_block,
        }
    }

    pub fn new_comptime(prototype: MIRComptimeFnPrototype, body: MIRComptimeBody<'thir>) -> Self {
        let current_block = body.entry();
        Self {
            kind: MIRBodyKind::Comptime { prototype, body },
            current_block,
        }
    }

    pub fn new_comptime_scratch(body: MIRComptimeBody<'thir>) -> Self {
        let current_block = body.entry();
        Self {
            kind: MIRBodyKind::ComptimeScratch { body },
            current_block,
        }
    }

    pub fn runtime_prototype(&self) -> Option<&MIRFnPrototype> {
        match &self.kind {
            MIRBodyKind::Runtime { prototype, .. } => Some(prototype),
            MIRBodyKind::Comptime { .. } | MIRBodyKind::ComptimeScratch { .. } => None,
        }
    }

    pub fn comptime_prototype(&self) -> Option<&MIRComptimeFnPrototype> {
        match &self.kind {
            MIRBodyKind::Comptime { prototype, .. } => Some(prototype),
            MIRBodyKind::Runtime { .. } | MIRBodyKind::ComptimeScratch { .. } => None,
        }
    }

    pub fn is_comptime(&self) -> bool {
        matches!(self.kind, MIRBodyKind::Comptime { .. })
    }

    pub fn emit(&mut self, instruction: MIRInstruction) {
        assert!(
            !self.current_block_terminated(),
            "instruction follows a terminator"
        );
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => {
                body.push_instr_at(self.current_block, instruction)
            }
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => body
                .push_instr_at(
                    self.current_block,
                    MIRComptimeInstruction::Runtime(instruction),
                ),
        }
    }

    #[allow(dead_code)]
    pub fn emit_comptime(&mut self, op: MIRComptimeOp<'thir>, token_range: TokenRange) {
        assert!(
            !self.current_block_terminated(),
            "instruction follows a terminator"
        );
        match &mut self.kind {
            MIRBodyKind::Runtime { .. } => panic!("comptime operation emitted in a runtime body"),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => body
                .push_instr_at(
                    self.current_block,
                    MIRComptimeInstruction::Comptime { op, token_range },
                ),
        }
    }

    pub fn current_block_terminated(&self) -> bool {
        match &self.kind {
            MIRBodyKind::Runtime { body, .. } => body
                .block(self.current_block)
                .and_then(|block| block.last_instruction())
                .is_some_and(MIRInstructionLike::is_terminator),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => body
                .block(self.current_block)
                .and_then(|block| block.last_instruction())
                .is_some_and(MIRInstructionLike::is_terminator),
        }
    }

    pub fn current_block_reachable(&self) -> bool {
        let MIRBodyKind::Runtime { body, .. } = &self.kind else {
            unreachable!("reachability query requires a runtime body")
        };
        let mut visited = vec![false; body.blocks().len()];
        let mut pending = vec![body.entry()];
        while let Some(id) = pending.pop() {
            if visited[id.index()] {
                continue;
            }
            visited[id.index()] = true;
            if id == self.current_block {
                return true;
            }
            if let Some(instruction) = body.block(id).and_then(|block| block.last_instruction()) {
                pending.extend(
                    cx_mir::expr::visit::successors(instruction)
                        .into_iter()
                        .map(|target| target.block),
                );
            }
        }
        false
    }

    pub fn has_block(&self, block: MIRBasicBlockID) -> bool {
        match &self.kind {
            MIRBodyKind::Runtime { body, .. } => body.block(block).is_some(),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.block(block).is_some()
            }
        }
    }

    pub fn block_parameter_type(&self, block: MIRBasicBlockID, index: usize) -> Option<MIRTypeID> {
        let parameter = match &self.kind {
            MIRBodyKind::Runtime { body, .. } => body.block(block)?.params().get(index).copied(),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.block(block)?.params().get(index).copied()
            }
        }?;
        self.register(parameter).map(|register| register.ty)
    }

    pub fn add_block(&mut self, name: Option<CXIdent>) -> MIRBasicBlockID {
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => match name {
                Some(name) => body.add_block_named(name),
                None => body.add_block(),
            },
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                match name {
                    Some(name) => body.add_block_named(name),
                    None => body.add_block(),
                }
            }
        }
    }

    pub fn add_scope(&mut self, range: TokenRange) -> MIRScopeID {
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => body.add_scope(range),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_scope(range)
            }
        }
    }

    pub fn scope(&self, id: MIRScopeID) -> Option<&MIRScopeDecl> {
        match &self.kind {
            MIRBodyKind::Runtime { body, .. } => body.scope(id),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.scope(id)
            }
        }
    }

    pub fn add_register(&mut self, ty: MIRTypeID, name: Option<CXIdent>) -> MIRRegisterID {
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => body.add_register(ty, name),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_register(ty, name)
            }
        }
    }

    pub fn add_comptime_register(
        &mut self,
        ty: MIRComptimeType,
        name: Option<CXIdent>,
    ) -> MIRComptimeRegisterID {
        match &mut self.kind {
            MIRBodyKind::Runtime { .. } => panic!("comptime register in a runtime body"),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_comptime_register(ty, name)
            }
        }
    }

    pub fn add_comptime_parameter(
        &mut self,
        ty: MIRComptimeType,
        name: Option<CXIdent>,
        nodrop: bool,
        scope: MIRScopeID,
    ) -> MIRComptimeParameter {
        match &mut self.kind {
            MIRBodyKind::Runtime { .. } => panic!("comptime parameter in a runtime body"),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_comptime_parameter(ty, name, nodrop, scope)
            }
        }
    }

    #[allow(dead_code)]
    pub fn register(&self, id: MIRRegisterID) -> Option<&MIRRegisterDecl> {
        match &self.kind {
            MIRBodyKind::Runtime { body, .. } => body.register(id),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.register(id)
            }
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
            MIRBodyKind::Runtime { body, .. } => body.add_place(ty, name, nodrop, scope),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_place(ty, name, nodrop, scope)
            }
        }
    }

    pub fn mark_adopted(&mut self, place: MIRPlaceID) {
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => body.mark_adopted(place),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.mark_adopted(place)
            }
        }
    }

    pub fn places_in_scope(&self, scope: MIRScopeID) -> Vec<MIRPlaceID> {
        let places = match &self.kind {
            MIRBodyKind::Runtime { body, .. } => body.places(),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.places()
            }
        };
        places
            .iter()
            .filter(|place| place.scope == scope)
            .rev()
            .map(|place| place.id)
            .collect()
    }

    pub fn add_parameter(&mut self, parameter: &MIRFnParam, scope: MIRScopeID) -> MIRPlaceID {
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => body.add_parameter(parameter, scope),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_parameter(parameter, scope)
            }
        }
    }

    pub fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        name: Option<CXIdent>,
    ) -> MIRRegisterID {
        match &mut self.kind {
            MIRBodyKind::Runtime { body, .. } => body.add_block_param(block, ty, name),
            MIRBodyKind::Comptime { body, .. } | MIRBodyKind::ComptimeScratch { body } => {
                body.add_block_param(block, ty, name)
            }
        }
    }

    pub fn set_current_block(&mut self, block: MIRBasicBlockID) {
        self.current_block = block;
    }

    pub fn finish(self) -> MIRBodyKind<'thir> {
        self.kind
    }
}
