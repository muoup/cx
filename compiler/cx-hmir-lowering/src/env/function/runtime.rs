use std::collections::HashMap;

use cx_hmir::body::HMIRLocalID;
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRFnPrototype, MIRFunction, MIRInstructionKind, MIRPlaceDecl,
    MIRScopeID, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;
use index_vec::IndexVec;

use crate::{
    env::module::MIRFunctionBuilder,
    value::{HMIRSlotID, HMIRValue},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Emission {
    Enabled,
    Suppressed,
}

// Where 'break' and 'continue' lead, and how many scopes were open around the loop they leave
#[derive(Debug, Clone, Copy)]
pub struct LoopFrame {
    pub continue_block: MIRBasicBlockID,
    pub break_block: MIRBasicBlockID,
    pub scope_depth: usize,
}

#[derive(Debug)]
pub struct RuntimeContext {
    builder: MIRFunctionBuilder,
    locals: HashMap<HMIRLocalID, HMIRValue>,
    slots: IndexVec<HMIRSlotID, MIRBindable>,
    open_scopes: Vec<MIRScopeID>,
    scratch_scope: MIRScopeID,
    loops: Vec<LoopFrame>,
    lifts: Vec<(HMIRSlotID, MIRBasicBlockID)>,
    emission: Emission,
}

impl RuntimeContext {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        let mut builder = MIRFunctionBuilder::new(prototype);
        let scratch_scope = builder.add_scope(TokenRange::internal());

        Self {
            builder,
            locals: HashMap::new(),
            slots: IndexVec::new(),
            open_scopes: Vec::new(),
            scratch_scope,
            loops: Vec::new(),
            lifts: Vec::new(),
            emission: Emission::Enabled,
        }
    }

    pub fn builder(&self) -> &MIRFunctionBuilder {
        &self.builder
    }

    pub fn builder_mut(&mut self) -> &mut MIRFunctionBuilder {
        &mut self.builder
    }

    pub fn emission(&self) -> Emission {
        self.emission
    }

    pub fn set_emission(&mut self, emission: Emission) -> Emission {
        std::mem::replace(&mut self.emission, emission)
    }

    pub fn emit(&mut self, kind: MIRInstructionKind, span: TokenRange) {
        if self.emission == Emission::Suppressed {
            return;
        }
        
        let terminator = kind.is_terminator();
        if matches!(kind, MIRInstructionKind::Jump { .. } | MIRInstructionKind::Branch { .. }) {
            self.preserve_lifts(&span);
        }
        self.builder.emit(kind, span);
        
        if terminator {
            self.set_emission(Emission::Suppressed);
        }
    }

    pub fn add_lift(&mut self, slot: HMIRSlotID) {
        let block = self.builder.current_block();
        self.lifts.push((slot, block));
    }

    pub fn lift_count(&self) -> usize {
        self.lifts.len()
    }

    pub fn release_lifts(&mut self, count: usize) {
        self.lifts.truncate(count);
    }

    // A lift register is only valid in its defining block, so before evaluation continues in another block, a
    // capture that may still be used is moved into scratch storage, which carries no obligations
    fn preserve_lifts(&mut self, span: &TokenRange) {
        let block = self.builder.current_block();
        let (preserved, pending) = std::mem::take(&mut self.lifts)
            .into_iter()
            .partition(|(_, defined)| *defined == block);
        self.lifts = pending;

        for (slot, _) in preserved {
            let MIRBindable::Register(register) = self.slots[slot] else {
                continue;
            };
            let ty = self
                .builder
                .register(register)
                .expect("a lift register is declared")
                .ty();
            let place = self.builder.allocate_place(MIRPlaceDecl::new(
                ty,
                None,
                self.scratch_scope,
                false,
                false,
            ));
            self.builder.emit(
                MIRInstructionKind::Store {
                    target: MIRTarget::Place(place),
                    value: MIRValue::Register(register),
                    ty,
                    bitfield: None,
                },
                span.clone(),
            );
            self.builder.emit(
                MIRInstructionKind::Initialize {
                    place: MIRBindable::Place(place),
                },
                span.clone(),
            );
            self.slots[slot] = MIRBindable::Place(place);
        }
    }

    pub fn slots(&self) -> impl Iterator<Item = &MIRBindable> {
        self.slots.iter()
    }

    pub fn slot(&self, slot: HMIRSlotID) -> &MIRBindable {
        &self.slots[slot]
    }

    pub fn slot_mut(&mut self, slot: HMIRSlotID) -> &mut MIRBindable {
        &mut self.slots[slot]
    }

    pub fn allocate_slot(&mut self, bindable: MIRBindable) -> HMIRSlotID {
        self.slots.push(bindable)
    }
    
    pub fn locals(&self) -> impl Iterator<Item = (&HMIRLocalID, &HMIRValue)> {
        self.locals.iter()
    }

    pub fn local(&self, local: HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(&local)
    }

    pub fn set_local(&mut self, local: HMIRLocalID, value: HMIRValue) {
        self.locals.insert(local, value);
    }

    pub fn remove_local(&mut self, local: HMIRLocalID) {
        self.locals.remove(&local);
    }

    pub fn open_scopes(&self) -> &[MIRScopeID] {
        &self.open_scopes
    }

    pub fn scratch_scope(&self) -> MIRScopeID {
        self.scratch_scope
    }

    pub fn push_scope(&mut self, scope: MIRScopeID) {
        self.open_scopes.push(scope);
    }

    pub fn pop_scope(&mut self) -> MIRScopeID {
        self.open_scopes
            .pop()
            .expect("Attempt to pop a scope when none is open")
    }

    pub fn current_loop(&self) -> Option<LoopFrame> {
        self.loops.last().copied()
    }

    pub fn push_loop(&mut self, frame: LoopFrame) {
        self.loops.push(frame);
    }

    pub fn pop_loop(&mut self) -> LoopFrame {
        self.loops
            .pop()
            .expect("Attempt to pop a loop when none is open")
    }

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
