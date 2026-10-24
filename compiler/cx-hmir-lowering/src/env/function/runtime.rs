use std::collections::HashMap;

use cx_hmir::{
    body::HMIRLocalID,
    expr::HMIRExprID,
};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRFnPrototype, MIRFunction, MIRInstructionKind, MIRPlaceDecl,
    MIRScopeID, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;
use index_vec::IndexVec;

use crate::{
    env::module::MIRFunctionBuilder,
    value::{HMIRPlace, HMIRSlotID, HMIRValue, HMIRValueKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Emission {
    Enabled,
    Suppressed,
    Unevaluated,
}

#[derive(Debug)]
pub(crate) struct ExitFrame {
    continue_block: Option<MIRBasicBlockID>,
    break_block: MIRBasicBlockID,
    scope_depth: usize,
    case_blocks: Vec<(HMIRExprID, MIRBasicBlockID)>,
}

impl ExitFrame {
    pub(crate) fn new(
        break_block: MIRBasicBlockID,
        continue_block: Option<MIRBasicBlockID>,
        scope_depth: usize,
        case_blocks: Vec<(HMIRExprID, MIRBasicBlockID)>,
    ) -> Self {
        Self { continue_block, break_block, scope_depth, case_blocks }
    }

    pub(crate) fn continue_block(&self) -> Option<MIRBasicBlockID> {
        self.continue_block
    }

    pub(crate) fn break_block(&self) -> MIRBasicBlockID {
        self.break_block
    }

    pub(crate) fn scope_depth(&self) -> usize {
        self.scope_depth
    }

    pub(crate) fn case_block(&self, case: HMIRExprID) -> Option<MIRBasicBlockID> {
        self.case_blocks
            .iter()
            .find_map(|(label, block)| (*label == case).then_some(*block))
    }
}

#[derive(Debug)]
struct YieldFrame {
    depth: usize,
    values: Vec<(MIRBasicBlockID, HMIRValue)>,
}

#[derive(Debug)]
pub struct RuntimeContext {
    builder: MIRFunctionBuilder,
    locals: HashMap<HMIRLocalID, HMIRValue>,
    captures: Vec<(HMIRLocalID, HMIRValue)>,
    slots: IndexVec<HMIRSlotID, MIRBindable>,
    open_scopes: Vec<MIRScopeID>,
    scratch_scope: MIRScopeID,
    exits: Vec<ExitFrame>,
    lifts: Vec<(HMIRSlotID, MIRBasicBlockID)>,
    yields: Vec<YieldFrame>,
    emission: Emission,
}

impl RuntimeContext {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        let mut builder = MIRFunctionBuilder::new(prototype);
        let scratch_scope = builder.add_scope(TokenRange::internal());

        Self {
            builder,
            locals: HashMap::new(),
            captures: Vec::new(),
            slots: IndexVec::new(),
            open_scopes: Vec::new(),
            scratch_scope,
            exits: Vec::new(),
            lifts: Vec::new(),
            yields: Vec::new(),
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
        if self.emission != Emission::Enabled {
            if self.emission == Emission::Unevaluated {
                let written = match &kind {
                    MIRInstructionKind::Store {
                        target: MIRTarget::Place(place),
                        ..
                    } => Some(MIRBindable::Place(*place)),
                    MIRInstructionKind::Invalidate { place, .. } => Some(place.clone()),
                    MIRInstructionKind::Store {
                        target: MIRTarget::Indirect(_) | MIRTarget::Global(_),
                        ..
                    } => {
                        self.captures.clear();
                        None
                    }
                    _ => None,
                };
                if let Some(written) = written {
                    self.captures.retain(|(local, _)| {
                        match self.locals.get(local).map(HMIRValue::kind) {
                            Some(HMIRValueKind::PlaceRef(HMIRPlace::Local(slot))) => {
                                self.slots[*slot] != written
                            }
                            _ => true,
                        }
                    });
                }
            }
            return;
        }

        let terminator = kind.is_terminator();
        if matches!(
            kind,
            MIRInstructionKind::Jump { .. }
                | MIRInstructionKind::Branch { .. }
                | MIRInstructionKind::CaseBranch { .. }
        ) {
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
    
    pub fn captures(&self) -> &[(HMIRLocalID, HMIRValue)] {
        &self.captures
    }

    pub fn set_captures(&mut self, captures: Vec<(HMIRLocalID, HMIRValue)>) {
        self.captures = captures;
    }

    pub fn locals(&self) -> impl Iterator<Item = (&HMIRLocalID, &HMIRValue)> {
        self.locals.iter()
    }

    pub fn local(&self, local: HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(&local)
    }

    pub fn set_local(&mut self, local: HMIRLocalID, value: HMIRValue) {
        self.captures.retain(|(id, _)| *id != local);
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

    pub(crate) fn current_exit(&self) -> Option<&ExitFrame> {
        self.exits.last()
    }

    pub(crate) fn current_loop(&self) -> Option<&ExitFrame> {
        self.exits
            .iter()
            .rev()
            .find(|frame| frame.continue_block().is_some())
    }

    pub(crate) fn in_switch(&self) -> bool {
        self.exits.iter().any(|frame| frame.continue_block().is_none())
    }

    pub(crate) fn case_block(&self, case: HMIRExprID) -> Option<MIRBasicBlockID> {
        self.exits
            .iter()
            .rev()
            .find_map(|frame| frame.case_block(case))
    }

    pub(crate) fn push_exit(&mut self, frame: ExitFrame) {
        self.exits.push(frame);
    }

    pub(crate) fn pop_exit(&mut self) -> ExitFrame {
        self.exits
            .pop()
            .expect("Attempt to pop an exit frame when none is open")
    }

    pub fn begin_yield(&mut self, depth: usize) {
        self.yields.push(YieldFrame {
            depth,
            values: Vec::new(),
        });
    }

    pub fn yielding(&self) -> bool {
        !self.yields.is_empty()
    }

    pub fn yield_value(&mut self, value: HMIRValue) -> usize {
        let block = self.builder.current_block();
        let frame = self.yields.last_mut().expect("yield has a target");
        frame.values.push((block, value));
        frame.depth
    }

    pub fn end_yield(&mut self) -> Vec<(MIRBasicBlockID, HMIRValue)> {
        self.yields.pop().expect("yield scope exists").values
    }

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
