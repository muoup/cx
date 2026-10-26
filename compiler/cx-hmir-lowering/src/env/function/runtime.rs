use std::collections::HashMap;

use cx_hmir::body::HMIRBodyID;
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRFnPrototype, MIRFunction, MIRInstructionKind, MIRScopeID,
    MIRTarget,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use index_vec::IndexVec;

use crate::{
    env::{function::scope::Scopes, module::MIRFunctionBuilder},
    value::{HMIRSlotID, HMIRValue},
};

mod scope;
mod slot;

pub(crate) use scope::{Defer, Exit, RuntimeScope};
pub(crate) use slot::Slot;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Emission {
    Enabled,
    Suppressed,
    Unevaluated,
}

#[derive(Debug)]
pub struct RuntimeContext {
    builder: MIRFunctionBuilder,
    emission: Emission,
    slots: IndexVec<HMIRSlotID, Slot>,
    scopes: Scopes<RuntimeScope>,
    scratch_scope: MIRScopeID,
    labels: HashMap<String, MIRBasicBlockID>,
}

impl RuntimeContext {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        let mut builder = MIRFunctionBuilder::new(prototype);
        let scratch_scope = builder.add_scope(TokenRange::internal());

        Self {
            builder,
            slots: IndexVec::new(),
            scopes: Scopes::default(),
            scratch_scope,
            emission: Emission::Enabled,
            labels: HashMap::new(),
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
                        for slot in &mut self.slots {
                            slot.set_known(None);
                        }
                        None
                    }
                    _ => None,
                };
                if let Some(written) = written {
                    for slot in &mut self.slots {
                        if slot.binding() == &written {
                            slot.set_known(None);
                        }
                    }
                }
            }
            return;
        }

        let terminator = kind.is_terminator();
        self.builder.emit(kind, span);

        if terminator {
            self.set_emission(Emission::Suppressed);
        }
    }

    pub(crate) fn slot(&self, slot: HMIRSlotID) -> &Slot {
        &self.slots[slot]
    }

    pub(crate) fn slot_mut(&mut self, slot: HMIRSlotID) -> &mut Slot {
        &mut self.slots[slot]
    }

    pub(crate) fn allocate_slot(&mut self, slot: Slot) -> HMIRSlotID {
        self.slots.push(slot)
    }

    pub(crate) fn scopes(&self) -> &Scopes<RuntimeScope> {
        &self.scopes
    }

    pub(crate) fn scopes_mut(&mut self) -> &mut Scopes<RuntimeScope> {
        &mut self.scopes
    }

    pub(crate) fn current_scope(&self) -> MIRScopeID {
        self.scopes
            .controls()
            .rev()
            .find_map(|(_, scope)| scope.mir())
            .expect("Place allocated outside of a scope")
    }

    pub(crate) fn deferred(&self) -> bool {
        self.scopes.controls().any(|(_, scope)| scope.deferred())
    }

    pub(crate) fn target<T>(
        &self,
        find: impl Fn(&RuntimeScope) -> Option<T>,
    ) -> Option<(usize, T)> {
        for (index, scope) in self.scopes.controls().rev() {
            if let Some(target) = find(scope) {
                return Some((index, target));
            }
            if scope.deferred() {
                break;
            }
        }
        None
    }

    pub(crate) fn defers_past(&self, depth: usize) -> bool {
        self.scopes
            .controls()
            .skip(depth)
            .any(|(_, scope)| !scope.defers().is_empty())
    }

    // A yield written in 'body' targets only the scopes of quotes materialized from that body, so a quote spliced
    // into another quote's code passes over that quote's yielding blocks
    pub(crate) fn yield_target(&self, body: HMIRBodyID) -> Option<usize> {
        let frame = |depth: usize| {
            self.scopes
                .controls()
                .rev()
                .filter(|(index, _)| *index < depth)
                .find_map(|(_, scope)| scope.quote())
        };
        let mut current = frame(self.scopes.len());
        for (index, scope) in self.scopes.controls().rev() {
            if scope.yields() && current.is_none_or(|current| current == body) {
                return Some(index);
            }
            if scope.deferred() {
                break;
            }
            if scope.quote().is_some() {
                current = frame(index);
            }
        }
        None
    }

    pub(crate) fn yield_value(&mut self, depth: usize, value: HMIRValue) {
        let block = self.builder.current_block();
        self.scopes.control_mut(depth).push_yield(block, value);
    }

    pub(crate) fn label(&mut self, name: &CXIdent) -> MIRBasicBlockID {
        if let Some(block) = self.labels.get(name.as_str()) {
            return *block;
        }
        let block = self.builder.add_block(Some(name.clone()));
        self.labels.insert(name.to_string(), block);
        block
    }

    pub fn scratch_scope(&self) -> MIRScopeID {
        self.scratch_scope
    }

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
