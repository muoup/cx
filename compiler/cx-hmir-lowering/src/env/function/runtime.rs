use rustc_hash::FxHashMap;

use cx_hmir::body::{HMIRBodyID, HMIRLocalID};
use cx_mir::{
    MIRBasicBlockID, MIRFnPrototype, MIRFunction, MIRInstructionKind, MIRIntrinsic, MIRPlaceID,
    MIRRegister,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::{
        function::scope::{Bindings, Scopes},
        module::MIRFunctionBuilder,
    },
    value::{
        HMIRRegisterID, HMIRValue,
        place::{HMIRPlaceBrand, HMIRPlaceID},
    },
};

mod scope;

pub(crate) use scope::{Defer, Exit, RuntimeScope};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Emission {
    Enabled,
    Suppressed,
    Unevaluated,
}

#[derive(Debug)]
pub struct RuntimeContext {
    builder: MIRFunctionBuilder,
    brand: HMIRPlaceBrand,
    emission: Emission,
    // The values known of the locals a detached context binds placeholders for
    frozen: Bindings,

    scopes: Scopes<RuntimeScope>,
    declared: usize,
    labels: FxHashMap<String, MIRBasicBlockID>,
}

impl RuntimeContext {
    pub fn new(prototype: MIRFnPrototype, brand: HMIRPlaceBrand) -> Self {
        Self {
            builder: MIRFunctionBuilder::new(prototype),
            brand,
            emission: Emission::Enabled,
            frozen: Bindings::default(),

            scopes: Scopes::default(),
            declared: 0,
            labels: FxHashMap::default(),
        }
    }

    pub(crate) fn frozen(&self, local: HMIRLocalID) -> Option<&HMIRValue> {
        self.frozen.get(&local)
    }

    pub(crate) fn freeze(&mut self, frozen: Bindings) {
        self.frozen = frozen;
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

    pub fn set_emission(&mut self, emission: Emission) {
        self.emission = emission;
    }

    pub fn emit(&mut self, kind: MIRInstructionKind, span: TokenRange) {
        if self.emission != Emission::Enabled {
            return;
        }
        let terminator = kind.is_terminator();
        self.builder.emit(kind, span);
        if terminator {
            self.emission = Emission::Suppressed;
        }
    }

    pub fn emit_intrinsic(&mut self, intrinsic: impl Into<MIRIntrinsic>, span: TokenRange) {
        self.emit(MIRInstructionKind::IntrinsicOp(intrinsic.into()), span)
    }

    pub(crate) fn register(&self, id: HMIRRegisterID) -> MIRRegister {
        MIRRegister::new(id.0)
    }

    pub(crate) fn brand(&self) -> HMIRPlaceBrand {
        self.brand
    }

    pub(crate) fn place(&self, id: HMIRPlaceID) -> MIRPlaceID {
        debug_assert_eq!(id.brand(), self.brand, "a place of another function");
        MIRPlaceID::new(id.index())
    }

    pub(crate) fn scopes(&self) -> &Scopes<RuntimeScope> {
        &self.scopes
    }

    pub(crate) fn scopes_mut(&mut self) -> &mut Scopes<RuntimeScope> {
        &mut self.scopes
    }

    // Places declared here are dropped when the innermost open scope closes
    pub(crate) fn declare(&mut self, place: HMIRPlaceID) {
        let top = self.scopes.top();
        self.scopes.control_mut(top).declare(place);
        self.declared += 1;
    }

    pub(crate) fn declared_places(&self) -> usize {
        self.declared
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

    pub(crate) fn quoting(&self) -> bool {
        self.scopes.controls().any(|(_, scope)| scope.quote().is_some())
    }

    pub(crate) fn sealed_past(&self, depth: usize) -> bool {
        self.scopes
            .controls()
            .skip(depth)
            .any(|(_, scope)| scope.sealed())
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

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
