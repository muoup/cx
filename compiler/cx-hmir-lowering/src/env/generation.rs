use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::VecDeque;

use cx_hmir::{HMIRDefID, HMIRFnSignature, ty::HMIRTypeID};
use cx_mir::MIRFunctionID;

use crate::value::HMIRValue;

///
/// Tracks the definitions that the unit has to generate. A definition is requested by whatever refers to it, at
/// most once, and is generated only after the definition being lowered at the time of the request has finished.
///
#[derive(Debug, Default)]
pub struct GenerationQueue {
    requested: FxHashSet<HMIRDefID>,
    pending: VecDeque<Generation>,
    generated: FxHashMap<Generated, MIRFunctionID>,
}

/// A function that no definition declares as it is generated
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Generated {
    /// A function definition completed with the comptime arguments bound to its parameters
    Template {
        def: HMIRDefID,
        bindings: Vec<Option<HMIRValue>>,
    },
    /// The function constructing a variant of a tagged union from its payload
    Constructor { ty: HMIRTypeID, variant: usize },
}

#[derive(Debug)]
pub enum Generation {
    Definition(HMIRDefID),
    Instance(Instance),
}

#[derive(Debug)]
pub struct Instance {
    source: Generated,
    function: MIRFunctionID,
    signature: HMIRFnSignature<HMIRTypeID>,
}

impl GenerationQueue {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn request(&mut self, def: HMIRDefID) {
        if self.requested.insert(def) {
            self.pending.push_back(Generation::Definition(def));
        }
    }

    pub fn generated_count(&self) -> usize {
        self.generated.len()
    }

    pub fn generated(&self, source: &Generated) -> Option<MIRFunctionID> {
        self.generated.get(source).copied()
    }

    // 'instance' must not have been generated before
    pub fn generate(&mut self, instance: Instance) {
        let previous = self.generated.insert(instance.source.clone(), instance.function);
        assert!(previous.is_none(), "a function is generated once");
        self.pending.push_back(Generation::Instance(instance));
    }

    pub fn next(&mut self) -> Option<Generation> {
        self.pending.pop_front()
    }
}

impl Instance {
    pub fn new(
        source: Generated,
        function: MIRFunctionID,
        signature: HMIRFnSignature<HMIRTypeID>,
    ) -> Self {
        Self {
            source,
            function,
            signature,
        }
    }

    pub fn source(&self) -> &Generated {
        &self.source
    }

    pub fn function(&self) -> MIRFunctionID {
        self.function
    }

    // The values the generated function has bound, by parameter of the source
    pub fn bindings(&self) -> &[Option<HMIRValue>] {
        match &self.source {
            Generated::Template { bindings, .. } => bindings,
            Generated::Constructor { .. } => &[],
        }
    }

    // The signature of the source, which keeps the parameters that the generated function has bound
    pub fn signature(&self) -> &HMIRFnSignature<HMIRTypeID> {
        &self.signature
    }
}
