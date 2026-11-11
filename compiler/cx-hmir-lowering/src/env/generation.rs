use rustc_hash::FxHashMap;
use std::collections::VecDeque;

use cx_hmir::{HMIRDefID, ty::HMIRTypeID};
use cx_mir::MIRFunctionID;

use crate::value::HMIRValue;

/// Something the unit has to generate, as whatever refers to it asks for it
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Generation {
    /// The function or global that a definition declares
    Definition(HMIRDefID),
    /// A function definition completed with the arguments of its comptime parameters, in their order
    Template {
        def: HMIRDefID,
        comptime_bindings: Vec<HMIRValue>,
    },
    /// The function constructing a variant of a tagged union from its payload
    Constructor { ty: HMIRTypeID, variant: usize },
}

///
/// Tracks what the unit has to generate. Each generation is requested at most once, and is generated only after
/// the definition being lowered at the time of the request has finished.
///
#[derive(Debug, Default)]
pub struct GenerationQueue {
    requested: FxHashMap<Generation, Option<MIRFunctionID>>,
    pending: VecDeque<Generation>,
}

impl GenerationQueue {
    pub fn new() -> Self {
        Self::default()
    }

    // 'function' is the function declared for the generation, where no definition declares it
    pub fn request(&mut self, generation: Generation, function: Option<MIRFunctionID>) {
        if !self.requested.contains_key(&generation) {
            self.requested.insert(generation.clone(), function);
            self.pending.push_back(generation);
        }
    }

    pub fn requested_count(&self) -> usize {
        self.requested.len()
    }

    pub fn function(&self, generation: &Generation) -> Option<MIRFunctionID> {
        self.requested.get(generation).copied().flatten()
    }

    pub fn next(&mut self) -> Option<Generation> {
        self.pending.pop_front()
    }
}
