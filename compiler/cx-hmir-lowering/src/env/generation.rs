use std::collections::{HashMap, HashSet, VecDeque, hash_map::Entry};

use cx_hmir::{HMIRDefID, HMIRFnSignature, ty::HMIRTypeID};
use cx_mir::MIRFunctionID;

use crate::value::{HMIRValue, constant::HMIRFunctionRef};

///
/// Tracks the definitions that the unit has to generate. A definition is requested by whatever refers to it, at
/// most once, and is generated only after the definition being lowered at the time of the request has finished.
///
#[derive(Debug, Default)]
pub struct GenerationQueue {
    requested: HashSet<HMIRDefID>,
    pending: VecDeque<Generation>,
    generated: HashMap<(HMIRFunctionRef, Vec<HMIRValue>), MIRFunctionID>,
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

    pub fn generated(
        &self,
        function: HMIRFunctionRef,
        args: &[HMIRValue],
    ) -> Option<MIRFunctionID> {
        self.generated.get(&(function, args.to_vec())).copied()
    }

    pub fn specialize(&mut self, args: Vec<HMIRValue>, specialization: Specialization) {
        let key = (HMIRFunctionRef::Def(specialization.def), args);
        if let Entry::Vacant(entry) = self.generated.entry(key) {
            entry.insert(specialization.function());
            self.pending
                .push_back(Generation::Specialization(specialization));
        }
    }

    pub fn construct(&mut self, constructor: Constructor) {
        let function = HMIRFunctionRef::Constructor {
            ty: constructor.ty,
            variant: constructor.variant,
        };
        if let Entry::Vacant(entry) = self.generated.entry((function, Vec::new())) {
            entry.insert(constructor.function);
            self.pending.push_back(Generation::Constructor(constructor));
        }
    }

    pub fn next(&mut self) -> Option<Generation> {
        self.pending.pop_front()
    }
}

#[derive(Debug)]
pub enum Generation {
    Definition(HMIRDefID),
    Specialization(Specialization),
    Constructor(Constructor),
}

#[derive(Debug)]
pub struct Specialization {
    def: HMIRDefID,
    function: MIRFunctionID,
    signature: HMIRFnSignature<HMIRTypeID>,
    bindings: Vec<Option<HMIRValue>>,
}

impl Specialization {
    pub fn new(
        def: HMIRDefID,
        function: MIRFunctionID,
        signature: HMIRFnSignature<HMIRTypeID>,
        bindings: Vec<Option<HMIRValue>>,
    ) -> Self {
        Self {
            def,
            function,
            signature,
            bindings,
        }
    }

    pub fn def(&self) -> HMIRDefID {
        self.def
    }
    pub fn function(&self) -> MIRFunctionID {
        self.function
    }
    pub fn signature(&self) -> &HMIRFnSignature<HMIRTypeID> {
        &self.signature
    }
    pub fn bindings(&self) -> &[Option<HMIRValue>] {
        &self.bindings
    }
}

#[derive(Debug)]
pub struct Constructor {
    ty: HMIRTypeID,
    variant: usize,
    function: MIRFunctionID,
}

impl Constructor {
    pub fn new(ty: HMIRTypeID, variant: usize, function: MIRFunctionID) -> Self {
        Self {
            ty,
            variant,
            function,
        }
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }
    pub fn variant(&self) -> usize {
        self.variant
    }
    pub fn function(&self) -> MIRFunctionID {
        self.function
    }
}
