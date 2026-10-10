use std::collections::HashMap;

use cx_hmir::{HMIRFunction, body::HMIRLocalID};
use cx_mir::MIRValue;

use crate::env::HMIREnvironment;

#[derive(Debug)]
pub struct FnLoweringContext<'global, 'hmir> {
    env: &'hmir mut HMIREnvironment<'global, 'hmir>,
    function: &'hmir HMIRFunction,

    locals: HashMap<HMIRLocalID, MIRValue>,
}

impl<'global, 'hmir> FnLoweringContext<'global, 'hmir> {
    pub fn new(
        env: &'hmir mut HMIREnvironment<'global, 'hmir>,
        function: &'hmir HMIRFunction,
    ) -> Self {
        Self {
            env,
            function,

            locals: HashMap::new(),
        }
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: MIRValue) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&MIRValue> {
        self.locals.get(id)
    }
}
