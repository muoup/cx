use std::collections::HashMap;

use cx_hmir::body::HMIRLocalID;
use cx_log::CXResult;
use cx_mir::{MIRBody, MIRFunctionID, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::MIRFunctionBuilder},
    lower::{HMIRValue, HMIRValueKind},
};

#[derive(Debug)]
pub struct FnLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    builder: MIRFunctionBuilder,

    locals: HashMap<HMIRLocalID, HMIRValue>,
}

impl<'env, 'global, 'hmir> FnLoweringContext<'env, 'global, 'hmir> {
    pub fn new(env: &'env mut HMIREnvironment<'global, 'hmir>, function: MIRFunctionID) -> Self {
        Self {
            env,
            builder: MIRFunctionBuilder::new(function),

            locals: HashMap::new(),
        }
    }

    pub fn env(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn builder(&mut self) -> &mut MIRFunctionBuilder {
        &mut self.builder
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: HMIRValue) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(id)
    }

    pub fn materialize(&mut self, value: &HMIRValue, span: &TokenRange) -> CXResult<MIRValue> {
        Ok(match value.kind() {
            HMIRValueKind::Constant(constant) => {
                MIRValue::Constant(self.env.lower_constant(constant, span)?)
            }
            HMIRValueKind::Register(register) => MIRValue::Register(*register),
            HMIRValueKind::Place(place) => MIRValue::PlaceRef(*place),
        })
    }

    pub fn finish(self) -> MIRBody {
        self.builder.finish()
    }
}
