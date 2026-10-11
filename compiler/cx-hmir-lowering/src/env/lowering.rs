use cx_hmir::{HMIRFnSignature, HMIRFunctionStage, body::HMIRLocalID, value::HMIRValue};

use crate::env::HMIREnvironment;

pub use crate::env::{comptime::ComptimeContext, runtime::RuntimeContext};

#[derive(Debug)]
pub struct FnLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    signature: &'hmir HMIRFnSignature,
    context: FnContext,
}

#[derive(Debug)]
pub enum FnContext {
    Runtime(RuntimeContext),
    Comptime(ComptimeContext),
}

impl<'env, 'global, 'hmir> FnLoweringContext<'env, 'global, 'hmir> {
    pub fn new(
        env: &'env mut HMIREnvironment<'global, 'hmir>,
        signature: &'hmir HMIRFnSignature,
        context: FnContext,
    ) -> Self {
        assert_eq!(signature.stage(), context.stage());

        Self {
            env,
            signature,
            context,
        }
    }

    pub fn child<'parent>(
        parent: &'parent mut Self,
        signature: Option<&'hmir HMIRFnSignature>,
        context: ComptimeContext,
    ) -> FnLoweringContext<'parent, 'global, 'hmir> {
        if let Some(signature) = signature {
            assert_eq!(signature.stage(), HMIRFunctionStage::Comptime);
        }

        FnLoweringContext {
            env: &mut *parent.env,
            signature: signature.unwrap_or(parent.signature),
            context: FnContext::Comptime(context),
        }
    }

    pub fn env(&self) -> &HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn env_mut(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn signature(&self) -> &'hmir HMIRFnSignature {
        self.signature
    }

    pub fn context(&self) -> &FnContext {
        &self.context
    }

    pub fn context_mut(&mut self) -> &mut FnContext {
        &mut self.context
    }

    pub fn into_context(self) -> FnContext {
        self.context
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: HMIRValue) {
        match &mut self.context {
            FnContext::Runtime(context) => context.set_local(id, value),
            FnContext::Comptime(context) => context.set_local(id, value),
        };
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&HMIRValue> {
        match &self.context {
            FnContext::Runtime(context) => context.local(*id),
            FnContext::Comptime(context) => context.local(*id),
        }
    }
}

impl FnContext {
    pub fn stage(&self) -> HMIRFunctionStage {
        match self {
            Self::Runtime(_) => HMIRFunctionStage::Runtime,
            Self::Comptime(_) => HMIRFunctionStage::Comptime,
        }
    }
}
