use cx_hmir::{HMIRFnSignature, HMIRFunctionStage, body::HMIRLocalID, expr::HMIRExprID};

pub mod comptime;
pub mod elaboration;
pub mod runtime;

use crate::{env::HMIREnvironment, value::HMIRValue};

pub use self::{
    comptime::ComptimeContext, elaboration::ElaborationScope, runtime::RuntimeContext,
};

#[derive(Debug)]
pub struct LocalLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    signature: Option<&'hmir HMIRFnSignature<HMIRExprID>>,
    context: FnContext,
    elaboration: ElaborationScope,
}

#[derive(Debug)]
pub enum FnContext {
    Runtime(RuntimeContext),
    Comptime(ComptimeContext)
}

impl<'env, 'global, 'hmir> LocalLoweringContext<'env, 'global, 'hmir> {
    pub fn new(
        env: &'env mut HMIREnvironment<'global, 'hmir>,
        signature: Option<&'hmir HMIRFnSignature<HMIRExprID>>,
        context: FnContext,
    ) -> Self {
        // A comptime context may stage a runtime signature, as prototype resolution does
        if let Some(signature) = signature {
            assert!(
                context.stage() == HMIRFunctionStage::Comptime
                    || signature.stage() == HMIRFunctionStage::Runtime,
                "comptime signature evaluated in a runtime context"
            );
        }

        Self {
            env,
            signature,
            context,
            elaboration: ElaborationScope::new(),
        }
    }

    pub fn child<'parent>(
        &'parent mut self,
        signature: Option<&'hmir HMIRFnSignature<HMIRExprID>>,
        context: FnContext,
    ) -> LocalLoweringContext<'parent, 'global, 'hmir> {
        LocalLoweringContext::new(&mut *self.env, signature, context)
    }

    pub fn env(&self) -> &HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn env_mut(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn signature(&self) -> Option<&'hmir HMIRFnSignature<HMIRExprID>> {
        self.signature
    }

    pub fn context(&self) -> &FnContext {
        &self.context
    }

    pub fn context_mut(&mut self) -> &mut FnContext {
        &mut self.context
    }

    pub fn runtime(&self) -> &RuntimeContext {
        match self.context() {
            FnContext::Runtime(context) => context,
            FnContext::Comptime(_) => {
                unreachable!("runtime context requested in a comptime context")
            }
        }
    }

    pub fn runtime_mut(&mut self) -> &mut RuntimeContext {
        match self.context_mut() {
            FnContext::Runtime(context) => context,
            FnContext::Comptime(_) => {
                unreachable!("runtime context requested in a comptime context")
            }
        }
    }

    pub fn comptime(&self) -> &ComptimeContext {
        match self.context() {
            FnContext::Runtime(_) => {
                unreachable!("comptime context requested in a runtime context")
            }
            FnContext::Comptime(context) => context,
        }
    }

    pub fn comptime_mut(&mut self) -> &mut ComptimeContext {
        match self.context_mut() {
            FnContext::Runtime(_) => {
                unreachable!("comptime context requested in a runtime context")
            }
            FnContext::Comptime(context) => context,
        }
    }

    pub fn elaboration(&self) -> &ElaborationScope {
        &self.elaboration
    }

    pub fn elaboration_mut(&mut self) -> &mut ElaborationScope {
        &mut self.elaboration
    }

    pub fn into_context(self) -> FnContext {
        self.context
    }

    pub fn remove_local(&mut self, id: HMIRLocalID) {
        match &mut self.context {
            FnContext::Runtime(context) => context.remove_local(id),
            FnContext::Comptime(context) => context.remove_local(id),
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

    pub fn runtime(&self) -> &RuntimeContext {
        match self {
            Self::Runtime(context) => context,
            Self::Comptime(_) => unreachable!("runtime context requested in a comptime context"),
        }
    }

    pub fn runtime_mut(&mut self) -> &mut RuntimeContext {
        match self {
            Self::Runtime(context) => context,
            Self::Comptime(_) => unreachable!("runtime context requested in a comptime context"),
        }
    }

    pub fn comptime(&self) -> &ComptimeContext {
        match self {
            Self::Runtime(_) => unreachable!("comptime context requested in a runtime context"),
            Self::Comptime(context) => context,
        }
    }

    pub fn comptime_mut(&mut self) -> &mut ComptimeContext {
        match self {
            Self::Runtime(_) => unreachable!("comptime context requested in a runtime context"),
            Self::Comptime(context) => context,
        }
    }
}
