use cx_hmir::{HMIRFnSignature, HMIRFunctionStage, body::HMIRLocalID, value::HMIRValue};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

pub mod comptime;
pub mod runtime;

use crate::env::{HMIREnvironment, function::runtime::Emission};

pub use self::{comptime::ComptimeContext, runtime::RuntimeContext};

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

    pub fn for_signature(
        env: &'env mut HMIREnvironment<'global, 'hmir>,
        signature: &'hmir HMIRFnSignature,
    ) -> Self {
        Self {
            env,
            signature,
            context: FnContext::Comptime(ComptimeContext::new()),
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

    pub fn runtime(&self) -> &RuntimeContext {
        match self.context() {
            FnContext::Runtime(context) => context,
            FnContext::Comptime(_) => unreachable!("runtime context requested in a comptime context"),
        }
    }

    pub fn runtime_mut(&mut self) -> &mut RuntimeContext {
        match self.context_mut() {
            FnContext::Runtime(context) => context,
            FnContext::Comptime(_) => unreachable!("runtime context requested in a comptime context"),
        }
    }

    pub fn comptime(&self) -> &ComptimeContext {
        match self.context() {
            FnContext::Runtime(_) => unreachable!("comptime context requested in a runtime context"),
            FnContext::Comptime(context) => context,
        }
    }

    pub fn comptime_mut(&mut self) -> &mut ComptimeContext {
        match self.context_mut() {
            FnContext::Runtime(_) => unreachable!("comptime context requested in a runtime context"),
            FnContext::Comptime(context) => context,
        }
    }

    // A terminator suppresses emission until evaluation resumes in another block
    pub fn emit(&mut self, kind: MIRInstructionKind, span: TokenRange) {
        let terminator = kind.is_terminator();
    
        self.emit(kind, span);
        if terminator {
            self.set_emission(Emission::Suppressed);
        }
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
