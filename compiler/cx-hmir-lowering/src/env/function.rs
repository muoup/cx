use std::{cell::Cell, mem};

use cx_hmir::{
    HMIRFnSignature, HMIRFunctionStage, HMIRUnitID, body::HMIRLocalID, expr::HMIRExprID,
    ty::HMIRTypeID, unit::def::HMIRDeclarationPoint,
};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;
use cx_util::dense_id;

pub mod comptime;
pub mod expectation;
pub mod runtime;

use crate::{env::HMIREnvironment, log::hmir_error, value::HMIRValue};

pub use self::{
    comptime::ComptimeContext,
    expectation::{Expectations, Permission},
    runtime::RuntimeContext,
};

thread_local! {
    static NEXT_EVALUATION: Cell<usize> = Cell::new(0);
}
dense_id!(EvaluationID, "ev.");

impl EvaluationID {
    pub fn fresh() -> Self {
        NEXT_EVALUATION.with(|next| {
            let id = next.get();
            next.set(id + 1);
            EvaluationID(id)
        })
    }
}

#[derive(Debug)]
pub struct LocalLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    
    source: HMIRUnitID,
    evaluation: EvaluationID,

    visible_through: Option<HMIRDeclarationPoint>,
    signature: Option<&'env HMIRFnSignature<HMIRExprID>>,
    return_type: Option<HMIRTypeID>,

    context: FnContext,
    expectations: Expectations,

    call_depth: usize,
}

#[derive(Debug)]
pub enum FnContext {
    Runtime(RuntimeContext),
    Comptime(ComptimeContext),
}

impl<'env, 'global, 'hmir> LocalLoweringContext<'env, 'global, 'hmir> {
    pub fn new(
        env: &'env mut HMIREnvironment<'global, 'hmir>,
        signature: Option<&'env HMIRFnSignature<HMIRExprID>>,
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

        let source = env.symbols().local_unit().id();
        Self {
            source,
            return_type: None,
            visible_through: None,
            call_depth: 0,
            env,
            signature,
            context,
            evaluation: EvaluationID::fresh(),
            expectations: Expectations::new(match signature {
                Some(signature) if signature.contract().is_safe() => Permission::Safe,
                _ => Permission::Nonsafe,
            }),
        }
    }

    pub fn child<'parent>(
        &'parent mut self,
        signature: Option<&'parent HMIRFnSignature<HMIRExprID>>,
        context: FnContext,
        evaluation: Option<EvaluationID>,
    ) -> LocalLoweringContext<'parent, 'global, 'hmir> {
        let mut child = LocalLoweringContext::new(&mut *self.env, signature, context);
        if let Some(evaluation) = evaluation {
            child.evaluation = evaluation;
            child.expectations = self.expectations;
        }
        child.source = self.source;
        child.return_type = self.return_type;
        child.visible_through = self.visible_through;
        child.call_depth = self.call_depth;
        child
    }

    pub(crate) fn enter_call(&mut self, span: &TokenRange) -> CXResult<()> {
        const LIMIT: usize = 64;
        if self.call_depth >= LIMIT {
            return Err(hmir_error(
                span,
                &mir::COMPTIME_CALL_DEPTH,
                LIMIT,
            ));
        }
        self.call_depth += 1;
        Ok(())
    }

    pub fn sourced(mut self, source: HMIRUnitID) -> Self {
        self.source = source;
        self
    }

    pub fn source(&self) -> HMIRUnitID {
        self.source
    }

    pub fn at(mut self, point: Option<HMIRDeclarationPoint>) -> Self {
        self.visible_through = point;
        self
    }

    pub fn visible_through(&self) -> Option<HMIRDeclarationPoint> {
        self.visible_through
    }

    pub fn return_type(&self) -> Option<HMIRTypeID> {
        self.return_type
    }

    pub fn with_return_type(mut self, ty: HMIRTypeID) -> Self {
        self.return_type = Some(ty);
        self
    }

    pub fn env(&self) -> &HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn env_mut(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn signature(&self) -> Option<&'env HMIRFnSignature<HMIRExprID>> {
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

    pub fn evaluation(&self) -> EvaluationID {
        self.evaluation
    }

    pub fn permission(&self) -> Permission {
        self.expectations.permission()
    }

    pub fn expectations(&self) -> Expectations {
        self.expectations
    }

    pub fn with_expectations<R>(
        &mut self,
        expectations: Expectations,
        scoped: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let outer = mem::replace(&mut self.expectations, expectations);
        let result = scoped(self);
        self.expectations = outer;
        result
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
