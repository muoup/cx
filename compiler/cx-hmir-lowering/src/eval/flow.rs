use cx_hmir::{HMIRFunctionStage, value::HMIRValue};
use cx_log::error::CXError;

use crate::env::lowering::FnContext;

pub(crate) type EvalResult<T> = Result<T, EvalStop>;

pub(crate) enum EvalStop {
    Comptime(ComptimeStop),
    Runtime(RuntimeStop),
    Diagnostic(CXError),
}

#[derive(Debug)]
pub(crate) enum ComptimeStop {
    Return(HMIRValue),
    Yield(HMIRValue),
    Break,
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RuntimeStop {
    Diverged,
}

impl EvalStop {
    pub fn assert_context(&self, context: &FnContext) {
        match self {
            Self::Comptime(_) => assert_eq!(context.stage(), HMIRFunctionStage::Comptime),
            Self::Runtime(_) => assert_eq!(context.stage(), HMIRFunctionStage::Runtime),
            Self::Diagnostic(_) => {}
        }
    }
}

impl From<CXError> for EvalStop {
    fn from(error: CXError) -> Self {
        Self::Diagnostic(error)
    }
}

impl From<ComptimeStop> for EvalStop {
    fn from(stop: ComptimeStop) -> Self {
        Self::Comptime(stop)
    }
}

impl From<RuntimeStop> for EvalStop {
    fn from(stop: RuntimeStop) -> Self {
        Self::Runtime(stop)
    }
}
