use cx_log::CXResult;
use cx_mir::{MIRFnPrototype};
use cx_tokens::TokenRange;

use crate::error::log_comptime_error;

pub trait MIRContext {
    fn current_prototype(&self) -> &MIRFnPrototype;

    fn log_error<T>(&self, range: TokenRange, message: impl Into<String>) -> CXResult<T> {
        log_comptime_error(self, range, message)
    }
}