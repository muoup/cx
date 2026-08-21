use cx_log::CXResult;
use cx_mir::MIRFunction;
use cx_tokens::TokenRange;

use crate::error::log_comptime_error;

pub trait MIRContext {
    fn current_function(&self) -> &MIRFunction;

    fn log_error<T>(&self, range: TokenRange, message: impl Into<String>) -> CXResult<T> {
        log_comptime_error(self, range, message)
    }
}