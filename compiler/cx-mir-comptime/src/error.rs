use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::context::MIRContext;

pub fn log_comptime_error<T, C: MIRContext + ?Sized>(context: &C, token_range: TokenRange, message: impl Into<String>) -> CXResult<T> {
    todo!()
}
