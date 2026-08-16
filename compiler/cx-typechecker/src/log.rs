use cx_log::{
    CXResult,
    error::{CXErr, CXErrMsg, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_pipeline_data::db::ModuleData;
use cx_tokens::TokenRange;

fn append_notes(mut message: String, notes: Vec<String>) -> String {
    for note in notes {
        message.push_str("\nnote: ");
        message.push_str(&note);
    }
    message
}

pub fn produce_compile_error(
    prefix: &'static str,
    module_data: &ModuleData,
    range: &TokenRange,
    message: impl Into<String>,
    notes: Vec<String>,
) -> CXErr {
    CXErr::new(
        CXStdErrMessage::error(prefix, append_notes(message.into(), notes)),
        module_data.convert_token_range(range),
    )
}

pub fn produce_(
    module_data: &ModuleData,
    range: &TokenRange,
    message: impl Into<String>,
    notes: Vec<String>,
) -> CXErr {
    produce_compile_error("TYPE ERROR", module_data, range, message, notes)
}

pub fn produce_comptime_error(
    module_data: &ModuleData,
    range: &TokenRange,
    message: impl Into<String>,
    notes: Vec<String>,
) -> CXErr {
    produce_compile_error("COMPTIME ERROR", module_data, range, message, notes)
}

pub fn type_error_msg(message: impl Into<String>) -> CXErrMsg {
    CXStdErrMessage::error("TYPE ERROR", message)
}

pub fn internal_type_error<T>(message: impl Into<String>) -> CXResult<T> {
    Err(CXErr::new(
        type_error_msg(message),
        CXInternalContext::error("typechecker diagnostic has no source range"),
    ))
}
