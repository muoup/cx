pub mod error;
pub mod pretty;
pub mod span;

pub use error::{
    CXError, CXErrorBase, CXErrorContext, CXErrorMessage, CXRawResult, CXResult, CXUnspannedError,
    PointingError, UnderlineError, complete_raw_result,
};
pub use pretty::*;
pub use span::{DiagnosticPointer, DiagnosticSpan, produce_diagnostic_error};
