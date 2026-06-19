use std::path::PathBuf;

use cx_log::{CXError, CXUnspannedError, DiagnosticSpan};
use cx_mir::{EnvironmentNamespace, mir::expression::MIRExpression};
use cx_pipeline_data::db::ModuleData;
use cx_safe_ir::ast::FMIRNode;
use cx_tokens::TokenRange;

use crate::{AnalysisDiagnosticContext, mir_conversion::environment::FMIREnvironment};

pub(crate) trait AnalysisDiagnosticSource {
    fn module_data(&self) -> &ModuleData;
    fn current_namespace(&self) -> &EnvironmentNamespace;
}

impl AnalysisDiagnosticSource for FMIREnvironment<'_> {
    fn module_data(&self) -> &ModuleData {
        self.module_data
    }

    fn current_namespace(&self) -> &EnvironmentNamespace {
        &self.current_namespace
    }
}

impl AnalysisDiagnosticSource for AnalysisDiagnosticContext<'_> {
    fn module_data(&self) -> &ModuleData {
        self.module_data()
    }

    fn current_namespace(&self) -> &EnvironmentNamespace {
        self.current_namespace()
    }
}

impl<T: AnalysisDiagnosticSource + ?Sized> AnalysisDiagnosticSource for &T {
    fn module_data(&self) -> &ModuleData {
        (*self).module_data()
    }

    fn current_namespace(&self) -> &EnvironmentNamespace {
        (*self).current_namespace()
    }
}

impl<T: AnalysisDiagnosticSource + ?Sized> AnalysisDiagnosticSource for &mut T {
    fn module_data(&self) -> &ModuleData {
        (**self).module_data()
    }

    fn current_namespace(&self) -> &EnvironmentNamespace {
        (**self).current_namespace()
    }
}

pub(crate) trait AnalysisRange {
    fn token_range(&self) -> &TokenRange;
}

impl AnalysisRange for MIRExpression {
    fn token_range(&self) -> &TokenRange {
        &self.token_range
    }
}

impl AnalysisRange for FMIRNode {
    fn token_range(&self) -> &TokenRange {
        &self.token_range
    }
}

impl<T: AnalysisRange + ?Sized> AnalysisRange for &T {
    fn token_range(&self) -> &TokenRange {
        (*self).token_range()
    }
}

pub(crate) fn produce_analysis_error(
    source: impl AnalysisDiagnosticSource,
    range_source: impl AnalysisRange,
    message: String,
) -> Box<dyn CXError> {
    let current_namespace = source.current_namespace().clone();
    let range = range_source.token_range();
    match range {
        TokenRange::Source { namespace, .. } => {
            let module_data = source.module_data();
            let tokens = module_data.lex_tokens.get(namespace);
            let fallback_file = module_data
                .unit_for_namespace(namespace)
                .map(|unit| unit.as_path().to_owned())
                .unwrap_or_else(|| PathBuf::from(namespace.identifier()));
            let span = range
                .to_diagnostic_span(tokens.as_ref(), fallback_file.as_path())
                .unwrap_or_else(|| DiagnosticSpan::new(fallback_file, 0, 1));

            cx_log::produce_diagnostic_error("ANALYSIS ERROR", message, Vec::new(), span)
        }
        TokenRange::Internal => Box::new(
            CXUnspannedError::new("ANALYSIS ERROR", message).with_notes(vec![
                "diagnostic originated in compiler-generated code".to_string(),
                format!("current analysis namespace: {current_namespace}"),
            ]),
        ),
        TokenRange::Error(range_error) => Box::new(
            CXUnspannedError::new("ANALYSIS ERROR", message).with_notes(vec![
                format!("failed to determine source range: {range_error}"),
                format!("current analysis namespace: {current_namespace}"),
            ]),
        ),
    }
}

#[macro_export]
macro_rules! log_analysis_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!($($arg)*);
            Err($crate::log::produce_analysis_error($env, $expr, message))
        }
    };
}
