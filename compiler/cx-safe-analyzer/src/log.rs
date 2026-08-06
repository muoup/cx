use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_thir::{EnvironmentNamespace, thir::expression::THIRExpression};
use cx_pipeline_data::db::ModuleData;
use cx_safe_ir::ast::FMIRNode;
use cx_tokens::TokenRange;

use crate::{AnalysisDiagnosticContext, thir_conversion::environment::FMIREnvironment};

pub(crate) trait AnalysisDiagnosticSource {
    fn module_data(&self) -> &ModuleData;
    fn current_namespace(&self) -> &EnvironmentNamespace;

    fn error(&self, range_source: impl AnalysisRange, message: impl Into<String>) -> CXErr {
        let message = format!(
            "{}\nnote: current analysis namespace: {}",
            message.into(),
            self.current_namespace()
        );
        CXErr::new(
            CXStdErrMessage::error("ANALYSIS ERROR", message),
            self.module_data()
                .convert_token_range(range_source.token_range()),
        )
    }

    fn log_error<T>(
        &self,
        range_source: impl AnalysisRange,
        message: impl Into<String>,
    ) -> CXResult<T> {
        Err(self.error(range_source, message))
    }
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

impl AnalysisRange for THIRExpression {
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

pub(crate) fn internal_analysis_error<T>(message: impl Into<String>) -> CXResult<T> {
    Err(CXErr::new(
        CXStdErrMessage::error("ANALYSIS ERROR", message.into()),
        CXInternalContext::error("analysis diagnostic has no source range"),
    ))
}
