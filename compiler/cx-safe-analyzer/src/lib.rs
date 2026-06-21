use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

use cx_log::{CXResult, CXUnspannedError};
use cx_mir::mir::data::{MIRFunction, MIRFunctionPrototype};
use cx_mir::registry::MIRDecomposedRegistry;
use cx_mir::{EnvironmentNamespace, MIRUnit};
use cx_pipeline_data::db::ModuleData;
use cx_safe_ir::ast::{FMIRFunction, FMIRNode};
use cx_tokens::TokenRange;

use crate::mir_conversion::{convert_mir, environment::FMIREnvironment};
use crate::simplify::assert_proven_conditions;
use crate::traversal::VisitControl;

pub(crate) mod log;
pub(crate) mod mir_conversion;
pub(crate) mod simplify;
pub(crate) mod traversal;

pub type FMIRAnalysisPass<'a> = &'a dyn Fn(&FMIRContext, FMIRFunction) -> CXResult<FMIRFunction>;

pub struct FMIRContext<'a> {
    env: FMIREnvironment<'a>,
    functions: HashMap<String, FMIRFunction>,
}

pub(crate) struct AnalysisDiagnosticContext<'a> {
    current_namespace: EnvironmentNamespace,
    module_data: &'a ModuleData,
    function_name: String,
}

impl<'a> AnalysisDiagnosticContext<'a> {
    fn new(
        function_prototype: &MIRFunctionPrototype,
        current_namespace: EnvironmentNamespace,
        module_data: &'a ModuleData,
    ) -> Self {
        Self {
            current_namespace,
            module_data,
            function_name: function_prototype.name().to_owned(),
        }
    }

    pub(crate) fn current_namespace(&self) -> &EnvironmentNamespace {
        &self.current_namespace
    }

    pub(crate) fn module_data(&self) -> &ModuleData {
        self.module_data
    }

    fn source_text_for_range(&self, range: &TokenRange) -> CXResult<String> {
        let TokenRange::Source {
            namespace,
            start_token,
            end_token,
        } = range
        else {
            return CXUnspannedError::result(
                "ANALYSIS ERROR",
                "Cannot resolve source text for a non-source token range",
            );
        };

        let tokens = self.module_data.lex_tokens.get(namespace);
        let start_token = tokens.get(*start_token).ok_or_else(|| {
            CXStdErrorMsg::error(
                "ANALYSIS ERROR",
                format!("Invalid source range: start token index {start_token} out of bounds"),
            )
        })?;
        let end_token = tokens.get(end_token.saturating_sub(1)).ok_or_else(|| {
            CXStdErrorMsg::error(
                "ANALYSIS ERROR",
                format!("Invalid source range: end token index {end_token} out of bounds"),
            )
        })?;
        if start_token.file_origin != end_token.file_origin {
            return CXUnspannedError::result(
                "ANALYSIS ERROR",
                format!(
                    "Source range tokens have different file origins: {} and {}",
                    start_token.file_origin.display(),
                    end_token.file_origin.display()
                ),
            );
        }

        let source_path = if start_token.file_origin.as_os_str().is_empty() {
            self.module_data
                .unit_for_namespace(namespace)
                .map(|unit| unit.as_path().to_owned())
                .unwrap_or_else(|| PathBuf::from(namespace.identifier()))
        } else {
            start_token.file_origin.as_ref().to_path_buf()
        };

        let file_contents = std::fs::read_to_string(source_path.as_path()).map_err(|_| {
            CXStdErrorMsg::error(
                "ANALYSIS ERROR",
                format!(
                    "Failed to read source file for analysis diagnostics: {}",
                    source_path.display()
                ),
            )
        })?;

        let source_slice = file_contents
            .get(start_token.byte_start_index..end_token.byte_end_index)
            .ok_or(CXStdErrorMsg::error(
                "ANALYSIS ERROR",
                format!(
                    "Invalid source range: token indices {start_token} to {end_token} out of bounds in file {}",
                    source_path.display()
                ),
            ))?
            .trim();
        Ok(source_slice.to_string())
    }

    fn failure_message(&self, message: &str, condition: &FMIRNode) -> String {
        if let Some(ret_name) = message.strip_prefix("postcondition failed:") {
            let post_condition_expr = self
                .source_text_for_range(&condition.token_range)
                .unwrap_or_else(|_| "<unknown>".to_string());
            return format!(
                "In function `{}`, contract condition\n   post({}): ({})\nwill never be true at return site",
                self.function_name, ret_name, post_condition_expr
            );
        }

        format!(
            "FMIR analysis error in safe function '{}': {} (condition proven false)",
            self.function_name, message
        )
    }

    fn fail_proven_false(
        &self,
        message: &str,
        node: &FMIRNode,
        condition: &FMIRNode,
    ) -> CXResult<VisitControl> {
        let resolved_message = self.failure_message(message, condition);

        log_analysis_error!(self, node, "{}", resolved_message)
    }
}

impl<'a> FMIRContext<'a> {
    pub fn new(
        current_namespace: EnvironmentNamespace,
        module_data: &'a ModuleData,
        registry: &'a MIRDecomposedRegistry,
    ) -> Self {
        FMIRContext {
            env: FMIREnvironment::new(current_namespace, module_data, registry),
            functions: HashMap::new(),
        }
    }

    pub fn new_from(mir: &'a MIRUnit, module_data: &'a ModuleData) -> CXResult<Self> {
        let mut context =
            FMIRContext::new(mir.source_namespace.clone(), module_data, &mir.registry);

        for function in mir.functions.iter() {
            if !function.prototype.signature().contract.safe {
                continue;
            }

            context.consume_mir_function(function)?;
        }

        Ok(context)
    }

    pub fn consume_mir_function(&mut self, mir_function: &MIRFunction) -> CXResult<()> {
        let fmir_function = convert_mir(&mut self.env, mir_function)?;

        self.functions
            .insert(mir_function.prototype.name().to_owned(), fmir_function);

        Ok(())
    }

    pub fn apply_analysis_pass(&mut self, pass: FMIRAnalysisPass) -> CXResult<()> {
        let mut next_functions = HashMap::with_capacity(self.functions.len());
        for (name, function) in self.functions.iter() {
            next_functions.insert(name.clone(), pass(self, function.clone())?);
        }
        self.functions = next_functions;
        Ok(())
    }

    pub fn apply_standard_analysis_passes(&mut self) -> CXResult<()> {
        for function in self.functions.values() {
            assert_proven_conditions(
                &function.prototype,
                &function.body,
                self.env.current_namespace.clone(),
                self.env.module_data,
            )?;
        }

        Ok(())
    }

    pub fn drain_functions(&mut self) -> Vec<(String, FMIRFunction)> {
        self.functions.drain().collect()
    }
}

impl Display for FMIRContext<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut names = self.functions.keys().cloned().collect::<Vec<_>>();
        names.sort();

        writeln!(f, "FMIR Context:")?;
        for name in names {
            if let Some(function) = self.functions.get(&name) {
                writeln!(f, "{}", function.display_with(self.env.type_definitions))?;
            }
        }

        Ok(())
    }
}
