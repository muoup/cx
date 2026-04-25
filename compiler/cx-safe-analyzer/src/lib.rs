use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

use cx_mir::mir::data::MIRFunctionPrototype;
use cx_mir::mir::program::{MIRFunction, MIRUnit};
use cx_safe_ir::ast::{FMIRFunction, FMIRNode};
use cx_tokens::TokenRange;
use cx_util::{CXError, CXResult};

use crate::mir_conversion::{convert_mir, environment::FMIREnvironment};
use crate::simplify::assert_proven_conditions;
use crate::traversal::VisitControl;

pub(crate) mod log;
pub(crate) mod mir_conversion;
pub(crate) mod simplify;
pub(crate) mod traversal;

pub type FMIRAnalysisPass<'a> = &'a dyn Fn(&FMIRContext, FMIRFunction) -> CXResult<FMIRFunction>;

pub struct FMIRContext {
    env: FMIREnvironment,
    functions: HashMap<String, FMIRFunction>,
}

struct AnalysisDiagnosticContext {
    compilation_unit: PathBuf,
    file_contents: Option<String>,
    function_name: String,
}

impl AnalysisDiagnosticContext {
    fn new(function_prototype: &MIRFunctionPrototype, compilation_unit: &Path) -> Self {
        Self {
            compilation_unit: compilation_unit.to_path_buf(),
            file_contents: std::fs::read_to_string(compilation_unit).ok(),
            function_name: function_prototype.name.as_string(),
        }
    }

    fn source_text_for_range(&self, range: &TokenRange) -> CXResult<String> {
        let source_path = if range.file_origin.is_empty() {
            self.compilation_unit.as_path()
        } else {
            Path::new(range.file_origin.as_ref())
        };
        let owned_file_contents;
        let file_contents = if source_path == self.compilation_unit.as_path() {
            self.file_contents.as_ref().ok_or_else(|| {
                CXError::create_boxed(format!(
                    "Failed to read source file for analysis diagnostics: {}",
                    self.compilation_unit.display()
                ))
            })?
        } else {
            owned_file_contents = std::fs::read_to_string(source_path).map_err(|_| {
                CXError::create_boxed(format!(
                    "Failed to read source file for analysis diagnostics: {}",
                    source_path.display()
                ))
            })?;
            &owned_file_contents
        };
        let tokens = cx_lexer::lex(file_contents).map_err(|_| {
            CXError::create_boxed(format!(
                "Failed to lex source file for analysis diagnostics: {}",
                source_path.display()
            ))
        })?;

        let start_token = tokens.get(range.start_token).ok_or_else(|| {
            CXError::create_boxed(format!(
                "Invalid source range: start token index {} out of bounds",
                range.start_token
            ))
        })?;
        let end_token = tokens
            .get(range.end_token.saturating_sub(1))
            .ok_or_else(|| {
                CXError::create_boxed(format!(
                    "Invalid source range: end token index {} out of bounds",
                    range.end_token
                ))
            })?;
        if start_token.file_origin != end_token.file_origin {
            return CXError::create_result(format!(
                "Source range tokens have different file origins: {} and {}",
                start_token.file_origin, end_token.file_origin
            ));
        }

        let source_slice = file_contents
            .get(start_token.byte_start_index..end_token.byte_end_index)
            .ok_or(CXError::create_boxed(format!(
                "Invalid source range: token indices {} to {} out of bounds in file {}",
                range.start_token,
                range.end_token,
                self.compilation_unit.display()
            )))?
            .trim();
        Ok(source_slice.to_string())
    }

    fn failure_message(&self, message: &str, condition: &FMIRNode) -> CXResult<String> {
        if let Some(ret_name) = message.strip_prefix("postcondition failed:") {
            let post_condition_expr = condition
                .token_range
                .as_ref()
                .ok_or_else(|| {
                    CXError::create_boxed("Condition node has no source range for diagnostics")
                })
                .and_then(|range| self.source_text_for_range(range))?;
            return Ok(format!(
                "In function `{}`, contract condition\n   post({}): ({})\nwill never be true at return site",
                self.function_name, ret_name, post_condition_expr
            ));
        }

        Ok(format!(
            "FMIR analysis error in safe function '{}': {} (condition proven false)",
            self.function_name, message
        ))
    }

    fn fail_proven_false(
        &self,
        message: &str,
        node: &FMIRNode,
        condition: &FMIRNode,
    ) -> CXResult<VisitControl> {
        let resolved_message = self.failure_message(message, condition)?;
        log_analysis_error!(self, node, "{}", resolved_message)
    }
}

impl FMIRContext {
    pub fn new(compilation_unit: PathBuf) -> Self {
        FMIRContext {
            env: FMIREnvironment::new(compilation_unit, Default::default()),
            functions: HashMap::new(),
        }
    }

    pub fn new_from(mir: &MIRUnit) -> CXResult<Self> {
        let mut context = FMIRContext {
            env: FMIREnvironment::new(mir.source_path.to_owned(), mir.type_definitions.clone()),
            functions: HashMap::new(),
        };

        for function in mir.functions.iter() {
            if !function.prototype.contract.safe {
                continue;
            }

            context.consume_mir_function(function)?;
        }

        Ok(context)
    }

    pub fn consume_mir_function(&mut self, mir_function: &MIRFunction) -> CXResult<()> {
        let fmir_function = convert_mir(&mut self.env, mir_function)?;

        self.functions
            .insert(mir_function.prototype.name.as_string(), fmir_function);

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

    pub fn apply_standard_analysis_passes(&mut self, compilation_unit: &Path) -> CXResult<()> {
        for function in self.functions.values() {
            assert_proven_conditions(&function.prototype, &function.body, compilation_unit)?;
        }

        Ok(())
    }

    pub fn drain_functions(&mut self) -> Vec<(String, FMIRFunction)> {
        self.functions.drain().collect()
    }
}

impl Display for FMIRContext {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut names = self.functions.keys().cloned().collect::<Vec<_>>();
        names.sort();

        writeln!(f, "FMIR Context:")?;
        for name in names {
            if let Some(function) = self.functions.get(&name) {
                writeln!(f, "{}", function.display_with(&self.env.type_definitions))?;
            }
        }

        Ok(())
    }
}
