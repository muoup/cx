use std::path::PathBuf;

use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_tokens::token::Token;

pub struct SourceContext<'a> {
    pub tokens: &'a [Token],
    pub compilation_unit: CompilationUnit,
    pub working_directory: PathBuf,
    pub module_data: &'a ModuleData,
}

impl<'a> SourceContext<'a> {
    pub fn new(
        tokens: &'a [Token],
        compilation_unit: CompilationUnit,
        working_directory: PathBuf,
        module_data: &'a ModuleData,
    ) -> Self {
        Self {
            tokens,
            compilation_unit,
            working_directory,
            module_data,
        }
    }

    pub fn resolve_compilation_unit(&self, module: &str) -> CompilationUnit {
        CompilationUnit::from_rooted(module, &self.working_directory)
    }
}
