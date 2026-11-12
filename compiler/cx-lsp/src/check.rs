use crate::diagnostics;
use crate::index::{self, FileIndex};
use crate::position::SourceCache;
use crate::project::ProjectSettings;
use cx_namespace::module::ModulePath;
use cx_pipeline::pipeline::{FailurePolicy, Pipeline};
use cx_pipeline_data::jobs::CompilationStep;
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilationUnit, CompilerBackend, CompilerConfig,
    GlobalCompilationContext, OptimizationLevel, UnsafeFlags,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::Path;
use tower_lsp::lsp_types::{Diagnostic, Url};

pub struct CheckReport {
    pub diagnostics: FxHashMap<Url, Vec<Diagnostic>>,
    pub checked_files: FxHashSet<Url>,
    pub index: FxHashMap<Url, FileIndex>,
}

pub fn check(file_path: &Path, project: &ProjectSettings) -> CheckReport {
    let context = GlobalCompilationContext::new(CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend: CompilerBackend::Cranelift,
        optimization_level: OptimizationLevel::O0,
        require_explicit_return: None,
        output: project.root.join("cx-lsp-output"),
        source_root: project.source_root.clone(),

        module_mode: true,
        unsafe_flags: UnsafeFlags::default(),
        verbose: false,
        dump: false,

        include_dirs: project.include_dirs.clone(),
        internal_directory: project.root.join(".internal").join("cx-lsp"),
        compilation_mode: CompilationMode::Executable,

        link_entries: vec![],
        native_objects: vec![],
        predefined_macros: project.predefined_macros.clone(),
        external_modules: Default::default(),
        header_modules: Default::default(),
    });
    let unit = CompilationUnit::new(
        &project.source_root,
        ModulePath::new(file_path.to_path_buf()),
        None,
    );

    let mut checked_files = FxHashSet::default();
    let failures = Pipeline::new(&context, CompilationStep::MIR)
        .roots([unit])
        .policy(FailurePolicy::Continue)
        .run(|job| {
            if let Ok(uri) = Url::from_file_path(job.unit.module().as_path()) {
                checked_files.insert(uri);
            }
        })
        .err()
        .unwrap_or_default();

    let mut sources = SourceCache::default();
    CheckReport {
        diagnostics: diagnostics::from_failures(&failures, &mut sources),
        checked_files,
        index: index::build::from_context(&context, &mut sources),
    }
}
