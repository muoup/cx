use crate::compilation::{timed_compilation, CompilationFailure, CompilationResult, TestTempDir};
use cx_pipeline::compile;
use cx_pipeline::project::{target_sources, target_usage};
use cx_pipeline_data::config::{load_project, LinkEntry, Profile, ProjectConfig};
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel,
    UnsafeFlags,
};
use rustc_hash::FxHashSet;
use std::path::{Path, PathBuf};

/// One target of a `cx.lua` project, built with its release profile outside the project's own
/// `.internal` directory.
pub struct ProjectBinary {
    pub root: PathBuf,
    /// Relative to `root`.
    pub sources: Vec<PathBuf>,
    pub include_dirs: Vec<PathBuf>,
    pub predefined_macros: Vec<(String, String)>,
    pub native_objects: Vec<PathBuf>,
    pub link_entries: Vec<LinkEntry>,
    pub optimization_level: OptimizationLevel,
    external_modules: FxHashSet<PathBuf>,
    project: ProjectConfig,
}

impl ProjectBinary {
    pub fn load(root: &Path, name: &str) -> Result<Self, String> {
        let project = load_project(root).map_err(|error| error.message())?;
        let target = project
            .target(name)
            .ok_or_else(|| format!("{} has no target named '{name}'", root.display()))?;
        let internal_directory = root.join(".internal");
        let sources = target_sources(&project, target).map_err(|error| error.message())?;
        let usage = target_usage(&project, &internal_directory, Profile::Release, target)
            .map_err(|error| error.message())?;

        Ok(Self {
            root: root.to_path_buf(),
            sources: sources
                .sources
                .iter()
                .map(|source| source.strip_prefix(root).unwrap_or(source).to_path_buf())
                .collect(),
            include_dirs: usage.include,
            predefined_macros: usage.defines,
            native_objects: usage.objects,
            link_entries: usage.link,
            optimization_level: project
                .profile(Profile::Release)
                .optimization
                .unwrap_or_default(),
            external_modules: usage.external,
            project,
        })
    }

    /// Builds the target with `backend`, whatever backend the project asks for. In
    /// [`CompilationMode::Object`] the sources are compiled and nothing is linked.
    pub fn compile(
        &self,
        backend: CompilerBackend,
        compilation_mode: CompilationMode,
        temp_dir: &TestTempDir,
    ) -> Result<CompilationResult, CompilationFailure> {
        let config = CompilerConfig {
            architecture: ArchitectureConfig::native(),
            backend,
            optimization_level: self.optimization_level,
            require_explicit_return: self.project.profile(Profile::Release).require_explicit_return,
            output: temp_dir.path().join("case.out"),
            unsafe_flags: UnsafeFlags::default(),
            compilation_mode,
            verbose: false,
            dump: false,
            source_root: self.project.source_root.clone(),
            internal_directory: temp_dir.internal_directory(),
            module_mode: true,
            record_types: false,
            link_entries: self.link_entries.clone(),
            native_objects: self.native_objects.clone(),
            include_dirs: self.include_dirs.clone(),
            predefined_macros: self.predefined_macros.clone(),
            external_modules: self.external_modules.clone(),
            header_modules: FxHashSet::default(),
        };
        let sources = self
            .sources
            .iter()
            .map(|source| self.root.join(source))
            .collect::<Vec<_>>();

        timed_compilation(config, |config| compile(config, &sources).map(drop))
    }
}
