use crate::compilation::{timed_compilation, CompilationFailure, CompilationResult, TestTempDir};
use cx_pipeline::{binary_sources, multi_file_compilation, standard_compilation};
use cx_pipeline_data::config::{load_config, CXProjectConfig, LinkEntry};
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel,
};
use std::path::{Path, PathBuf};

/// One binary of a `cx.toml` project, resolved so that it can be built outside the project's own
/// `.internal` directory.
pub struct ProjectBinary {
    pub root: PathBuf,
    /// Relative to `root`.
    pub sources: Vec<PathBuf>,
    pub include_dirs: Vec<PathBuf>,
    pub native_objects: Vec<PathBuf>,
    pub link_entries: Vec<LinkEntry>,
    pub optimization_level: OptimizationLevel,
    /// Whether the sources are compiled as given, rather than discovered from the first of them.
    matched_sources: bool,
    config: CXProjectConfig,
}

impl ProjectBinary {
    pub fn load(root: &Path, name: &str) -> Result<Self, String> {
        let config = load_config(&root.join("cx.toml"))?;
        let (target, binary) = config
            .workspace
            .iter()
            .flat_map(|workspace| workspace.targets.values())
            .find_map(|target| {
                let binary = target.binaries.iter().flatten().find(|binary| binary.name == name)?;
                Some((target, binary))
            })
            .ok_or_else(|| format!("{} has no binary named '{name}'", root.display()))?;
        let resolve = |paths: &Option<Vec<String>>| {
            paths.iter().flatten().map(|path| root.join(path)).collect::<Vec<_>>()
        };

        Ok(Self {
            root: root.to_path_buf(),
            sources: binary_sources(root, binary).map_err(|error| error.message())?,
            include_dirs: resolve(&target.include_dirs),
            native_objects: resolve(&target.native_objects),
            link_entries: target.link.clone().unwrap_or_default(),
            optimization_level: config
                .build
                .as_ref()
                .map(|build| build.optimization_level())
                .transpose()?
                .flatten()
                .unwrap_or_default(),
            matched_sources: binary.match_patterns.is_some(),
            config,
        })
    }

    /// Builds the binary with `backend`, whatever backend the project asks for. In
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
            require_explicit_return: self
                .config
                .build
                .as_ref()
                .and_then(|build| build.require_explicit_return),
            output: temp_dir.path().join("case.out"),
            unsafe_mode: false,
            compilation_mode,
            verbose: false,
            dump: false,
            working_directory: self.root.clone(),
            internal_directory: temp_dir.internal_directory(),
            module_mode: true,
            project_config: Some(self.config.clone()),
            link_entries: self.link_entries.clone(),
            native_objects: self.native_objects.clone(),
            include_dirs: self.include_dirs.clone(),
            predefined_macros: vec![],
        };

        timed_compilation(config, |config| {
            if self.matched_sources {
                multi_file_compilation(config, &self.sources)
            } else {
                standard_compilation(config, &self.sources[0])
            }
        })
    }
}
