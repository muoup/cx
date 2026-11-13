use crate::compilation::{timed_compilation, CompilationFailure, CompilationResult, TestTempDir};
use cx_pipeline::compile;
use cx_pipeline::project::{native_inputs, target_sources, ProjectPackages};
use cx_pipeline_data::config::{
    load_project, IncludeSpecSource, LinkEntry, ProfileConfig, ProjectConfig, DEBUG_PROFILE,
};
use cx_pipeline_data::package::PackageId;
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel,
    UnsafeFlags,
};
use rustc_hash::FxHashSet;
use std::path::{Path, PathBuf};

const PROFILE: &str = "release";

/// One target of a `cx.lua` project, built with its release profile (or the debug profile when
/// it has none) outside the project's own `.internal` directory.
pub struct ProjectBinary {
    pub root: PathBuf,
    /// Relative to `root`.
    pub sources: Vec<PathBuf>,
    /// The target package's unaliased include directories.
    pub include_dirs: Vec<PathBuf>,
    pub predefined_macros: Vec<(String, String)>,
    pub native_objects: Vec<PathBuf>,
    pub link_entries: Vec<LinkEntry>,
    pub optimization_level: OptimizationLevel,
    profile: ProfileConfig,
    packages: ProjectPackages,
    package: PackageId,
}

impl ProjectBinary {
    pub fn load(root: &Path, name: &str) -> Result<Self, String> {
        let project = load_project(root).map_err(|error| error.message())?;
        let target = project
            .target(name)
            .ok_or_else(|| format!("{} has no target named '{name}'", root.display()))?;
        let profile = project
            .profile(PROFILE)
            .or_else(|| project.profile(DEBUG_PROFILE))
            .expect("every project has the debug profile")
            .clone();
        let sources = target_sources(&project, target).map_err(|error| error.message())?;
        let packages =
            ProjectPackages::new(&project, &profile.settings).map_err(|error| error.message())?;
        let package = packages.id(target.package);
        let (native_objects, link_entries) = native_inputs(&project, &all_packages(&project));

        Ok(Self {
            root: root.to_path_buf(),
            sources: sources
                .iter()
                .map(|source| source.strip_prefix(root).unwrap_or(source).to_path_buf())
                .collect(),
            include_dirs: include_dirs(&project, target.package),
            predefined_macros: packages.packages.get(package).defines.clone(),
            native_objects,
            link_entries,
            optimization_level: profile.settings.optimization.unwrap_or_default(),
            profile,
            packages,
            package,
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
            require_explicit_return: self.profile.settings.require_explicit_return,
            output: temp_dir.path().join("case.out"),
            unsafe_flags: UnsafeFlags::default(),
            compilation_mode,
            verbose: false,
            dump: false,
            internal_directory: temp_dir.internal_directory(),
            packages: self.packages.packages.clone(),
            module_mode: true,
            record_types: false,
            link_entries: self.link_entries.clone(),
            native_objects: self.native_objects.clone(),
            header_modules: FxHashSet::default(),
        };
        let sources = self
            .sources
            .iter()
            .map(|source| self.root.join(source))
            .collect::<Vec<_>>();

        timed_compilation(config, |config| compile(config, self.package, &sources).map(drop))
    }
}

fn all_packages(project: &ProjectConfig) -> Vec<usize> {
    (0..project.packages.len()).collect()
}

fn include_dirs(project: &ProjectConfig, package: usize) -> Vec<PathBuf> {
    project.packages[package]
        .usage
        .includes
        .iter()
        .filter_map(|spec| match (&spec.source, &spec.alias) {
            (IncludeSpecSource::Directory(directory), None) => Some(directory.clone()),
            _ => None,
        })
        .collect()
}
