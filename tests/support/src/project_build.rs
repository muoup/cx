use crate::compilation::{CompilationFailure, TestTempDir};
use chl_log::CHLResult;
use chl_pipeline::project::link::link_inputs;
use chl_pipeline::project::packages::{artifact_path, header_directory};
use chl_pipeline_data::config::{load_project, LinkEntry, ProjectConfig, TargetKind, DEBUG_PROFILE};
use chl_pipeline_data::package::Packages;
use chl_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, UnsafeFlags,
};
use chl_log::catalogue::driver as catalogue;
use chl_log::error::context::CHLNoSourceContext;
use chl_log::error::CHLError;
use std::path::{Path, PathBuf};
use std::time::Instant;

pub struct ProjectBuild {
    pub project: ProjectConfig,
    pub profile_directory: PathBuf,
}

pub struct FfiLibArtifact {
    pub object: PathBuf,
    pub headers: PathBuf,
    pub links: Vec<LinkEntry>,
}

/// Builds `targets` (every executable when empty) of the project at `root` into `temp_dir`.
pub fn build_project(
    root: &Path,
    targets: &[String],
    profile: Option<&str>,
    backend: CompilerBackend,
    temp_dir: &TestTempDir,
) -> Result<ProjectBuild, CompilationFailure> {
    let start = Instant::now();
    build(root, targets, profile, backend, temp_dir)
        .map_err(|error| CompilationFailure::new(&error, start.elapsed()))
}

impl ProjectBuild {
    pub fn executable(&self, target: &str) -> Option<PathBuf> {
        let index = self.project.target(target)?;
        (self.project.targets[index].kind == TargetKind::Executable)
            .then(|| artifact_path(&self.project, &self.profile_directory, index))
    }

    pub fn ffi_lib(&self, target: &str) -> Option<FfiLibArtifact> {
        let index = self.project.target(target)?;
        (self.project.targets[index].kind == TargetKind::FfiLib).then(|| FfiLibArtifact {
            object: artifact_path(&self.project, &self.profile_directory, index),
            headers: header_directory(&self.project, &self.profile_directory, index),
            links: link_inputs(&self.project, index).links,
        })
    }
}

fn build(
    root: &Path,
    targets: &[String],
    profile: Option<&str>,
    backend: CompilerBackend,
    temp_dir: &TestTempDir,
) -> CHLResult<ProjectBuild> {
    let project = load_project(root)?;
    let name = profile.unwrap_or(DEBUG_PROFILE);
    let profile = project
        .profile(name)
        .ok_or_else(|| invalid(format!("unknown profile '{name}'")))?
        .clone();
    let selected = match targets.is_empty() {
        true => project.executables(),
        false => targets
            .iter()
            .map(|target| project.target(target).ok_or_else(|| invalid(format!("unknown target '{target}'"))))
            .collect::<CHLResult<Vec<_>>>()?,
    };

    let internal_directory = temp_dir.internal_directory();
    let base = CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend,
        compilation_mode: CompilationMode::Executable,
        optimization_level: profile.settings.optimization.unwrap_or_default(),
        require_explicit_return: profile.settings.require_explicit_return,
        output: project.root.clone(),
        internal_directory: internal_directory.clone(),
        packages: Packages::new(Vec::new()),
        link_entries: vec![],
        native_objects: vec![],
        header_modules: Default::default(),
        unsafe_flags: UnsafeFlags {
            no_assert: profile.settings.no_assert.unwrap_or(false),
            no_analysis: profile.settings.no_analysis.unwrap_or(false),
        },
        verbose: false,
        dump: false,
        module_mode: true,
        record_types: false,
    };
    chl_pipeline::project::build(&base, &project, &profile, &selected)?;
    Ok(ProjectBuild { profile_directory: internal_directory.join(&profile.name), project })
}

fn invalid(message: String) -> CHLError {
    CHLError::new(
        catalogue::INVALID_CONFIG.bind(message),
        CHLNoSourceContext::error("project test could not select its build"),
    )
}
