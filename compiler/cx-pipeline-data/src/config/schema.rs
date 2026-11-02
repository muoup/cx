use std::path::PathBuf;

use crate::{CompilerBackend, OptimizationLevel};

#[derive(Debug, Clone)]
pub struct ProjectConfig {
    pub root: PathBuf,
    pub name: String,
    pub source_root: PathBuf,
    pub debug: ProfileSettings,
    pub release: ProfileSettings,
    pub targets: Vec<TargetConfig>,
    pub dependencies: Vec<DependencyConfig>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Profile {
    Debug,
    Release,
}

#[derive(Debug, Clone, Default)]
pub struct ProfileSettings {
    pub optimization: Option<OptimizationLevel>,
    pub backend: Option<CompilerBackend>,
    pub no_analysis: Option<bool>,
    pub no_assert: Option<bool>,
    pub require_explicit_return: Option<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetKind {
    Executable,
    Library,
}

#[derive(Debug, Clone)]
pub struct TargetConfig {
    pub name: String,
    pub kind: TargetKind,
    pub sources: Vec<SourcePattern>,
    pub usage: Usage,
    pub deps: Vec<DependencyRef>,
    pub default: bool,
}

#[derive(Debug, Clone)]
pub struct SourcePattern {
    pub pattern: String,
    pub exported: bool,
}

#[derive(Debug, Clone, Default)]
pub struct Usage {
    pub include: Vec<PathBuf>,
    pub defines: Vec<(String, String)>,
    pub objects: Vec<PathBuf>,
    pub link: Vec<LinkEntry>,
}

#[derive(Debug, Clone)]
pub struct DependencyConfig {
    pub usage: Usage,
    pub deps: Vec<DependencyRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DependencyRef {
    Bundle(usize),
    Target(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LinkEntry {
    pub name: String,
    pub kind: LinkKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkKind {
    System,
    Static,
    Dynamic,
}

impl Profile {
    pub fn name(self) -> &'static str {
        match self {
            Profile::Debug => "debug",
            Profile::Release => "release",
        }
    }
}

impl ProjectConfig {
    pub fn profile(&self, profile: Profile) -> &ProfileSettings {
        match profile {
            Profile::Debug => &self.debug,
            Profile::Release => &self.release,
        }
    }

    pub fn target(&self, name: &str) -> Option<&TargetConfig> {
        self.targets.iter().find(|target| target.name == name)
    }
}
