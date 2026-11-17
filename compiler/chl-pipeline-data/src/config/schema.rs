use std::path::PathBuf;

use crate::{CompilerBackend, OptimizationLevel};

pub const DEBUG_PROFILE: &str = "debug";

#[derive(Debug, Clone)]
pub struct ProjectConfig {
    pub root: PathBuf,
    pub packages: Vec<PackageConfig>,
    pub profiles: Vec<ProfileConfig>,
    pub targets: Vec<TargetConfig>,
}

#[derive(Debug, Clone)]
pub struct PackageConfig {
    pub name: String,
    pub root: PathBuf,
    pub source_root: PathBuf,
    pub mounts: Vec<Mount>,
    pub deps: Vec<usize>,
    pub library: Option<LibraryConfig>,
    pub includes: Vec<IncludeSpec>,
    pub defines: Vec<(String, String)>,
    pub objects: Vec<PathBuf>,
    pub links: Vec<LinkEntry>,
}

#[derive(Debug, Clone, Default)]
pub struct LibraryConfig {
    pub matches: Vec<String>,
    pub excludes: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Mount {
    pub prefix: String,
    pub target: MountTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MountTarget {
    Std,
    Package(usize),
}

#[derive(Debug, Clone)]
pub struct IncludeSpec {
    pub source: IncludeSpecSource,
    pub alias: Option<String>,
}

#[derive(Debug, Clone)]
pub enum IncludeSpecSource {
    Directory(PathBuf),
    Glob { base: PathBuf, pattern: String },
}

#[derive(Debug, Clone)]
pub struct ProfileConfig {
    pub name: String,
    pub settings: ProfileSettings,
}

#[derive(Debug, Clone, Default)]
pub struct ProfileSettings {
    pub optimization: Option<OptimizationLevel>,
    pub backend: Option<CompilerBackend>,
    pub no_analysis: Option<bool>,
    pub no_assert: Option<bool>,
    pub require_explicit_return: Option<bool>,
    pub defines: Vec<(String, String)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TargetKind {
    Executable,
    FfiLib,
}

#[derive(Debug, Clone)]
pub struct TargetConfig {
    pub name: String,
    pub kind: TargetKind,
    pub package: usize,
    pub matches: Vec<String>,
    pub excludes: Vec<String>,
    pub headers: Vec<String>,
    pub ffi_includes: Vec<FfiInclude>,
}

#[derive(Debug, Clone)]
pub struct FfiInclude {
    pub target: usize,
    pub prefix: Option<String>,
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

impl ProfileSettings {
    pub fn inherit(mut self, base: &ProfileSettings) -> Self {
        self.optimization = self.optimization.or(base.optimization);
        self.backend = self.backend.or(base.backend);
        self.no_analysis = self.no_analysis.or(base.no_analysis);
        self.no_assert = self.no_assert.or(base.no_assert);
        self.require_explicit_return = self.require_explicit_return.or(base.require_explicit_return);
        self.defines = base.defines.iter().cloned().chain(self.defines).collect();
        self
    }
}

impl ProjectConfig {
    pub fn profile(&self, name: &str) -> Option<&ProfileConfig> {
        self.profiles.iter().find(|profile| profile.name == name)
    }

    pub fn target(&self, name: &str) -> Option<usize> {
        self.selectable().find(|index| self.targets[*index].name == name)
    }

    pub fn executables(&self) -> Vec<usize> {
        self.selectable()
            .filter(|index| self.targets[*index].kind == TargetKind::Executable)
            .collect()
    }

    pub fn selectable(&self) -> impl Iterator<Item = usize> + '_ {
        (0..self.targets.len()).filter(|index| self.targets[*index].package == 0)
    }

    pub fn dep_closure(&self, package: usize) -> Vec<usize> {
        let mut closure = vec![package];
        let mut index = 0;
        while index < closure.len() {
            for dep in &self.packages[closure[index]].deps {
                if !closure.contains(dep) {
                    closure.push(*dep);
                }
            }
            index += 1;
        }
        closure
    }
}
