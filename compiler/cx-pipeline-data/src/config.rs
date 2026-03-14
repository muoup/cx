use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Deserialize)]
pub struct CXProjectConfig {
    pub project: ProjectSection,
    pub build: Option<BuildSection>,
    pub workspace: Option<WorkspaceSection>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ProjectSection {
    pub name: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct BuildSection {
    pub backend: Option<String>,
    pub optimization: Option<String>,
    pub analysis: Option<bool>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct WorkspaceSection {
    pub targets: HashMap<String, TargetConfig>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct TargetConfig {
    pub binaries: Option<Vec<BinaryEntry>>,
    pub libraries: Option<Vec<LibraryEntry>>,
    pub link: Option<Vec<LinkEntry>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct BinaryEntry {
    pub name: String,
    pub entry: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LibraryEntry {
    pub name: String,
    pub entry: String,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LinkEntry {
    pub name: String,
    pub kind: String,
}

/// Search upward from `start_dir` for a `cx.toml` file.
/// Returns the path to the directory containing `cx.toml` and the parsed config.
pub fn find_and_load_config(start_dir: &Path) -> Option<(PathBuf, CXProjectConfig)> {
    let mut current = start_dir.to_path_buf();
    loop {
        let config_path = current.join("cx.toml");
        if config_path.is_file() {
            let content = std::fs::read_to_string(&config_path).ok()?;
            let config: CXProjectConfig = toml::from_str(&content).ok()?;
            return Some((current, config));
        }
        if !current.pop() {
            return None;
        }
    }
}

/// Load a config from a specific path.
pub fn load_config(path: &Path) -> Result<CXProjectConfig, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
    toml::from_str(&content)
        .map_err(|e| format!("Failed to parse {}: {}", path.display(), e))
}
