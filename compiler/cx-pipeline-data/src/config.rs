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
    pub require_explicit_return: Option<bool>,
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
    pub native_objects: Option<Vec<String>>,
    pub include_dirs: Option<Vec<String>>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct BinaryEntry {
    pub name: String,
    pub entry: Option<String>,
    #[serde(rename = "match")]
    pub match_patterns: Option<Vec<String>>,
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
pub fn find_and_load_config(
    start_dir: &Path,
) -> Result<Option<(PathBuf, CXProjectConfig)>, String> {
    let mut current = start_dir.to_path_buf();
    loop {
        let config_path = current.join("cx.toml");
        if config_path.is_file() {
            let content = std::fs::read_to_string(&config_path)
                .map_err(|error| format!("Failed to read {}: {error}", config_path.display()))?;
            let config: CXProjectConfig = toml::from_str(&content)
                .map_err(|error| format!("Failed to parse {}: {error}", config_path.display()))?;
            return Ok(Some((current, config)));
        }
        if !current.pop() {
            return Ok(None);
        }
    }
}

/// Load a config from a specific path.
pub fn load_config(path: &Path) -> Result<CXProjectConfig, String> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read {}: {}", path.display(), e))?;
    toml::from_str(&content).map_err(|e| format!("Failed to parse {}: {}", path.display(), e))
}

#[cfg(test)]
mod tests {
    use super::{BinaryEntry, CXProjectConfig};

    #[test]
    fn parses_binary_match_patterns() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
match = ["src/*.c", "!src/generated.c"]
"#,
        )
        .unwrap();

        assert_eq!(binary.entry, None);
        assert_eq!(
            binary.match_patterns,
            Some(vec!["src/*.c".to_string(), "!src/generated.c".to_string()])
        );
    }

    #[test]
    fn parses_binary_match_patterns_without_entry() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
match = ["src/*.c", "platform.cx"]
"#,
        )
        .unwrap();

        assert_eq!(binary.entry, None);
        assert_eq!(
            binary.match_patterns,
            Some(vec!["src/*.c".to_string(), "platform.cx".to_string()])
        );
    }

    #[test]
    fn allows_entry_with_match_patterns() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
entry = "src/main.cx"
match = ["src/*.c"]
"#,
        )
        .unwrap();

        assert_eq!(binary.entry.as_deref(), Some("src/main.cx"));
        assert_eq!(binary.match_patterns, Some(vec!["src/*.c".to_string()]));
    }

    #[test]
    fn keeps_match_patterns_scoped_to_each_binary() {
        let config: CXProjectConfig = toml::from_str(
            r#"
[project]
name = "demo"

[workspace.targets.default]

[[workspace.targets.default.binaries]]
name = "c_binary"
match = ["src/*.c"]

[[workspace.targets.default.binaries]]
name = "cx_binary"
entry = "src/main.cx"
"#,
        )
        .unwrap();

        let workspace = config.workspace.unwrap();
        let binaries = workspace
            .targets
            .get("default")
            .unwrap()
            .binaries
            .as_ref()
            .unwrap();
        assert_eq!(binaries.len(), 2);
        assert_eq!(
            binaries[0].match_patterns,
            Some(vec!["src/*.c".to_string()])
        );
        assert_eq!(binaries[1].entry.as_deref(), Some("src/main.cx"));
        assert_eq!(binaries[1].match_patterns, None);
    }
}
