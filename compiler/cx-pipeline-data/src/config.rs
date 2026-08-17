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
    pub compile_all: Option<CompileAllConfig>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct CompileAllConfig {
    #[serde(rename = "match")]
    pub matches: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
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
    use super::BinaryEntry;

    #[test]
    fn parses_structured_compile_all() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
compile_all = { match = ["src/*.c"], exclude = ["src/generated.c"] }
"#,
        )
        .unwrap();

        assert_eq!(binary.entry, None);
        let compile_all = binary.compile_all.unwrap();
        assert_eq!(compile_all.matches, vec!["src/*.c"]);
        assert_eq!(compile_all.exclude, vec!["src/generated.c"]);
    }

    #[test]
    fn parses_compile_all_without_excludes() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
compile_all = { match = ["src/*.c", "platform.cx"] }
"#,
        )
        .unwrap();

        assert_eq!(binary.entry, None);
        let compile_all = binary.compile_all.unwrap();
        assert_eq!(compile_all.matches, vec!["src/*.c", "platform.cx"]);
        assert!(compile_all.exclude.is_empty());
    }

    #[test]
    fn allows_entry_with_compile_all() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
entry = "src/main.cx"
compile_all = { match = ["src/*.c"] }
"#,
        )
        .unwrap();

        assert_eq!(binary.entry.as_deref(), Some("src/main.cx"));
        assert_eq!(binary.compile_all.unwrap().matches, vec!["src/*.c"]);
    }
}
