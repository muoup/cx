use serde::{Deserialize, Deserializer};
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
    #[serde(default, deserialize_with = "deserialize_compile_all")]
    pub compile_all: Option<Vec<String>>,
    #[serde(default, deserialize_with = "deserialize_compile_all")]
    pub exclude: Option<Vec<String>>,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum CompileAllValue {
    Single(String),
    Multiple(Vec<String>),
}

fn deserialize_compile_all<'de, D>(deserializer: D) -> Result<Option<Vec<String>>, D::Error>
where
    D: Deserializer<'de>,
{
    Option::<CompileAllValue>::deserialize(deserializer).map(|value| {
        value.map(|value| match value {
            CompileAllValue::Single(pattern) => vec![pattern],
            CompileAllValue::Multiple(patterns) => patterns,
        })
    })
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
    fn parses_single_compile_all_pattern() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
compile_all = "src/*.c"
"#,
        )
        .unwrap();

        assert_eq!(binary.entry, None);
        assert_eq!(binary.compile_all, Some(vec!["src/*.c".to_string()]));
        assert_eq!(binary.exclude, None);
    }

    #[test]
    fn parses_multiple_compile_all_patterns() {
        let binary: BinaryEntry = toml::from_str(
            r#"
name = "demo"
compile_all = ["src/*.c", "platform.cx"]
"#,
        )
        .unwrap();

        assert_eq!(binary.entry, None);
        assert_eq!(
            binary.compile_all,
            Some(vec!["src/*.c".to_string(), "platform.cx".to_string()])
        );
    }
}
