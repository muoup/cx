use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn expand_patterns(
    base: &Path,
    patterns: &[String],
    excludes: &[String],
) -> Result<Vec<PathBuf>, String> {
    let mut matches = BTreeSet::new();

    for pattern in patterns {
        let pattern = normalize_pattern(pattern);
        if Path::new(pattern.as_str()).is_absolute() {
            return Err(format!("compile_all pattern must be relative: {pattern}"));
        }

        let root = search_root(base, pattern.as_str());
        if root.exists() {
            collect_matches(base, &root, pattern.as_str(), &mut matches)?;
        }
    }

    let excludes = excludes
        .iter()
        .map(|pattern| normalize_pattern(pattern))
        .collect::<Vec<_>>();
    matches.retain(|path| {
        let path = path.to_string_lossy().replace('\\', "/");
        !excludes
            .iter()
            .any(|pattern| wildcard_match(pattern, path.as_str()))
    });

    if matches.is_empty() {
        return Err(format!(
            "compile_all patterns matched no source files: {}",
            patterns.join(", ")
        ));
    }

    Ok(matches.into_iter().collect())
}

fn normalize_pattern(pattern: &str) -> String {
    pattern
        .replace('\\', "/")
        .trim_start_matches("./")
        .to_string()
}

fn search_root(base: &Path, pattern: &str) -> PathBuf {
    let mut root = base.to_path_buf();
    let mut has_wildcard = false;

    for component in Path::new(pattern).components() {
        let component = component.as_os_str().to_string_lossy();
        if component.contains('*') || component.contains('?') {
            has_wildcard = true;
            break;
        }
        root.push(component.as_ref());
    }

    if has_wildcard {
        root
    } else {
        root.parent().unwrap_or(base).to_path_buf()
    }
}

fn collect_matches(
    base: &Path,
    current: &Path,
    pattern: &str,
    matches: &mut BTreeSet<PathBuf>,
) -> Result<(), String> {
    if current.is_file() {
        add_match(base, current, pattern, matches);
        return Ok(());
    }

    let entries = fs::read_dir(current).map_err(|error| {
        format!(
            "failed to read compile_all directory {}: {error}",
            current.display()
        )
    })?;
    for entry in entries {
        let entry = entry.map_err(|error| format!("failed to read compile_all entry: {error}"))?;
        let path = entry.path();
        if path.is_dir() {
            collect_matches(base, &path, pattern, matches)?;
        } else {
            add_match(base, &path, pattern, matches);
        }
    }

    Ok(())
}

fn add_match(base: &Path, path: &Path, pattern: &str, matches: &mut BTreeSet<PathBuf>) {
    let Ok(relative) = path.strip_prefix(base) else {
        return;
    };
    let relative = relative.to_string_lossy().replace('\\', "/");
    if wildcard_match(pattern, relative.as_str()) {
        matches.insert(relative.into());
    }
}

fn wildcard_match(pattern: &str, value: &str) -> bool {
    fn matches(pattern: &[u8], value: &[u8]) -> bool {
        match pattern.first() {
            None => value.is_empty(),
            Some(b'*') => {
                matches(&pattern[1..], value)
                    || value.first().is_some_and(|character| *character != b'/')
                        && matches(pattern, &value[1..])
            }
            Some(b'?') => {
                value.first().is_some_and(|character| *character != b'/')
                    && matches(&pattern[1..], &value[1..])
            }
            Some(character) => {
                value.first() == Some(character) && matches(&pattern[1..], &value[1..])
            }
        }
    }

    matches(pattern.as_bytes(), value.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::expand_patterns;
    use std::fs;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static NEXT_TEST_DIRECTORY: AtomicUsize = AtomicUsize::new(0);

    fn test_directory() -> PathBuf {
        let id = NEXT_TEST_DIRECTORY.fetch_add(1, Ordering::Relaxed);
        std::env::temp_dir().join(format!("cx-source-expansion-{}-{id}", std::process::id()))
    }

    #[test]
    fn expands_single_pattern() {
        let root = test_directory();
        fs::create_dir_all(root.join("src/nested")).unwrap();
        fs::write(root.join("src/main.c"), "").unwrap();
        fs::write(root.join("src/nested/other.c"), "").unwrap();
        fs::write(root.join("src/main.cx"), "").unwrap();

        let result = expand_patterns(&root, &["src/*.c".to_string()], &[]).unwrap();

        assert_eq!(result, vec![PathBuf::from("src/main.c")]);
        fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn expands_and_deduplicates_multiple_patterns() {
        let root = test_directory();
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/main.c"), "").unwrap();
        fs::write(root.join("src/other.c"), "").unwrap();

        let result = expand_patterns(
            &root,
            &["src/*.c".to_string(), "src/main.c".to_string()],
            &[],
        )
        .unwrap();

        assert_eq!(
            result,
            vec![PathBuf::from("src/main.c"), PathBuf::from("src/other.c")]
        );
        fs::remove_dir_all(root).unwrap();
    }
}
