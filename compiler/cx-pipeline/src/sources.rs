use crate::log::pipeline_error;
use cx_log::{CXResult, catalogue::driver as catalogue};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

pub(crate) fn expand_patterns(base: &Path, patterns: &[String]) -> CXResult<Vec<PathBuf>> {
    let mut selected = BTreeSet::new();

    for raw_pattern in patterns {
        let (exclude, raw_pattern) = match raw_pattern.strip_prefix('!') {
            Some(pattern) => (true, pattern),
            None => (false, raw_pattern.as_str()),
        };
        let pattern = normalize_pattern(raw_pattern);
        if pattern.is_empty() {
            return Err(pipeline_error(&catalogue::EMPTY_MATCH_PATTERN, ()));
        }
        if Path::new(pattern.as_str()).is_absolute() {
            return Err(pipeline_error(&catalogue::ABSOLUTE_MATCH_PATTERN, pattern));
        }

        let root = search_root(base, pattern.as_str());
        if root.exists() {
            let mut pattern_matches = BTreeSet::new();
            collect_matches(base, &root, pattern.as_str(), &mut pattern_matches)?;
            if exclude {
                for path in pattern_matches {
                    selected.remove(&path);
                }
            } else {
                selected.extend(pattern_matches);
            }
        }
    }

    if selected.is_empty() {
        return Err(pipeline_error(
            &catalogue::NO_MATCHED_SOURCES,
            patterns.join(", "),
        ));
    }

    Ok(selected.into_iter().collect())
}

pub(crate) fn prepend_entry(sources: &mut Vec<PathBuf>, entry: Option<&str>) {
    let Some(entry) = entry else {
        return;
    };
    let entry = PathBuf::from(normalize_pattern(entry));
    sources.retain(|source| source != &entry);
    sources.insert(0, entry);
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
) -> CXResult<()> {
    if current.is_file() {
        add_match(base, current, pattern, matches)?;
        return Ok(());
    }

    let entries = fs::read_dir(current).map_err(|error| {
        pipeline_error(
            &catalogue::READ_MATCH_DIRECTORY,
            (current.display().to_string(), error.to_string()),
        )
    })?;
    for entry in entries {
        let entry = entry
            .map_err(|error| pipeline_error(&catalogue::READ_MATCH_ENTRY, error.to_string()))?;
        let path = entry.path();
        if path.is_dir() {
            collect_matches(base, &path, pattern, matches)?;
        } else {
            add_match(base, &path, pattern, matches)?;
        }
    }

    Ok(())
}

fn add_match(
    base: &Path,
    path: &Path,
    pattern: &str,
    matches: &mut BTreeSet<PathBuf>,
) -> CXResult<()> {
    let relative = path.strip_prefix(base).map_err(|error| {
        pipeline_error(
            &catalogue::RELATIVE_MATCH_PATH,
            (
                path.display().to_string(),
                base.display().to_string(),
                error.to_string(),
            ),
        )
    })?;
    let relative = relative.to_string_lossy().replace('\\', "/");
    if wildcard_match(pattern, relative.as_str()) {
        matches.insert(relative.into());
    }
    Ok(())
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
