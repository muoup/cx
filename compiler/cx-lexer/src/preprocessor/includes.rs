use std::path::PathBuf;

#[cfg(not(feature = "ignore-system-headers"))]
use std::sync::OnceLock;

use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_util::module_path::cx_library_directory;

use crate::{
    context::{LexingContext, SourceInput},
    lexer::{scanner::LexTransition, source::LanguageMode},
    preprocessor::{conditionals::rest_of_logical_directive, includes},
};

pub(crate) fn handle_include(
    context: &mut LexingContext,
    directive_start: usize,
    _directive_end: usize,
) -> CXResult<LexTransition> {
    if !context.current_frame().is_active() {
        context.skip_tail();
        return Ok(LexTransition::Continue);
    }

    context.current_frame_mut().skip_whitespace();

    let file_name_start = context.current_frame().cursor;
    let Some(file_name) = context.current_frame_mut().next_word() else {
        let frame = context.current_frame();

        return frame
            .cursor_view()
            .log_error(directive_start, "#include requires a file path");
    };
    let _file_name_end = context.current_frame().cursor;

    if !(file_name.starts_with('"') && file_name.ends_with('"'))
        && !(file_name.starts_with('<') && file_name.ends_with('>'))
    {
        let frame = context.current_frame();
        return frame.cursor_view().log_error(
            file_name_start,
            format!(
                "Invalid include path '{}': expected \"...\" or <...>",
                file_name
            ),
        );
    }

    let current_file = context.current_frame().file_path.clone();
    let path = match includes::resolve_path(&current_file, &context.include_dirs, &file_name) {
        Some(path) => path,
        None => {
            let frame = context.current_frame();
            return frame.cursor_view().log_error(
                file_name_start,
                format!("Included file not found: {file_name}"),
            );
        }
    };

    let canonical_path = path.canonicalize().unwrap_or(path.clone());
    if context.once_files.contains(&canonical_path) {
        return Ok(LexTransition::Continue);
    }

    let source = std::fs::read_to_string(path.as_path()).map_err(|e| {
        CXErr::new(
            CXStdErrMessage::error(
                "LEXER ERROR",
                format!("Failed to read included file {}: {}", path.display(), e),
            ),
            CXInternalContext::error("failed to read included source file"),
        )
    })?;

    let parent_mode = context.current_frame().language_mode;
    let language_mode =
        LanguageMode::for_include_path(&path, parent_mode, file_name.starts_with('<'));

    Ok(LexTransition::PushSource(SourceInput {
        source,
        path,
        language_mode,
    }))
}

pub(crate) fn handle_pragma(context: &mut LexingContext) -> CXResult<LexTransition> {
    if !context.current_frame().is_active() {
        context.skip_tail();
        return Ok(LexTransition::Continue);
    }

    let pragma = rest_of_logical_directive(context.current_frame_mut())
        .trim()
        .to_string();
    if pragma == "once" {
        let frame = context.current_frame();
        let canonical_path = frame
            .file_path
            .canonicalize()
            .unwrap_or_else(|_| frame.file_path.clone());
        context.once_files.insert(canonical_path);
    }
    Ok(LexTransition::Continue)
}

pub(crate) fn resolve_path(
    current_file: &std::path::Path,
    include_dirs: &[PathBuf],
    file_name: &str,
) -> Option<PathBuf> {
    let is_quoted = file_name.starts_with('"') && file_name.ends_with('"');
    let is_angled = file_name.starts_with('<') && file_name.ends_with('>');

    if !is_quoted && !is_angled {
        return None;
    }

    let inner = &file_name[1..file_name.len() - 1];
    let mut candidates = Vec::new();

    if is_quoted && let Some(parent) = current_file.parent() {
        candidates.push(parent.join(inner));
    }

    let bundled = PathBuf::from(cx_library_directory(&format!("libc/{inner}")));

    let search = candidates
        .into_iter()
        .chain(include_dirs.iter().map(|dir| dir.join(inner)))
        .collect::<Vec<_>>();

    #[cfg(not(feature = "ignore-system-headers"))]
    let search = search
        .into_iter()
        .chain(system_include_dirs().iter().map(|dir| dir.join(inner)));

    #[cfg(feature = "ignore-system-headers")]
    let search = search.into_iter();

    search
        .chain(std::iter::once(bundled))
        .find(|path| path.is_file())
}

#[cfg(not(feature = "ignore-system-headers"))]
fn system_include_dirs() -> &'static [PathBuf] {
    static SYSTEM_INCLUDE_DIRS: OnceLock<Vec<PathBuf>> = OnceLock::new();

    SYSTEM_INCLUDE_DIRS.get_or_init(discover_system_include_dirs)
}

#[cfg(all(unix, not(feature = "ignore-system-headers")))]
fn discover_system_include_dirs() -> Vec<PathBuf> {
    let mut dirs = vec![PathBuf::from("/usr/include")];
    dirs.extend(multiarch_include_dirs());
    dirs.extend(gcc_include_dirs());
    dirs
}

#[cfg(all(not(unix), not(feature = "ignore-system-headers")))]
fn discover_system_include_dirs() -> Vec<PathBuf> {
    vec![]
}

#[cfg(all(unix, not(feature = "ignore-system-headers")))]
fn multiarch_include_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    if let Ok(output) = std::process::Command::new("gcc")
        .arg("-print-multiarch")
        .output()
        && output.status.success()
        && let Ok(tuple) = String::from_utf8(output.stdout)
    {
        let include_dir = PathBuf::from("/usr/include").join(tuple.trim());
        if include_dir.is_dir() {
            dirs.push(include_dir);
        }
    }

    dirs
}

#[cfg(all(unix, not(feature = "ignore-system-headers")))]
fn gcc_include_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let Ok(targets) = std::fs::read_dir("/usr/lib/gcc") else {
        return dirs;
    };

    for target in targets.flatten() {
        let Ok(versions) = std::fs::read_dir(target.path()) else {
            continue;
        };

        for version in versions.flatten() {
            let include_dir = version.path().join("include");
            if include_dir.is_dir() {
                dirs.push(include_dir);
            }
        }
    }

    dirs
}
