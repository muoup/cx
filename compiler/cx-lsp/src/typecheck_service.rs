//! Typecheck service for LSP integration
//!
//! This module provides utilities for converting compiler type errors
//! into LSP diagnostics format.

use crate::position::{byte_range, line_range};
use cx_namespace::module::ModulePath;
use cx_pipeline::LSPErrors;
use cx_pipeline::project;
use cx_pipeline_data::config::{load_project, Profile, CONFIG_FILE};
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilationUnit, CompilerBackend, CompilerConfig,
    GlobalCompilationContext, OptimizationLevel, UnsafeFlags,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::{Path, PathBuf};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Range,
    Url,
};

pub struct CheckReport {
    pub diagnostics: FxHashMap<Url, Vec<Diagnostic>>,
    pub checked_files: FxHashSet<Url>,
}

pub struct ProjectSettings {
    source_root: PathBuf,
    include_dirs: Vec<PathBuf>,
    predefined_macros: Vec<(String, String)>,
}

fn load_project_settings(project_root: &Path) -> Result<ProjectSettings, String> {
    let mut settings = ProjectSettings {
        source_root: project_root.to_path_buf(),
        include_dirs: vec![],
        predefined_macros: vec![],
    };
    if !project_root.join(CONFIG_FILE).is_file() {
        return Ok(settings);
    }

    let project = load_project(project_root).map_err(|error| error.message())?;
    let internal_directory = project_root.join(".internal");
    settings.source_root = project.source_root.clone();
    for target in &project.targets {
        let usage = project::target_usage(&project, &internal_directory, Profile::Debug, target)
            .map_err(|error| error.message())?;
        for include_dir in usage.include {
            if !settings.include_dirs.contains(&include_dir) {
                settings.include_dirs.push(include_dir);
            }
        }
        for define in usage.defines {
            if !settings.predefined_macros.iter().any(|(name, _)| *name == define.0) {
                settings.predefined_macros.push(define);
            }
        }
    }
    Ok(settings)
}

pub fn typecheck_file(file_path: &Path, project_root: &Path) -> Result<CheckReport, String> {
    let internal_directory = project_root.join(".internal").join("cx-lsp");
    let ProjectSettings {
        source_root,
        include_dirs,
        predefined_macros,
    } = load_project_settings(project_root)?;
    let unit = CompilationUnit::new(&source_root, ModulePath::new(file_path.to_path_buf()), None);

    let context = GlobalCompilationContext::new(CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend: CompilerBackend::Cranelift,
        optimization_level: OptimizationLevel::O0,
        require_explicit_return: None,
        output: project_root.join("cx-lsp-output"),
        source_root,

        module_mode: true,
        unsafe_flags: UnsafeFlags::default(),
        verbose: false,
        dump: false,

        include_dirs,
        internal_directory,
        compilation_mode: CompilationMode::Executable,

        link_entries: vec![],
        native_objects: vec![],
        predefined_macros,
        external_modules: Default::default(),
        header_modules: Default::default(),
    });

    let check_result = cx_pipeline::typecheck_only_lsp(&context, &unit);
    let checked_files = check_result
        .checked_files
        .into_iter()
        .filter_map(|path| Url::from_file_path(path).ok())
        .collect();

    Ok(CheckReport {
        diagnostics: group_diagnostics_by_file(&check_result.errors),
        checked_files,
    })
}

fn related_information(
    uri: &Url,
    range: Range,
    notes: &[String],
) -> Option<Vec<DiagnosticRelatedInformation>> {
    if notes.is_empty() {
        return None;
    }

    Some(
        notes
            .iter()
            .map(|note| DiagnosticRelatedInformation {
                location: Location {
                    uri: uri.clone(),
                    range,
                },
                message: note.clone(),
            })
            .collect(),
    )
}

/// Convert an LSPErrors to an LSP Diagnostic
///
/// This handles both spanned errors and fatal errors.
fn lsp_error_to_diagnostic(error: &LSPErrors, file_contents: &str) -> Diagnostic {
    match error {
        LSPErrors::SpannedError {
            compilation_unit,
            code,
            message,
            byte_start,
            byte_end,
            notes,
        } => {
            let uri = Url::from_file_path(compilation_unit).ok();
            let range = byte_range(file_contents, *byte_start, *byte_end);
            let related_information = uri
                .as_ref()
                .and_then(|uri| related_information(uri, range, notes));

            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: (!code.is_empty()).then(|| NumberOrString::String(code.clone())),
                message: message.clone(),
                related_information,
                source: Some("cx".to_string()),
                ..Default::default()
            }
        }
        LSPErrors::FatalError {
            code,
            message,
            line,
            ..
        } => Diagnostic {
            range: line_range(file_contents, *line),
            code: (!code.is_empty()).then(|| NumberOrString::String(code.clone())),
            severity: Some(DiagnosticSeverity::ERROR),
            message: message.clone(),
            source: Some("cx".to_string()),
            ..Default::default()
        },
    }
}

/// Group diagnostics by file for publishing
///
/// LSP requires publishing diagnostics separately for each file.
/// This function takes a list of LSPErrors and groups them by file.
pub fn group_diagnostics_by_file(errors: &[LSPErrors]) -> FxHashMap<Url, Vec<Diagnostic>> {
    let mut grouped = FxHashMap::default();
    let mut source_cache = FxHashMap::<PathBuf, String>::default();

    for error in errors {
        let compilation_unit = match error {
            LSPErrors::SpannedError {
                compilation_unit, ..
            } => compilation_unit,
            LSPErrors::FatalError {
                compilation_unit, ..
            } => compilation_unit,
        };

        let uri = match Url::from_file_path(Path::new(compilation_unit)) {
            Ok(u) => u,
            Err(_) => continue,
        };

        let file_contents = source_cache
            .entry(compilation_unit.clone())
            .or_insert_with(|| std::fs::read_to_string(compilation_unit).unwrap_or_default());

        grouped
            .entry(uri)
            .or_insert_with(Vec::new)
            .push(lsp_error_to_diagnostic(error, file_contents));
    }

    grouped
}