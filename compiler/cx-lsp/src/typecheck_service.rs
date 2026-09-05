//! Typecheck service for LSP integration
//!
//! This module provides utilities for converting compiler type errors
//! into LSP diagnostics format.

use crate::position::{byte_range, line_range};
use cx_pipeline::LSPErrors;
use cx_pipeline_data::config::CXProjectConfig;
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, GlobalCompilationContext,
    OptimizationLevel,
};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Range, Url,
};

pub struct CheckReport {
    pub diagnostics: HashMap<Url, Vec<Diagnostic>>,
    pub checked_files: HashSet<Url>,
}

pub struct ProjectSettings {
    config: Option<CXProjectConfig>,
    include_dirs: Vec<PathBuf>,
}

fn load_project_settings(project_root: &Path) -> Result<ProjectSettings, String> {
    let config_path = project_root.join("cx.toml");
    if !config_path.is_file() {
        return Ok(ProjectSettings {
            config: None,
            include_dirs: vec![],
        });
    }

    let project_config = cx_pipeline_data::config::load_config(&config_path)?;
    let mut include_dirs = Vec::new();

    if let Some(workspace) = &project_config.workspace {
        for target in workspace.targets.values() {
            for include_dir in target.include_dirs.iter().flatten() {
                let include_dir = PathBuf::from(include_dir);
                let include_dir = if include_dir.is_absolute() {
                    include_dir
                } else {
                    project_root.join(include_dir)
                };

                if !include_dirs.contains(&include_dir) {
                    include_dirs.push(include_dir);
                }
            }
        }
    }

    Ok(ProjectSettings {
        config: Some(project_config),
        include_dirs,
    })
}

pub fn typecheck_file(file_path: &Path, project_root: &Path) -> Result<CheckReport, String> {
    let unit_identifier = file_path
        .strip_prefix(project_root)
        .unwrap_or(file_path)
        .to_string_lossy()
        .to_string();

    let unit = cx_pipeline_data::CompilationUnit::from_rooted(&unit_identifier, project_root);
    let internal_directory = project_root.join(".internal").join("cx-lsp");
    let ProjectSettings {
        config,
        include_dirs,
    } = load_project_settings(project_root)?;

    let context = GlobalCompilationContext {
        config: CompilerConfig {
            architecture: ArchitectureConfig::native(),
            backend: CompilerBackend::Cranelift,
            optimization_level: OptimizationLevel::O0,
            require_explicit_return: None,
            output: project_root.join("cx-lsp-output"),
            working_directory: project_root.to_path_buf(),

            module_mode: true,
            unsafe_mode: false,
            verbose: false,
            dump: false,

            project_config: config,
            include_dirs,
            internal_directory,
            compilation_mode: CompilationMode::Executable,

            link_entries: vec![],
            native_objects: vec![],
            predefined_macros: vec![],
        },
        module_db: cx_pipeline_data::db::ModuleData::new(),
        linking_files: Mutex::new(HashSet::new()),
    };

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
                message: message.clone(),
                related_information,
                source: Some("cx".to_string()),
                ..Default::default()
            }
        }
        LSPErrors::FatalError { message, line, .. } => Diagnostic {
            range: line_range(file_contents, *line),
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
pub fn group_diagnostics_by_file(errors: &[LSPErrors]) -> HashMap<Url, Vec<Diagnostic>> {
    let mut grouped = HashMap::new();
    let mut source_cache = HashMap::<PathBuf, String>::new();

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
