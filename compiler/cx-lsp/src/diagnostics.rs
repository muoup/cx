use crate::position::SourceCache;
use cx_pipeline::pipeline::PipelineFailure;
use rustc_hash::{FxHashMap, FxHashSet};
use std::path::Path;
use std::sync::Mutex;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Url,
};

pub fn from_failures(
    failures: &[PipelineFailure],
    sources: &mut SourceCache,
) -> FxHashMap<Url, Vec<Diagnostic>> {
    let mut grouped = FxHashMap::<Url, Vec<Diagnostic>>::default();

    for failure in failures {
        let error = &failure.error;
        let span = error.source_span();
        let file: &Path = match &span {
            Some(span) => &span.file,
            None => failure.job.unit.module().as_path(),
        };
        let Ok(uri) = Url::from_file_path(file) else {
            continue;
        };
        let Some(source) = sources.get(file) else {
            continue;
        };
        let range = match &span {
            Some(span) => source.range(span.byte_start, span.byte_end),
            None => source.line(None),
        };
        let related_information = (!error.notes().is_empty()).then(|| {
            error
                .notes()
                .iter()
                .map(|note| DiagnosticRelatedInformation {
                    location: Location {
                        uri: uri.clone(),
                        range,
                    },
                    message: note.clone(),
                })
                .collect()
        });
        let code = error.code();

        grouped.entry(uri).or_default().push(Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: (!code.is_empty()).then_some(NumberOrString::String(code)),
            message: error.message(),
            related_information,
            source: Some("cx".to_string()),
            ..Default::default()
        });
    }

    grouped
}

pub struct Publication {
    pub uri: Url,
    pub diagnostics: Vec<Diagnostic>,
    pub version: Option<i32>,
}

#[derive(Default)]
pub struct DiagnosticStore {
    contributions: Mutex<FxHashMap<Url, FxHashMap<Url, Vec<Diagnostic>>>>,
}

impl DiagnosticStore {
    pub fn apply(
        &self,
        entry_uri: &Url,
        entry_version: Option<i32>,
        diagnostics: FxHashMap<Url, Vec<Diagnostic>>,
        checked_files: FxHashSet<Url>,
    ) -> Vec<Publication> {
        let mut contributions = self
            .contributions
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let previous = contributions.remove(entry_uri).unwrap_or_default();
        let mut authoritative_files = checked_files;
        authoritative_files.extend(previous.keys().cloned());
        authoritative_files.extend(diagnostics.keys().cloned());

        let mut affected_files = previous.keys().cloned().collect::<FxHashSet<_>>();
        affected_files.extend(diagnostics.keys().cloned());
        affected_files.insert(entry_uri.clone());

        for contribution in contributions.values_mut() {
            for file_uri in &authoritative_files {
                if contribution.remove(file_uri).is_some() {
                    affected_files.insert(file_uri.clone());
                }
            }
        }

        contributions.insert(entry_uri.clone(), diagnostics);

        affected_files
            .into_iter()
            .map(|uri| {
                let mut merged = Vec::new();
                for contribution in contributions.values() {
                    for diagnostic in contribution.get(&uri).into_iter().flatten() {
                        if !merged.contains(diagnostic) {
                            merged.push(diagnostic.clone());
                        }
                    }
                }
                let version = (uri == *entry_uri).then_some(entry_version).flatten();
                Publication {
                    uri,
                    diagnostics: merged,
                    version,
                }
            })
            .collect()
    }
}
