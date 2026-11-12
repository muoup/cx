pub mod build;

use cx_hmir::HMIRDeclarationRole;
use dashmap::DashMap;
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tower_lsp::lsp_types::{Location, Position, Range, Url};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolClass {
    Function,
    Global,
    Type,
    Local,
    Parameter,
}

#[derive(Debug, Clone)]
pub struct DeclarationSite {
    pub location: Location,
    pub role: HMIRDeclarationRole,
}

#[derive(Debug, Clone)]
pub struct Reference {
    pub range: Range,
    pub class: SymbolClass,
    pub targets: Vec<DeclarationSite>,
}

#[derive(Debug, Clone)]
pub struct IndexedSymbol {
    pub name: String,
    pub class: SymbolClass,
    pub range: Range,
    pub selection: Range,
}

#[derive(Debug, Default)]
pub struct FileIndex {
    pub symbols: Vec<IndexedSymbol>,
    pub references: Vec<Reference>,
}

impl FileIndex {
    pub fn reference_at(&self, position: Position) -> Option<&Reference> {
        self.references
            .iter()
            .filter(|reference| reference.range.start <= position && position <= reference.range.end)
            .min_by_key(|reference| {
                let Range { start, end } = reference.range;
                (end.line - start.line, end.character.saturating_sub(start.character))
            })
    }
}

#[derive(Default)]
pub struct ProjectIndex {
    files: DashMap<Url, Arc<FileIndex>>,
}

impl ProjectIndex {
    pub fn file(&self, uri: &Url) -> Option<Arc<FileIndex>> {
        self.files.get(uri).map(|file| file.clone())
    }

    pub fn update(&self, files: FxHashMap<Url, FileIndex>) {
        for (uri, file) in files {
            self.files.insert(uri, Arc::new(file));
        }
    }
}
