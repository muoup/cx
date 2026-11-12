use crate::index::{FileIndex, SymbolClass};
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

#[allow(deprecated)]
pub fn document_symbols(index: &FileIndex) -> Vec<DocumentSymbol> {
    index
        .symbols
        .iter()
        .map(|symbol| DocumentSymbol {
            name: symbol.name.clone(),
            detail: None,
            kind: match symbol.class {
                SymbolClass::Function => SymbolKind::FUNCTION,
                SymbolClass::Type => SymbolKind::STRUCT,
                SymbolClass::Global | SymbolClass::Local | SymbolClass::Parameter => {
                    SymbolKind::VARIABLE
                }
            },
            tags: None,
            deprecated: None,
            range: symbol.range,
            selection_range: symbol.selection,
            children: None,
        })
        .collect()
}
