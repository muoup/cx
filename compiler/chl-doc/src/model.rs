use serde::Serialize;

use crate::sections::DocComment;

#[derive(Debug, Serialize)]
pub struct Documentation {
    pub modules: Vec<ModuleDoc>,
}

#[derive(Debug, Serialize)]
pub struct ModuleDoc {
    pub module: String,
    pub source: String,
    pub doc: Option<DocComment>,
    pub items: Vec<ItemDoc>,
}

#[derive(Debug, Serialize)]
pub struct ItemDoc {
    pub name: String,
    pub kind: ItemKind,
    pub declaration: String,
    pub line: usize,
    pub doc: Option<DocComment>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum ItemKind {
    Type,
    Function,
    Global,
}

impl Documentation {
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).expect("documentation serializes")
    }
}
