use std::{
    cmp::Reverse,
    iter,
    path::{Path, PathBuf},
};

use cx_hir::ast::modifiers::VisibilityMode;
use cx_hmir::{HMIRDeclaration, HMIRDef, HMIRDefKind, HMIRUnit};
use cx_namespace::module::NamespacePath;
use cx_tokens::token::Token;
use rustc_hash::FxHashMap;

use crate::{
    comment, declaration,
    model::{ItemDoc, ItemKind, ModuleDoc},
    sections::DocComment,
};

#[derive(Default)]
pub struct Sources {
    files: FxHashMap<PathBuf, Option<String>>,
}

impl Sources {
    pub fn get(&mut self, path: &Path) -> Option<&str> {
        self.files
            .entry(path.to_path_buf())
            .or_insert_with(|| std::fs::read_to_string(path).ok())
            .as_deref()
    }
}

pub struct ModuleSource<'a> {
    pub unit: &'a HMIRUnit,
    pub file: &'a Path,
    pub tokens: &'a [Token],
}

pub struct Extraction {
    pub module: ModuleDoc,
    pub warnings: Vec<String>,
}

struct Placed {
    start: usize,
    end: usize,
    item: ItemDoc,
}

// What the module exports, in declaration order; 'root' is what its source path is reported relative to
pub fn module(input: ModuleSource, root: &Path, sources: &mut Sources) -> Option<Extraction> {
    let ModuleSource { unit, file, tokens } = input;
    let source = sources.get(file)?.to_string();
    let mut warnings = Vec::new();
    let mut placed = Vec::new();

    for (_, def) in unit.defs() {
        let Some(shown) = exported(def, file) else {
            continue;
        };
        let Some((_, start, header_end)) = shown.span().source_bounds() else {
            continue;
        };
        let kind = kind(def);
        let (end, declaration) = match kind {
            ItemKind::Type => {
                let end = declaration::end(tokens, file, header_end).unwrap_or(header_end);
                (end, declaration::dedent(&source, start, end))
            }
            ItemKind::Function | ItemKind::Global => {
                (header_end, declaration::collapse(&source[start..header_end]))
            }
        };
        let name = item_name(def, unit.namespace());
        let doc = doc(def, sources).map(|text| DocComment::parse(&text));
        if let Some(doc) = &doc {
            for parameter in unknown_parameters(shown, doc) {
                warnings.push(format!(
                    "{}: '{name}' documents unknown parameter '{parameter}'",
                    file.display()
                ));
            }
        }
        placed.push(Placed {
            start,
            end,
            item: ItemDoc {
                name,
                kind,
                declaration,
                line: source[..start].matches('\n').count() + 1,
                doc,
            },
        });
    }

    placed.sort_by_key(|placed| placed.start);
    let types = placed
        .iter()
        .filter(|placed| placed.item.kind == ItemKind::Type)
        .map(|placed| (placed.start, placed.end))
        .collect::<Vec<_>>();
    placed.retain(|placed| {
        !types
            .iter()
            .any(|&(start, end)| start < placed.start && placed.start < end)
    });

    Some(Extraction {
        module: ModuleDoc {
            module: unit.namespace().to_string(),
            source: file
                .strip_prefix(root)
                .unwrap_or(file)
                .to_string_lossy()
                .replace('\\', "/"),
            doc: comment::module(&source).map(|text| DocComment::parse(&text)),
            items: placed.into_iter().map(|placed| placed.item).collect(),
        },
        warnings,
    })
}

// The declaration a def is shown by when one in 'file' exports it: its definition there, or else its first there
fn exported<'a>(def: &'a HMIRDef, file: &Path) -> Option<&'a HMIRDeclaration> {
    let in_file = def
        .declarations()
        .iter()
        .filter(|declaration| {
            declaration
                .span()
                .source_bounds()
                .is_some_and(|(origin, ..)| origin == file)
        })
        .collect::<Vec<_>>();
    in_file
        .iter()
        .any(|declaration| declaration.visibility() == VisibilityMode::Public)
        .then(|| in_file.into_iter().min_by_key(|declaration| Reverse(declaration.role())))
        .flatten()
}

fn kind(def: &HMIRDef) -> ItemKind {
    if def.declares_type() {
        return ItemKind::Type;
    }
    let function = def
        .declarations()
        .iter()
        .any(|declaration| matches!(declaration.kind(), Some(HMIRDefKind::Function(_))));
    match function {
        true => ItemKind::Function,
        false => ItemKind::Global,
    }
}

// The definition's doc comment, or else the first a declaration has
pub fn doc(def: &HMIRDef, sources: &mut Sources) -> Option<String> {
    let mut declarations = def.declarations().iter().collect::<Vec<_>>();
    declarations.sort_by_key(|declaration| Reverse(declaration.role()));
    declarations.into_iter().find_map(|declaration| {
        let (file, start, _) = declaration.span().source_bounds()?;
        comment::leading(sources.get(file)?, start)
    })
}

fn item_name(def: &HMIRDef, module: &NamespacePath) -> String {
    let name = def.name();
    let namespace = name.namespace.clone();
    let relative = namespace.clone().strip_prefix(module).unwrap_or(namespace);
    relative
        .segments()
        .iter()
        .map(|segment| segment.as_str())
        .chain(iter::once(source_name(name.name.as_str())))
        .collect::<Vec<_>>()
        .join("::")
}

// Tagged names carry their tag ('struct point')
fn source_name(name: &str) -> &str {
    name.rsplit(' ').next().unwrap_or_default()
}

fn unknown_parameters<'a>(
    declaration: &HMIRDeclaration,
    doc: &'a DocComment,
) -> impl Iterator<Item = &'a str> {
    let names = match declaration.kind() {
        Some(HMIRDefKind::Function(function)) => function
            .prototype()
            .params()
            .iter()
            .filter_map(|param| param.name().map(|name| name.as_str().to_string()))
            .collect(),
        _ => Vec::new(),
    };
    doc.parameters
        .iter()
        .map(|parameter| parameter.name.as_str())
        .filter(move |name| !names.iter().any(|known| known == name))
}
