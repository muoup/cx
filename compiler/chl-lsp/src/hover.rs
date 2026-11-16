use crate::index::{Declaration, DeclarationSite, FileIndex, SymbolClass};
use chl_doc::sections::DocComment;
use chl_hmir::HMIRDeclarationRole;
use std::cmp::Reverse;
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

const SINGLE_LINE_WIDTH: usize = 60;
const TYPE_LINES: usize = 24;

pub fn hover(index: &FileIndex, position: Position) -> Option<Hover> {
    let mut sections = Vec::new();
    let range = match index.reference_at(position) {
        Some(reference) => {
            let mut ty = reference.ty.as_deref();
            if let Some(declaration) = declaration(&reference.targets) {
                let mut text = format(declaration, reference.class);
                if let Some(inferred) = ty.filter(|_| is_inferred(&declaration.text)) {
                    text = format!("{text}: {inferred}");
                    ty = None;
                }
                sections.push(format!("```chl\n{text}\n```"));
            }
            if let Some(doc) = doc(&reference.targets) {
                sections.push(render(&DocComment::parse(doc)));
            }
            if let Some(ty) = ty {
                sections.push(format!("Type: `{ty}`"));
            }
            reference.range
        }
        None => {
            let span = index.type_at(position)?;
            sections.push(format!("Type: `{}`", span.ty));
            span.range
        }
    };
    if sections.is_empty() {
        return None;
    }

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: sections.join("\n\n---\n\n"),
        }),
        range: Some(range),
    })
}

fn declaration(targets: &[DeclarationSite]) -> Option<&Declaration> {
    targets
        .iter()
        .find(|target| target.role == HMIRDeclarationRole::Definition)
        .or(targets.first())
        .map(|target| &target.declaration)
        .filter(|declaration| !declaration.text.is_empty())
}

// The definition's doc comment, or else the first a declaration has
fn doc(targets: &[DeclarationSite]) -> Option<&str> {
    let mut targets = targets.iter().collect::<Vec<_>>();
    targets.sort_by_key(|target| Reverse(target.role));
    targets.into_iter().find_map(|target| target.declaration.doc.as_deref())
}

// A declaration whose type is left to inference ('auto x', 'some(auto& x)')
fn is_inferred(text: &str) -> bool {
    text.split(|character: char| !(character.is_alphanumeric() || character == '_'))
        .any(|word| word == "auto")
}

fn render(doc: &DocComment) -> String {
    let mut blocks = vec![doc.summary.clone()];
    blocks.extend(doc.body.clone());
    if !doc.parameters.is_empty() {
        let items = doc
            .parameters
            .iter()
            .map(|parameter| format!("- `{}` — {}", parameter.name, parameter.description))
            .collect::<Vec<_>>();
        blocks.push(format!("**Parameters**\n\n{}", items.join("\n")));
    }
    if let Some(returns) = &doc.returns {
        blocks.push(format!("**Returns**\n\n{returns}"));
    }
    if let Some(examples) = &doc.examples {
        blocks.push(format!("**Examples**\n\n{examples}"));
    }
    blocks.retain(|block| !block.is_empty());
    blocks.join("\n\n")
}

fn format(declaration: &Declaration, class: SymbolClass) -> String {
    let text = &declaration.text;
    if class == SymbolClass::Type {
        return truncate(text);
    }
    if class != SymbolClass::Function || text.len() <= SINGLE_LINE_WIDTH {
        return text.clone();
    }
    let Some((open, close)) = parameter_list(text, declaration.name_end) else {
        return text.clone();
    };
    let params = split_top_level(&text[open + 1..close]);
    if params.is_empty() {
        return text.clone();
    }

    let mut lines = vec![text[..=open].to_string()];
    let last = params.len() - 1;
    for (index, param) in params.iter().enumerate() {
        let separator = if index == last { "" } else { "," };
        lines.push(format!("    {param}{separator}"));
    }
    lines.push(text[close..].to_string());
    lines.join("\n")
}

fn truncate(text: &str) -> String {
    let lines = text.lines().collect::<Vec<_>>();
    if lines.len() <= TYPE_LINES {
        return text.to_string();
    }
    let mut kept = lines[..TYPE_LINES - 1].to_vec();
    kept.push("    // ...");
    kept.join("\n")
}

// The parentheses of the parameter list that directly follows the declared name
fn parameter_list(text: &str, name_end: usize) -> Option<(usize, usize)> {
    let rest = text.get(name_end..)?;
    let open = name_end + rest.find(|character: char| !character.is_whitespace())?;
    if !text[open..].starts_with('(') {
        return None;
    }

    let mut depth = 0usize;
    for (offset, character) in text[open..].char_indices() {
        match character {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    return Some((open, open + offset));
                }
            }
            _ => {}
        }
    }
    None
}

fn split_top_level(list: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0;
    for (offset, character) in list.char_indices() {
        match character {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                parts.push(list[start..offset].trim());
                start = offset + 1;
            }
            _ => {}
        }
    }
    parts.push(list[start..].trim());
    parts.retain(|part| !part.is_empty());
    parts
}
