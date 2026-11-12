use crate::index::{Declaration, DeclarationSite, FileIndex, SymbolClass};
use cx_hmir::HMIRDeclarationRole;
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

const SINGLE_LINE_WIDTH: usize = 60;

pub fn hover(index: &FileIndex, position: Position) -> Option<Hover> {
    let (range, declaration, ty) = match index.reference_at(position) {
        Some(reference) => (
            reference.range,
            declaration(&reference.targets).map(|declaration| format(declaration, reference.class)),
            reference.ty.as_deref(),
        ),
        None => {
            let span = index.type_at(position)?;
            (span.range, None, Some(span.ty.as_str()))
        }
    };

    let mut sections = Vec::new();
    if let Some(declaration) = declaration {
        sections.push(format!("```cx\n{declaration}\n```"));
    }
    if let Some(ty) = ty {
        sections.push(format!("Type: `{ty}`"));
    }
    if sections.is_empty() {
        return None;
    }

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: sections.join("\n\n"),
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

fn format(declaration: &Declaration, class: SymbolClass) -> String {
    let text = &declaration.text;
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
