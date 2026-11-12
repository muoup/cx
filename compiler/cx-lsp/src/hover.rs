use crate::index::{DeclarationSite, FileIndex};
use cx_hmir::HMIRDeclarationRole;
use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

pub fn hover(index: &FileIndex, position: Position) -> Option<Hover> {
    let (range, declaration, ty) = match index.reference_at(position) {
        Some(reference) => (
            reference.range,
            declaration(&reference.targets),
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

fn declaration(targets: &[DeclarationSite]) -> Option<&str> {
    targets
        .iter()
        .find(|target| target.role == HMIRDeclarationRole::Definition)
        .or(targets.first())
        .map(|target| target.declaration.as_str())
        .filter(|declaration| !declaration.is_empty())
}
