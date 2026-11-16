use serde::Serialize;

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct DocComment {
    pub summary: String,
    pub body: Option<String>,
    pub parameters: Vec<DocParameter>,
    pub returns: Option<String>,
    pub examples: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DocParameter {
    pub name: String,
    pub description: String,
}

#[derive(Clone, Copy)]
enum Section {
    Prose,
    Parameters,
    Returns,
    Examples,
}

impl DocComment {
    pub fn parse(text: &str) -> Self {
        let mut prose = Vec::new();
        let mut parameters = Vec::new();
        let mut returns = Vec::new();
        let mut examples = Vec::new();

        let mut section = Section::Prose;
        let mut fenced = false;
        for line in text.lines() {
            if !fenced && let Some(title) = heading(line) {
                section = match title.to_ascii_lowercase().as_str() {
                    "parameters" | "params" => Section::Parameters,
                    "returns" | "return" => Section::Returns,
                    "examples" | "example" => Section::Examples,
                    _ => Section::Prose,
                };
                if matches!(section, Section::Prose) {
                    prose.push(line);
                }
                continue;
            }
            if is_fence(line) {
                fenced = !fenced;
            }
            match section {
                Section::Prose => prose.push(line),
                Section::Parameters => parameter(&mut parameters, line),
                Section::Returns => returns.push(line),
                Section::Examples => examples.push(line),
            }
        }

        let prose = block(&prose).unwrap_or_default();
        let (summary, body) = match prose.split_once("\n\n") {
            Some((summary, body)) => (summary.trim().to_string(), block(&[body])),
            None => (prose, None),
        };
        Self {
            summary,
            body,
            parameters,
            returns: block(&returns),
            examples: block(&examples),
        }
    }
}

fn heading(line: &str) -> Option<&str> {
    let text = line.trim_start();
    let title = text.trim_start_matches('#');
    (title.len() < text.len() && title.starts_with(' ')).then(|| title.trim())
}

fn is_fence(line: &str) -> bool {
    let text = line.trim_start();
    text.starts_with("```") || text.starts_with("~~~")
}

// '- name: description', continued on the lines that follow
fn parameter(parameters: &mut Vec<DocParameter>, line: &str) {
    let text = line.trim();
    if text.is_empty() {
        return;
    }
    let item = text.strip_prefix("- ").or_else(|| text.strip_prefix("* "));
    let named = item.and_then(|item| item.split_once(':')).and_then(|(name, description)| {
        let name = name.trim().trim_matches('`');
        let identifier = !name.is_empty()
            && name.chars().all(|character| character.is_alphanumeric() || character == '_');
        identifier.then(|| (name, description.trim()))
    });
    match (named, parameters.last_mut()) {
        (Some((name, description)), _) => parameters.push(DocParameter {
            name: name.to_string(),
            description: description.to_string(),
        }),
        (None, Some(last)) => {
            last.description.push(' ');
            last.description.push_str(text);
        }
        (None, None) => {}
    }
}

fn block(lines: &[&str]) -> Option<String> {
    let text = lines.join("\n");
    let text = text.trim();
    (!text.is_empty()).then(|| text.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splits_recognized_sections() {
        let doc = DocComment::parse(
            "Appends a value.\n\nGrows when full.\n\n# Parameters\n- this: The vector.\n- `object`: The value\n  moved in.\n\n# Returns\nNothing.\n\n# Examples\n```cx\n# not a heading\nv |> push(1);\n```",
        );
        assert_eq!(doc.summary, "Appends a value.");
        assert_eq!(doc.body.as_deref(), Some("Grows when full."));
        assert_eq!(
            doc.parameters,
            vec![
                DocParameter { name: "this".into(), description: "The vector.".into() },
                DocParameter { name: "object".into(), description: "The value moved in.".into() },
            ]
        );
        assert_eq!(doc.returns.as_deref(), Some("Nothing."));
        assert_eq!(doc.examples.as_deref(), Some("```cx\n# not a heading\nv |> push(1);\n```"));
    }

    #[test]
    fn unknown_headings_stay_in_prose() {
        let doc = DocComment::parse("Summary.\n\n# Safety\nCareful.");
        assert_eq!(doc.body.as_deref(), Some("# Safety\nCareful."));
    }
}
