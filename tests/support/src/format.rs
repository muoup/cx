use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct TestSpec {
    pub path: PathBuf,
    pub stdout: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct FormatError {
    pub path: PathBuf,
    pub line: usize,
    pub message: String,
}

impl Display for FormatError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.path.display(), self.line, self.message)
    }
}

impl std::error::Error for FormatError {}

pub fn parse_file(path: &Path) -> Result<TestSpec, FormatError> {
    let source = std::fs::read_to_string(path).map_err(|error| FormatError {
        path: path.to_path_buf(),
        line: 0,
        message: format!("failed to read test source: {error}"),
    })?;

    parse_source(path, &source)
}

pub fn expected_stdout(path: &Path) -> Result<Option<String>, FormatError> {
    let spec = parse_file(path)?;

    if let Some(lines) = spec.stdout {
        return Ok(Some(stdout_text(&lines)));
    }

    let sidecar = path.with_extension("cx-output");
    if !sidecar.exists() {
        return Ok(None);
    }

    std::fs::read_to_string(&sidecar)
        .map(Some)
        .map_err(|error| FormatError {
            path: sidecar,
            line: 0,
            message: format!("failed to read expected output: {error}"),
        })
}

pub fn parse_source(path: &Path, source: &str) -> Result<TestSpec, FormatError> {
    let mut stdout: Option<Vec<String>> = None;

    for (line, comment) in comments(source) {
        for (offset, comment_line) in comment.lines().enumerate() {
            let line_number = line + offset;
            let directive = comment_line.trim_start();

            if let Some(value) = directive.strip_prefix("CX-STDOUT-NEXT:") {
                let lines = stdout.as_mut().ok_or_else(|| FormatError {
                    path: path.to_path_buf(),
                    line: line_number,
                    message: "CX-STDOUT-NEXT must follow CX-STDOUT".to_string(),
                })?;
                lines.push(directive_value(value));
                continue;
            }

            if let Some(value) = directive.strip_prefix("CX-STDOUT:") {
                if stdout.is_some() {
                    return Err(FormatError {
                        path: path.to_path_buf(),
                        line: line_number,
                        message: "use CX-STDOUT-NEXT for subsequent output lines".to_string(),
                    });
                }

                stdout = Some(vec![directive_value(value)]);
            }
        }
    }

    Ok(TestSpec {
        path: path.to_path_buf(),
        stdout,
    })
}

fn directive_value(value: &str) -> String {
    let value = value.strip_prefix(' ').unwrap_or(value);
    value.strip_suffix(' ').unwrap_or(value).to_string()
}

fn stdout_text(lines: &[String]) -> String {
    let mut output = lines.join("\n");
    output.push('\n');
    output
}

fn comments(source: &str) -> Vec<(usize, String)> {
    let bytes = source.as_bytes();
    let mut comments = Vec::new();
    let mut index = 0;
    let mut line = 1;

    while index < bytes.len() {
        if bytes[index] == b'/' && bytes.get(index + 1) == Some(&b'/') {
            let comment_line = line;
            let start = index + 2;
            index = start;
            while index < bytes.len() && bytes[index] != b'\n' {
                index += 1;
            }
            comments.push((comment_line, source[start..index].to_string()));
            continue;
        }

        if bytes[index] == b'/' && bytes.get(index + 1) == Some(&b'*') {
            let comment_line = line;
            let start = index + 2;
            index = start;
            while index < bytes.len() {
                if bytes[index] == b'*' && bytes.get(index + 1) == Some(&b'/') {
                    comments.push((comment_line, source[start..index].to_string()));
                    index += 2;
                    break;
                }
                if bytes[index] == b'\n' {
                    line += 1;
                }
                index += 1;
            }
            continue;
        }

        if bytes[index] == b'"' || bytes[index] == b'\'' {
            let quote = bytes[index];
            index += 1;
            while index < bytes.len() {
                if bytes[index] == b'\\' {
                    index = (index + 2).min(bytes.len());
                    continue;
                }
                if bytes[index] == quote {
                    index += 1;
                    break;
                }
                if bytes[index] == b'\n' {
                    line += 1;
                }
                index += 1;
            }
            continue;
        }

        if bytes[index] == b'\n' {
            line += 1;
        }
        index += 1;
    }

    comments
}

#[cfg(test)]
mod tests {
    use super::parse_source;
    use std::path::Path;

    #[test]
    fn parses_stdout_sequence_from_line_comments() {
        let source = "// CX-STDOUT: first\n// CX-STDOUT-NEXT: second\n";
        let spec = parse_source(Path::new("case.cx"), source).unwrap();

        assert_eq!(
            spec.stdout,
            Some(vec!["first".to_string(), "second".to_string()])
        );
    }

    #[test]
    fn parses_directives_from_block_comments() {
        let source = "/* CX-STDOUT: first\n CX-STDOUT-NEXT: second */\n";
        let spec = parse_source(Path::new("case.cx"), source).unwrap();

        assert_eq!(
            spec.stdout,
            Some(vec!["first".to_string(), "second".to_string()])
        );
    }

    #[test]
    fn ignores_directive_text_in_literals() {
        let source = "const char* text = \"CX-STDOUT: not a directive\";\n";
        let spec = parse_source(Path::new("case.c"), source).unwrap();

        assert_eq!(spec.stdout, None);
    }

    #[test]
    fn requires_a_first_stdout_directive() {
        let error = parse_source(Path::new("case.cx"), "// CX-STDOUT-NEXT: second\n").unwrap_err();

        assert!(error.message.contains("must follow"));
        assert_eq!(error.line, 1);
    }
}
