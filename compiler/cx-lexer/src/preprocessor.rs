use crate::unified_lexer::Lexer;
use cx_util::{CXResult, char_iter::CharIter, module_path::stdlib_directory};
use std::path::PathBuf;

pub(crate) fn generate_lexable_slice<'a>(lexer: &mut Lexer<'a>) -> Option<CharIter<'a>> {
    lexer.char_iter.skip_whitespace();

    // find whichever comes first: end of line or start of comment
    let start = lexer.char_iter.current_iter;

    while let Some(c) = lexer.char_iter.next() {
        match c {
            '\n' => break,
            '/' => {
                if lexer.char_iter.peek() == Some('/') || lexer.char_iter.peek() == Some('*') {
                    lexer.char_iter.back();
                    break;
                }
            }
            _ => (),
        }
    }

    if start == lexer.char_iter.current_iter {
        return None;
    }

    Some(CharIter::sub_iter(
        &lexer.char_iter,
        start,
        &lexer.source[0..lexer.char_iter.current_iter],
    ))
}

// returns true if a comment was handled, false otherwise
pub(crate) fn handle_comment(lexer: &mut Lexer) -> bool {
    assert_eq!(lexer.char_iter.peek(), Some('/'));
    lexer.char_iter.next();

    match lexer.char_iter.peek() {
        Some('/') => {
            lexer.char_iter.next();
            lexer.char_iter.skip_line();
            true
        }

        Some('*') => {
            lexer.char_iter.next();
            while lexer.char_iter.has_next() {
                if lexer.char_iter.peek() == Some('*') {
                    lexer.char_iter.next();

                    if lexer.char_iter.peek() == Some('/') {
                        lexer.char_iter.next();
                    }
                } else {
                    lexer.char_iter.next();
                }
            }

            true
        }

        _ => {
            lexer.char_iter.back();
            false
        }
    }
}

fn resolve_include_path(lexer: &Lexer, file_name: &str) -> Option<PathBuf> {
    let is_quoted = file_name.starts_with('"') && file_name.ends_with('"');
    let is_angled = file_name.starts_with('<') && file_name.ends_with('>');

    if !is_quoted && !is_angled {
        return None;
    }

    let inner = &file_name[1..file_name.len() - 1];
    let mut candidates = Vec::new();

    if is_quoted && let Some(parent) = lexer.file_path.parent() {
        candidates.push(parent.join(inner));
    }

    std::iter::once(PathBuf::from(stdlib_directory(&format!("libc/{inner}"))))
        .chain(lexer.include_dirs.iter().map(|dir| dir.join(inner)))
        .find(|path| path.is_file())
}

pub(crate) fn handle_directive(lexer: &mut Lexer) -> CXResult<()> {
    let directive_start = lexer.char_iter.current_iter;
    let Some(directive) = lexer.char_iter.next_word() else {
        return log_lexer_error!(
            lexer.file_path.as_path(),
            lexer.source,
            directive_start,
            lexer.char_iter.current_iter,
            "Expected preprocessor directive"
        );
    };
    let directive = directive.to_string();
    let directive_end = lexer.char_iter.current_iter;

    match directive.as_str() {
        "#include" => {
            lexer.char_iter.skip_whitespace();
            let file_name_start = lexer.char_iter.current_iter;
            let Some(file_name) = lexer.char_iter.next_word() else {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "#include requires a file path"
                );
            };
            let file_name = file_name.to_string();
            let file_name_end = lexer.char_iter.current_iter;

            let path = match resolve_include_path(lexer, &file_name) {
                Some(path) => path,
                None if !(file_name.starts_with('"') && file_name.ends_with('"'))
                    && !(file_name.starts_with('<') && file_name.ends_with('>')) =>
                {
                    return log_lexer_error!(
                        lexer.file_path.as_path(),
                        lexer.source,
                        file_name_start,
                        file_name_end,
                        "Invalid include path '{}': expected \"...\" or <...>",
                        file_name
                    );
                }
                None => {
                    return log_lexer_error!(
                        lexer.file_path.as_path(),
                        lexer.source,
                        file_name_start,
                        file_name_end,
                        "Included file not found: {file_name}"
                    );
                }
            };

            let string = match std::fs::read_to_string(path.as_path()) {
                Ok(string) => string,
                Err(e) => {
                    return log_lexer_error!(
                        lexer.file_path.as_path(),
                        lexer.source,
                        file_name_start,
                        file_name_end,
                        "Failed to read included file {}: {}",
                        path.display(),
                        e
                    );
                }
            };

            let tokens = lexer.independent_lex(&string, path.as_path())?;
            lexer.tokens.extend(tokens);
        }

        "#define" => {
            lexer.char_iter.skip_whitespace();
            let Some(name) = lexer.char_iter.next_word() else {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "#define requires a macro name"
                );
            };
            let name = name.to_string();

            let rest_of_line = lexer.char_iter.rest_of_line().to_string();

            let file_path = lexer.file_path.clone();
            let tokens = lexer.independent_lex(&rest_of_line, &file_path)?;

            lexer.macros.insert(name, tokens.into_boxed_slice());
        }

        dir => {
            return log_lexer_error!(
                lexer.file_path.as_path(),
                lexer.source,
                directive_start,
                directive_end,
                "Preprocessor directive '{}' is not yet implemented",
                dir
            );
        }
    }

    Ok(())
}

// pub(crate) fn preprocess_line(preprocessor: &mut Preprocessor, mut string: &str) -> String {
//     if string.contains("/*") {
//         if string.contains("*/") {
//             let pre = string.split_once("/*").unwrap().0;
//             let post = string.split_once("*/").unwrap().1;
//
//             return format!("{}{}", preprocess_line(preprocessor, pre), preprocess_line(preprocessor, post));
//         }
//
//         string = string.split_once("/*").unwrap().0;
//         preprocessor.in_ml_comment = true;
//     }
//
//     if preprocessor.in_ml_comment {
//         if string.contains("*/") {
//             string = string.rsplit_once("*/")
//                 .unwrap()
//                 .1;
//             preprocessor.in_ml_comment = false;
//         } else {
//             return "".to_string();
//         }
//     }
//
//     if string.contains("//") {
//         string = string.split("//").next().unwrap();
//     }
//
//     if !string.trim_start().starts_with("#") {
//         return handle_non_directive(preprocessor, string);
//     }
//
//     let mut split = string.split_whitespace();
//
//     match split.next().unwrap() {
//         "#include" => {
//             let file_name = split.next().unwrap();
//
//             let prefix = if file_name.starts_with("\"") && file_name.ends_with("\"") {
//                 "".to_string()
//             } else if file_name.starts_with("<") && file_name.ends_with(">") {
//                 format!("{}/libc/", libary_path_prefix())
//             } else {
//                 panic!("Invalid include statement: {file_name}");
//             };
//
//             let path = format!("{}{}", prefix, &file_name[1.. file_name.len() - 1]);
//             let string = std::fs::read_to_string(path.as_str())
//                 .unwrap_or_else(|_| panic!("Failed to read file: {path}"));
//
//             string.lines()
//                 .map(|line| preprocess_line(preprocessor, line))
//                 .collect::<Vec<_>>()
//                 .join("\n")
//         },
//         "#define" => {
//             let token = split.next().unwrap();
//             let value = split.collect::<Vec<_>>().join(" ");
//
//             preprocessor.defined_tokens.push((token.to_string(), value.to_string()));
//             "".to_string()
//         },
//         dir => todo!("Preprocessor directive not implemented: {dir}")
//     }
// }
