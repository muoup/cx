use cx_util::char_iter::CharIter;
use crate::unified_lexer::Lexer;

pub(crate) fn generate_lexable_slice<'a>(lexer: &'a mut Lexer<'a>) -> CharIter<'a> {
    todo!("\
        Generates a sub iterator which contains lexable text (i.e. no directive or comment in slice),\
        moves lexer's iterator to the end of this slice, and returns that sub iterator which contains\
        the current state of the lexer iterator with a shortened slice to what it should lex.\
    ")
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
        },

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
        },

        _ => {
            lexer.char_iter.back();
            false
        },
    }
}

pub(crate) fn handle_directive(lexer: &mut Lexer) {
    todo!("Handle preprocessor directives, move directive logic from preprocess_line")
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