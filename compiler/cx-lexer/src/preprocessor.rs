use crate::line_lexer::lex_line;
use crate::unified_lexer::{ConditionalFrame, Lexer, Macro};
use cx_tokens::token::{OperatorType, PunctuatorType, Token, TokenKind};
use cx_util::{CXResult, char_iter::CharIter, module_path::stdlib_directory};
use std::collections::HashMap;
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
                        break;
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

    let bundled = PathBuf::from(stdlib_directory(&format!("libc/{inner}")));
    let system = system_include_dirs()
        .into_iter()
        .map(|dir| dir.join(inner))
        .collect::<Vec<_>>();

    let search = candidates
        .into_iter()
        .chain(lexer.include_dirs.iter().map(|dir| dir.join(inner)))
        .collect::<Vec<_>>();

    if is_compiler_library_file(lexer) {
        search
            .into_iter()
            .chain(std::iter::once(bundled))
            .chain(system)
            .find(|path| path.is_file())
    } else {
        search
            .into_iter()
            .chain(system)
            .chain(std::iter::once(bundled))
            .find(|path| path.is_file())
    }
}

fn is_compiler_library_file(lexer: &Lexer) -> bool {
    let lib_dir = PathBuf::from(stdlib_directory(""));
    let file_path = lexer
        .file_path
        .canonicalize()
        .unwrap_or_else(|_| lexer.file_path.clone());
    let lib_dir = lib_dir.canonicalize().unwrap_or(lib_dir);

    file_path.starts_with(lib_dir)
}

fn system_include_dirs() -> Vec<PathBuf> {
    #[cfg(unix)]
    {
        let mut dirs = vec![PathBuf::from("/usr/include")];
        dirs.extend(gcc_include_dirs());
        dirs
    }

    #[cfg(not(unix))]
    {
        vec![]
    }
}

#[cfg(unix)]
fn gcc_include_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    let Ok(targets) = std::fs::read_dir("/usr/lib/gcc") else {
        return dirs;
    };

    for target in targets.flatten() {
        let Ok(versions) = std::fs::read_dir(target.path()) else {
            continue;
        };

        for version in versions.flatten() {
            let include_dir = version.path().join("include");
            if include_dir.is_dir() {
                dirs.push(include_dir);
            }
        }
    }

    dirs
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
    let mut directive = directive.to_string();
    if directive == "#" {
        let Some(name) = lexer.char_iter.next_word() else {
            return log_lexer_error!(
                lexer.file_path.as_path(),
                lexer.source,
                directive_start,
                lexer.char_iter.current_iter,
                "Expected preprocessor directive after '#'"
            );
        };
        directive.push_str(name);
    }
    let directive_end = lexer.char_iter.current_iter;

    match directive.as_str() {
        "#include" => {
            if !lexer.is_active() {
                skip_directive_tail(lexer);
                return Ok(());
            }

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

            let canonical_path = path.canonicalize().unwrap_or(path.clone());
            if lexer.once_files.contains(&canonical_path) {
                return Ok(());
            }

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
            if !lexer.is_active() {
                skip_directive_tail(lexer);
                return Ok(());
            }

            lexer.char_iter.skip_whitespace();
            let Some((name, params)) = read_macro_head(lexer) else {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "#define requires a macro name"
                );
            };

            let rest_of_line = rest_of_logical_directive(lexer);

            let replacement = strip_replacement_comments(&rest_of_line);
            let tokens = lex_line(
                &mut CharIter::new(&replacement),
                lexer.file_path.to_string_lossy().to_string(),
            )?;

            let macro_ = if let Some(params) = params {
                Macro::Function {
                    params: params.into_boxed_slice(),
                    body: tokens.into_boxed_slice(),
                }
            } else {
                Macro::Object(tokens.into_boxed_slice())
            };

            lexer.macros.insert(name, macro_);
        }

        "#undef" => {
            if lexer.is_active() {
                if let Some(name) = lexer.char_iter.next_word() {
                    lexer.macros.remove(name);
                }
            }
            skip_directive_tail(lexer);
        }

        "#ifdef" | "#ifndef" => {
            lexer.char_iter.skip_whitespace();
            let name_start = lexer.char_iter.current_iter;
            let Some(name) = lexer.char_iter.next_word() else {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "{} requires a macro name",
                    directive
                );
            };

            let is_defined = lexer.macros.contains_key(name);
            let condition = if directive == "#ifdef" {
                is_defined
            } else {
                !is_defined
            };
            push_conditional(lexer, condition);

            if lexer.char_iter.current_iter == name_start {
                skip_directive_tail(lexer);
            }
        }

        "#if" => {
            let condition = if lexer.is_active() {
                let expression = rest_of_logical_directive(lexer);
                eval_preprocessor_expr(lexer, &expression, directive_start)?
            } else {
                skip_directive_tail(lexer);
                false
            };
            push_conditional(lexer, condition);
        }

        "#elif" => {
            let parent_can_activate = match lexer.conditionals.last() {
                Some(frame) if frame.else_seen => {
                    return log_lexer_error!(
                        lexer.file_path.as_path(),
                        lexer.source,
                        directive_start,
                        directive_end,
                        "#elif after #else"
                    );
                }
                Some(frame) => frame.parent_active && !frame.any_branch_taken,
                None => {
                    return log_lexer_error!(
                        lexer.file_path.as_path(),
                        lexer.source,
                        directive_start,
                        directive_end,
                        "#elif without matching #if"
                    );
                }
            };

            let condition = if parent_can_activate {
                let expression = rest_of_logical_directive(lexer);
                eval_preprocessor_expr(lexer, &expression, directive_start)?
            } else {
                skip_directive_tail(lexer);
                false
            };

            let frame = lexer.conditionals.last_mut().unwrap();
            frame.branch_active = parent_can_activate && condition;
            frame.any_branch_taken |= frame.branch_active;
        }

        "#else" => {
            skip_directive_tail(lexer);
            let Some(frame) = lexer.conditionals.last_mut() else {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "#else without matching #if"
                );
            };

            if frame.else_seen {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "Duplicate #else"
                );
            }

            frame.else_seen = true;
            frame.branch_active = frame.parent_active && !frame.any_branch_taken;
            frame.any_branch_taken |= frame.branch_active;
        }

        "#endif" => {
            skip_directive_tail(lexer);
            if lexer.conditionals.pop().is_none() {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "#endif without matching #if"
                );
            }
        }

        "#pragma" => {
            if !lexer.is_active() {
                skip_directive_tail(lexer);
                return Ok(());
            }

            let pragma = rest_of_logical_directive(lexer).trim().to_string();
            if pragma == "once" {
                let canonical_path = lexer
                    .file_path
                    .canonicalize()
                    .unwrap_or_else(|_| lexer.file_path.clone());
                lexer.once_files.insert(canonical_path);
            }
        }

        "#line" => {
            skip_directive_tail(lexer);
        }

        "#warning" => {
            skip_directive_tail(lexer);
        }

        "#error" => {
            let message = rest_of_logical_directive(lexer);
            if lexer.is_active() {
                return log_lexer_error!(
                    lexer.file_path.as_path(),
                    lexer.source,
                    directive_start,
                    directive_end,
                    "#error{}{}",
                    if message.trim().is_empty() { "" } else { ": " },
                    message.trim()
                );
            }
        }

        dir => {
            if !lexer.is_active() {
                skip_directive_tail(lexer);
                return Ok(());
            }

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

fn rest_of_logical_directive(lexer: &mut Lexer) -> String {
    let mut output = String::new();

    loop {
        let line = lexer.char_iter.rest_of_line();
        let continued = line.trim_end().ends_with('\\');

        if continued {
            let trimmed = line.trim_end();
            output.push_str(&trimmed[..trimmed.len() - 1]);
            output.push(' ');
            if lexer.char_iter.peek() == Some('\n') {
                lexer.char_iter.next();
            }
        } else {
            output.push_str(line);
            break;
        }
    }

    output
}

fn strip_replacement_comments(input: &str) -> String {
    let mut output = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '/' {
            match chars.peek() {
                Some('/') => break,
                Some('*') => {
                    chars.next();
                    while let Some(c) = chars.next() {
                        if c == '*' && chars.peek() == Some(&'/') {
                            chars.next();
                            break;
                        }
                    }
                }
                _ => output.push(c),
            }
        } else {
            output.push(c);
        }
    }

    output
}

fn skip_directive_tail(lexer: &mut Lexer) {
    while let Some(c) = lexer.char_iter.peek() {
        if c == '\n' {
            lexer.char_iter.next();
            break;
        }

        if c == '/' {
            lexer.char_iter.next();
            match lexer.char_iter.peek() {
                Some('/') => {
                    lexer.char_iter.skip_line();
                    break;
                }
                Some('*') => {
                    lexer.char_iter.next();
                    while lexer.char_iter.has_next() {
                        if lexer.char_iter.peek() == Some('*') {
                            lexer.char_iter.next();
                            if lexer.char_iter.peek() == Some('/') {
                                lexer.char_iter.next();
                                break;
                            }
                        } else {
                            lexer.char_iter.next();
                        }
                    }
                }
                _ => {}
            }
        } else {
            lexer.char_iter.next();
        }
    }
}

fn read_macro_head(lexer: &mut Lexer) -> Option<(String, Option<Vec<String>>)> {
    let start = lexer.char_iter.current_iter;
    while let Some(c) = lexer.char_iter.peek() {
        if c.is_ascii_alphanumeric() || c == '_' {
            lexer.char_iter.next();
        } else {
            break;
        }
    }

    if start == lexer.char_iter.current_iter {
        return None;
    }

    let name = lexer.source[start..lexer.char_iter.current_iter].to_string();

    if lexer.char_iter.peek() != Some('(') {
        return Some((name, None));
    }

    lexer.char_iter.next();
    let params_start = lexer.char_iter.current_iter;
    while let Some(c) = lexer.char_iter.peek() {
        if c == ')' {
            let params_text = &lexer.source[params_start..lexer.char_iter.current_iter];
            lexer.char_iter.next();
            let params = if params_text.trim().is_empty() {
                Vec::new()
            } else {
                params_text
                    .split(',')
                    .map(|param| param.trim().to_string())
                    .collect()
            };
            return Some((name, Some(params)));
        }
        lexer.char_iter.next();
    }

    Some((name, Some(Vec::new())))
}

fn push_conditional(lexer: &mut Lexer, condition: bool) {
    let parent_active = lexer.is_active();
    let branch_active = parent_active && condition;
    lexer.conditionals.push(ConditionalFrame {
        parent_active,
        branch_active,
        any_branch_taken: branch_active,
        else_seen: false,
    });
}

fn eval_preprocessor_expr(
    lexer: &mut Lexer,
    expression: &str,
    directive_start: usize,
) -> CXResult<bool> {
    let expanded = expand_defined_ops(lexer, expression);
    let file_path = lexer.file_path.clone();
    let mut tokens = lex_line(
        &mut CharIter::new(&expanded),
        file_path.to_string_lossy().to_string(),
    )?;
    tokens = lexer.expand_macros(tokens);
    let mut parser = PreprocessorExprParser {
        tokens: &tokens,
        index: 0,
        macros: &lexer.macros,
    };

    match parser.parse_expression() {
        Some(value) => Ok(value != 0),
        None => log_lexer_error!(
            lexer.file_path.as_path(),
            lexer.source,
            directive_start,
            lexer.char_iter.current_iter,
            "Failed to evaluate preprocessor expression"
        ),
    }
}

fn expand_defined_ops(lexer: &Lexer, expression: &str) -> String {
    let mut result = String::new();
    let bytes = expression.as_bytes();
    let mut index = 0;

    while index < bytes.len() {
        if expression[index..].starts_with("defined") {
            let after = index + "defined".len();
            let prev_ok = index == 0
                || !expression[..index]
                    .chars()
                    .last()
                    .map(|c| c.is_ascii_alphanumeric() || c == '_')
                    .unwrap_or(false);
            let next_ok = after >= bytes.len()
                || !expression[after..]
                    .chars()
                    .next()
                    .map(|c| c.is_ascii_alphanumeric() || c == '_')
                    .unwrap_or(false);

            if prev_ok
                && next_ok
                && let Some((name, next_index)) = parse_defined_operand(expression, after)
            {
                result.push_str(if lexer.macros.contains_key(&name) {
                    "1"
                } else {
                    "0"
                });
                index = next_index;
                continue;
            }
        }

        result.push(bytes[index] as char);
        index += 1;
    }

    result
}

fn parse_defined_operand(expression: &str, mut index: usize) -> Option<(String, usize)> {
    index = skip_ascii_whitespace(expression, index);
    if expression.as_bytes().get(index) == Some(&b'(') {
        index += 1;
        index = skip_ascii_whitespace(expression, index);
        let (name, next) = parse_ident(expression, index)?;
        index = skip_ascii_whitespace(expression, next);
        if expression.as_bytes().get(index) != Some(&b')') {
            return None;
        }
        return Some((name, index + 1));
    }

    parse_ident(expression, index)
}

fn skip_ascii_whitespace(expression: &str, mut index: usize) -> usize {
    while expression
        .as_bytes()
        .get(index)
        .map(|byte| byte.is_ascii_whitespace())
        .unwrap_or(false)
    {
        index += 1;
    }
    index
}

fn parse_ident(expression: &str, index: usize) -> Option<(String, usize)> {
    let bytes = expression.as_bytes();
    let first = *bytes.get(index)?;
    if !(first.is_ascii_alphabetic() || first == b'_') {
        return None;
    }

    let mut end = index + 1;
    while bytes
        .get(end)
        .map(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
        .unwrap_or(false)
    {
        end += 1;
    }

    Some((expression[index..end].to_string(), end))
}

struct PreprocessorExprParser<'a> {
    tokens: &'a [Token],
    index: usize,
    macros: &'a HashMap<String, Macro>,
}

impl PreprocessorExprParser<'_> {
    fn parse_expression(&mut self) -> Option<i64> {
        self.parse_conditional()
    }

    fn parse_conditional(&mut self) -> Option<i64> {
        let condition = self.parse_logical_or()?;
        if !self.consume_punctuator(PunctuatorType::QuestionMark) {
            return Some(condition);
        }

        let true_value = self.parse_expression()?;
        if !self.consume_punctuator(PunctuatorType::Colon) {
            return None;
        }
        let false_value = self.parse_expression()?;

        Some(if condition != 0 {
            true_value
        } else {
            false_value
        })
    }

    fn parse_logical_or(&mut self) -> Option<i64> {
        let mut lhs = self.parse_logical_and()?;
        while self.consume_operator(OperatorType::DoubleBar) {
            let rhs = self.parse_logical_and()?;
            lhs = i64::from(lhs != 0 || rhs != 0);
        }
        Some(lhs)
    }

    fn parse_logical_and(&mut self) -> Option<i64> {
        let mut lhs = self.parse_equality()?;
        while self.consume_operator(OperatorType::DoubleAmpersand) {
            let rhs = self.parse_equality()?;
            lhs = i64::from(lhs != 0 && rhs != 0);
        }
        Some(lhs)
    }

    fn parse_equality(&mut self) -> Option<i64> {
        let mut lhs = self.parse_relational()?;
        loop {
            if self.consume_operator(OperatorType::Equal) {
                lhs = i64::from(lhs == self.parse_relational()?);
            } else if self.consume_operator(OperatorType::NotEqual) {
                lhs = i64::from(lhs != self.parse_relational()?);
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_relational(&mut self) -> Option<i64> {
        let mut lhs = self.parse_additive()?;
        loop {
            if self.consume_operator(OperatorType::Less) {
                lhs = i64::from(lhs < self.parse_additive()?);
            } else if self.consume_operator(OperatorType::LessEqual) {
                lhs = i64::from(lhs <= self.parse_additive()?);
            } else if self.consume_operator(OperatorType::Greater) {
                lhs = i64::from(lhs > self.parse_additive()?);
            } else if self.consume_operator(OperatorType::GreaterEqual) {
                lhs = i64::from(lhs >= self.parse_additive()?);
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_additive(&mut self) -> Option<i64> {
        let mut lhs = self.parse_multiplicative()?;
        loop {
            if self.consume_operator(OperatorType::Plus) {
                lhs += self.parse_multiplicative()?;
            } else if self.consume_operator(OperatorType::Minus) {
                lhs -= self.parse_multiplicative()?;
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Option<i64> {
        let mut lhs = self.parse_unary()?;
        loop {
            if self.consume_operator(OperatorType::Asterisk) {
                lhs *= self.parse_unary()?;
            } else if self.consume_operator(OperatorType::Slash) {
                let rhs = self.parse_unary()?;
                lhs /= rhs;
            } else if self.consume_operator(OperatorType::Percent) {
                let rhs = self.parse_unary()?;
                lhs %= rhs;
            } else {
                return Some(lhs);
            }
        }
    }

    fn parse_unary(&mut self) -> Option<i64> {
        if self.consume_operator(OperatorType::Exclamation) {
            return Some(i64::from(self.parse_unary()? == 0));
        }
        if self.consume_operator(OperatorType::Minus) {
            return Some(-self.parse_unary()?);
        }
        if self.consume_operator(OperatorType::Plus) {
            return self.parse_unary();
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Option<i64> {
        match self.tokens.get(self.index).map(|token| &token.kind)? {
            TokenKind::IntLiteral(value) => {
                self.index += 1;
                Some(*value)
            }
            TokenKind::Identifier(name) => {
                self.index += 1;
                match self.macros.get(name) {
                    Some(Macro::Object(body)) if body.len() == 1 => match body[0].kind {
                        TokenKind::IntLiteral(value) => Some(value),
                        _ => Some(0),
                    },
                    _ => Some(0),
                }
            }
            TokenKind::Punctuator(PunctuatorType::OpenParen) => {
                self.index += 1;
                let value = self.parse_expression()?;
                if !self.consume_punctuator(PunctuatorType::CloseParen) {
                    return None;
                }
                Some(value)
            }
            _ => None,
        }
    }

    fn consume_operator(&mut self, operator: OperatorType) -> bool {
        if matches!(
            self.tokens.get(self.index).map(|token| &token.kind),
            Some(TokenKind::Operator(op)) if *op == operator
        ) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn consume_punctuator(&mut self, punctuator: PunctuatorType) -> bool {
        if matches!(
            self.tokens.get(self.index).map(|token| &token.kind),
            Some(TokenKind::Punctuator(punc)) if *punc == punctuator
        ) {
            self.index += 1;
            true
        } else {
            false
        }
    }
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
