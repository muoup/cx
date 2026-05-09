use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use cx_tokens::token::{PunctuatorType, Token, TokenKind};
use cx_util::{module_path::cx_library_directory, CXError, CXResult};

use crate::{
    lexer::{
        comments::skip_directive_tail,
        scanner::{LexEvent, LexTransition, Lexer},
        source::{ConditionalFrame, SourceFrame},
    },
    preprocessor::{Preprocessor, builtins::builtin_macros},
};

#[derive(Clone)]
pub(crate) enum Macro {
    Object(Box<[Token]>),
    Function {
        params: Box<[String]>,
        body: Box<[Token]>,
    },
}

pub(crate) struct SourceInput {
    pub(crate) source: String,
    pub(crate) path: PathBuf,
}

pub(crate) struct LexingContext {
    pub(crate) include_dirs: Vec<PathBuf>,
    pub(crate) macros: HashMap<String, Macro>,
    pub(crate) once_files: HashSet<PathBuf>,
    sources: Vec<SourceFrame>,
    pending_tokens: Vec<Token>,
    tokens: Vec<Token>,
}

impl LexingContext {
    pub(crate) fn new(
        source: String,
        source_path: &Path,
        include_dirs: &[PathBuf],
    ) -> CXResult<Self> {
        let builtin_path = PathBuf::from(cx_library_directory("libc/internal/__builtins.h"));
        let builtin_source = std::fs::read_to_string(&builtin_path).map_err(|e| {
            CXError::create_boxed(format!(
                "Failed to read internal builtin header {}: {}",
                builtin_path.display(),
                e
            ))
        })?;

        Ok(Self {
            include_dirs: include_dirs.to_vec(),
            macros: builtin_macros(),
            once_files: HashSet::new(),
            sources: vec![
                SourceFrame::new(source, source_path),
                SourceFrame::new(builtin_source, &builtin_path),
            ],
            pending_tokens: Vec::new(),
            tokens: Vec::new(),
        })
    }

    pub(crate) fn run(mut self) -> CXResult<Vec<Token>> {
        let preprocessor = Preprocessor;

        while !self.sources.is_empty() {
            let event = {
                let frame = self.current_frame_mut();
                Lexer::new(frame).step()?
            };

            let transition = match event {
                LexEvent::Continue => LexTransition::Continue,
                LexEvent::Tokens(tokens) => {
                    self.emit_tokens(tokens);
                    LexTransition::Continue
                }
                LexEvent::Directive => preprocessor.handle_directive(&mut self)?,
                LexEvent::Eof => LexTransition::PopSource,
            };

            self.apply_transition(transition)?;
        }

        Ok(self.tokens)
    }

    pub(crate) fn current_frame(&self) -> &SourceFrame {
        self.sources.last().unwrap()
    }

    pub(crate) fn current_frame_mut(&mut self) -> &mut SourceFrame {
        self.sources.last_mut().unwrap()
    }

    pub(crate) fn emit_tokens(&mut self, tokens: Vec<Token>) {
        self.pending_tokens.extend(tokens);
        if !self.has_incomplete_function_macro_invocation(&self.pending_tokens) {
            self.flush_pending_tokens();
        }
    }

    fn apply_transition(&mut self, transition: LexTransition) -> CXResult<()> {
        match transition {
            LexTransition::Continue => {}
            LexTransition::MoveTo(index) => {
                self.current_frame_mut().cursor = index;
            }
            LexTransition::PushSource(input) => {
                self.sources
                    .push(SourceFrame::new(input.source, &input.path));
            }
            LexTransition::PopSource => {
                self.finish_current_source()?;
                self.sources.pop();
            }
        }

        Ok(())
    }

    fn finish_current_source(&mut self) -> CXResult<()> {
        let frame = self.current_frame();

        if let Some(conditional) = frame.conditionals.last() {
            return log_lexer_error!(
                frame.file_path.as_path(),
                &frame.source,
                frame.cursor,
                frame.cursor,
                "Unclosed preprocessor conditional starting in {} branch",
                if conditional.parent_active {
                    "active"
                } else {
                    "inactive"
                }
            );
        }

        self.flush_pending_tokens();
        Ok(())
    }

    fn flush_pending_tokens(&mut self) {
        if self.pending_tokens.is_empty() {
            return;
        }

        let pending = std::mem::take(&mut self.pending_tokens);
        let expanded = self.expand_macros(pending);
        self.tokens.extend(expanded);
    }

    fn has_incomplete_function_macro_invocation(&self, tokens: &[Token]) -> bool {
        let mut index = 0;

        while index < tokens.len() {
            let TokenKind::Identifier(name) = &tokens[index].kind else {
                index += 1;
                continue;
            };

            if !matches!(self.macros.get(name), Some(Macro::Function { .. })) {
                index += 1;
                continue;
            }

            if !matches!(
                tokens.get(index + 1).map(|token| &token.kind),
                Some(TokenKind::Punctuator(PunctuatorType::OpenParen))
            ) {
                index += 1;
                continue;
            }

            let Some(next_index) = matching_close_paren_index(tokens, index + 1) else {
                return true;
            };
            index = next_index + 1;
        }

        false
    }

    pub(crate) fn expand_macros(&self, base_tokens: Vec<Token>) -> Vec<Token> {
        let mut current = base_tokens;
        for _ in 0..16 {
            let next = self.expand_macros_once(current.clone());
            if next == current {
                return next;
            }
            current = next;
        }
        current
    }

    fn expand_macros_once(&self, base_tokens: Vec<Token>) -> Vec<Token> {
        let mut expanded = Vec::new();
        let mut index = 0;

        while index < base_tokens.len() {
            let token = &base_tokens[index];
            let TokenKind::Identifier(name) = &token.kind else {
                expanded.push(token.clone());
                index += 1;
                continue;
            };

            let Some(macro_) = self.macros.get(name) else {
                expanded.push(token.clone());
                index += 1;
                continue;
            };

            match macro_ {
                Macro::Object(body) => {
                    expanded.extend(retarget_tokens(body.iter().cloned(), token));
                    index += 1;
                }
                Macro::Function { params, body } => {
                    let Some((mut args, next_index)) = parse_macro_args(&base_tokens, index + 1)
                    else {
                        expanded.push(token.clone());
                        index += 1;
                        continue;
                    };

                    if args.is_empty() && params.len() == 1 {
                        args.push(Vec::new());
                    }

                    if args.len() != params.len() {
                        expanded.push(token.clone());
                        index += 1;
                        continue;
                    }

                    let expanded_args = args
                        .iter()
                        .map(|arg| self.expand_macros(arg.clone()))
                        .collect::<Vec<_>>();

                    let mut body_index = 0;
                    while body_index < body.len() {
                        let body_token = &body[body_index];

                        if is_hash_hash(body, body_index) {
                            body_index += 2;
                            let Some(left) = expanded.pop() else {
                                continue;
                            };
                            let Some(next_token) = body.get(body_index) else {
                                expanded.push(left);
                                continue;
                            };
                            let mut right = replacement_tokens_for_macro_body_token(
                                next_token,
                                params,
                                &args,
                                &expanded_args,
                                false,
                            );
                            if right.is_empty() {
                                expanded.push(left);
                                body_index += 1;
                                continue;
                            }

                            let first_right = right.remove(0);
                            expanded.push(paste_tokens(left, first_right, token));
                            expanded.extend(retarget_tokens(right, token));
                            body_index += 1;
                            continue;
                        }

                        if matches!(
                            &body_token.kind,
                            TokenKind::Punctuator(PunctuatorType::Hash)
                        ) && let Some(next_token) = body.get(body_index + 1)
                            && let TokenKind::Identifier(identifier) = &next_token.kind
                            && let Some(param_index) =
                                params.iter().position(|param| param == identifier)
                        {
                            expanded.extend(retarget_tokens(
                                std::iter::once(Token::new_unknown(TokenKind::StringLiteral(
                                    stringify_macro_arg(&args[param_index]),
                                ))),
                                token,
                            ));
                            body_index += 2;
                            continue;
                        }

                        if let TokenKind::Identifier(identifier) = &body_token.kind
                            && let Some(param_index) =
                                params.iter().position(|param| param == identifier)
                        {
                            let arg_tokens = if is_hash_hash(body, body_index + 1) {
                                args[param_index].clone()
                            } else {
                                expanded_args[param_index].clone()
                            };
                            expanded.extend(retarget_tokens(
                                arg_tokens,
                                token,
                            ));
                            body_index += 1;
                            continue;
                        }

                        expanded
                            .extend(retarget_tokens(std::iter::once(body_token.clone()), token));
                        body_index += 1;
                    }

                    index = next_index;
                }
            }
        }

        expanded
    }

    pub fn push(&mut self, condition: bool) {
        let frame = self.current_frame_mut();

        let parent_active = frame.is_active();
        let branch_active = parent_active && condition;

        frame.conditionals.push(ConditionalFrame {
            parent_active,
            branch_active,
            any_branch_taken: branch_active,
            else_seen: false,
        });
    }

    pub fn skip_tail(&mut self) {
        self.current_frame_mut().with_iter(skip_directive_tail);
    }
}

fn is_hash_hash(tokens: &[Token], index: usize) -> bool {
    matches!(
        (
            tokens.get(index).map(|token| &token.kind),
            tokens.get(index + 1).map(|token| &token.kind)
        ),
        (
            Some(TokenKind::Punctuator(PunctuatorType::Hash)),
            Some(TokenKind::Punctuator(PunctuatorType::Hash))
        )
    )
}

fn replacement_tokens_for_macro_body_token(
    body_token: &Token,
    params: &[String],
    raw_args: &[Vec<Token>],
    expanded_args: &[Vec<Token>],
    expand_arg: bool,
) -> Vec<Token> {
    if let TokenKind::Identifier(identifier) = &body_token.kind
        && let Some(param_index) = params.iter().position(|param| param == identifier)
    {
        return if expand_arg {
            expanded_args[param_index].clone()
        } else {
            raw_args[param_index].clone()
        };
    }

    vec![body_token.clone()]
}

fn paste_tokens(left: Token, right: Token, expansion_site: &Token) -> Token {
    let pasted_text = format!(
        "{}{}",
        token_paste_text(&left.kind),
        token_paste_text(&right.kind)
    );
    let mut token = Token::new_unknown(TokenKind::from_str(pasted_text));
    token.byte_start_index = expansion_site.byte_start_index;
    token.byte_end_index = expansion_site.byte_end_index;
    token.file_origin = expansion_site.file_origin.clone();
    token
}

fn token_paste_text(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Identifier(name) => name.clone(),
        TokenKind::CompilerIdentifier(name) => format!("@{name}"),
        TokenKind::IntLiteral(value) => value.to_string(),
        TokenKind::FloatLiteral(value) => value.to_string(),
        TokenKind::StringLiteral(value) => format!("\"{value}\""),
        _ => kind.to_string(),
    }
}

fn retarget_tokens(
    tokens: impl IntoIterator<Item = Token>,
    expansion_site: &Token,
) -> Vec<Token> {
    tokens
        .into_iter()
        .map(|mut token| {
            token.byte_start_index = expansion_site.byte_start_index;
            token.byte_end_index = expansion_site.byte_end_index;
            token.file_origin = expansion_site.file_origin.clone();
            token
        })
        .collect()
}

fn stringify_macro_arg(tokens: &[Token]) -> String {
    tokens
        .iter()
        .map(|token| token.kind.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

fn matching_close_paren_index(tokens: &[Token], open_paren_index: usize) -> Option<usize> {
    if !matches!(
        tokens.get(open_paren_index).map(|token| &token.kind),
        Some(TokenKind::Punctuator(PunctuatorType::OpenParen))
    ) {
        return None;
    }

    let mut depth = 0usize;
    let mut index = open_paren_index;
    while index < tokens.len() {
        match &tokens[index].kind {
            TokenKind::Punctuator(PunctuatorType::OpenParen) => depth += 1,
            TokenKind::Punctuator(PunctuatorType::CloseParen) => {
                depth -= 1;
                if depth == 0 {
                    return Some(index);
                }
            }
            _ => {}
        }

        index += 1;
    }

    None
}

fn parse_macro_args(
    tokens: &[Token],
    open_paren_index: usize,
) -> Option<(Vec<Vec<Token>>, usize)> {
    if !matches!(
        tokens.get(open_paren_index).map(|token| &token.kind),
        Some(TokenKind::Punctuator(PunctuatorType::OpenParen))
    ) {
        return None;
    }

    let mut args = Vec::new();
    let mut current = Vec::new();
    let mut depth = 0usize;
    let mut index = open_paren_index + 1;

    while index < tokens.len() {
        let token = &tokens[index];
        match &token.kind {
            TokenKind::Punctuator(PunctuatorType::OpenParen) => {
                depth += 1;
                current.push(token.clone());
            }
            TokenKind::Punctuator(PunctuatorType::CloseParen) => {
                if depth == 0 {
                    if !current.is_empty() || !args.is_empty() {
                        args.push(current);
                    }
                    return Some((args, index + 1));
                }

                depth -= 1;
                current.push(token.clone());
            }
            TokenKind::Operator(cx_tokens::token::OperatorType::Comma) if depth == 0 => {
                args.push(current);
                current = Vec::new();
            }
            _ => current.push(token.clone()),
        }

        index += 1;
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use cx_tokens::token::OperatorType;

    fn token(kind: TokenKind) -> Token {
        Token::new_unknown(kind)
    }

    fn ident(name: &str) -> Token {
        token(TokenKind::Identifier(name.to_string()))
    }

    fn punctuator(punctuator: PunctuatorType) -> Token {
        token(TokenKind::Punctuator(punctuator))
    }

    fn comma() -> Token {
        token(TokenKind::Operator(OperatorType::Comma))
    }

    fn test_context() -> LexingContext {
        match LexingContext::new(String::new(), Path::new("test.cx"), &[]) {
            Ok(context) => context,
            Err(error) => panic!("{}", error.error_message()),
        }
    }

    #[test]
    fn function_macro_stringifies_raw_arg_and_expands_normal_arg() {
        let mut context = test_context();
        context.macros.insert(
            "A".to_string(),
            Macro::Object(Box::new([ident("B")])),
        );
        context.macros.insert(
            "F".to_string(),
            Macro::Function {
                params: Box::new(["x".to_string()]),
                body: Box::new([
                    punctuator(PunctuatorType::Hash),
                    ident("x"),
                    ident("x"),
                ]),
            },
        );

        let expanded = context.expand_macros(vec![
            ident("F"),
            punctuator(PunctuatorType::OpenParen),
            ident("A"),
            punctuator(PunctuatorType::CloseParen),
        ]);

        assert!(matches!(
            expanded.as_slice(),
            [
                Token {
                    kind: TokenKind::StringLiteral(raw),
                    ..
                },
                Token {
                    kind: TokenKind::Identifier(expanded),
                    ..
                },
            ] if raw == "A" && expanded == "B"
        ));
    }

    #[test]
    fn function_macro_pastes_argument_into_identifier() {
        let mut context = test_context();
        context.macros.insert(
            "__GLIBC_USE".to_string(),
            Macro::Function {
                params: Box::new(["F".to_string()]),
                body: Box::new([
                    ident("__GLIBC_USE_"),
                    punctuator(PunctuatorType::Hash),
                    punctuator(PunctuatorType::Hash),
                    ident("F"),
                ]),
            },
        );
        context.macros.insert(
            "__GLIBC_USE_ISOC23".to_string(),
            Macro::Object(Box::new([token(TokenKind::IntLiteral(1))])),
        );

        let expanded = context.expand_macros(vec![
            ident("__GLIBC_USE"),
            punctuator(PunctuatorType::OpenParen),
            ident("ISOC23"),
            punctuator(PunctuatorType::CloseParen),
        ]);

        assert!(matches!(
            expanded.as_slice(),
            [Token {
                kind: TokenKind::IntLiteral(1),
                ..
            }]
        ));
    }

    #[test]
    fn buffers_multiline_function_macro_invocations_until_args_close() {
        let mut context = test_context();
        context.macros.insert(
            "REDIRECT".to_string(),
            Macro::Function {
                params: Box::new(["name".to_string(), "proto".to_string(), "alias".to_string()]),
                body: Box::new([
                    ident("name"),
                    ident("proto"),
                    ident("__asm__"),
                    punctuator(PunctuatorType::OpenParen),
                    punctuator(PunctuatorType::Hash),
                    ident("alias"),
                    punctuator(PunctuatorType::CloseParen),
                ]),
            },
        );

        context.emit_tokens(vec![
            ident("REDIRECT"),
            punctuator(PunctuatorType::OpenParen),
            ident("fscanf"),
            comma(),
            punctuator(PunctuatorType::OpenParen),
            ident("FILE"),
        ]);
        assert!(context.tokens.is_empty());

        context.emit_tokens(vec![
            punctuator(PunctuatorType::CloseParen),
            comma(),
            ident("__isoc99_fscanf"),
            punctuator(PunctuatorType::CloseParen),
        ]);

        assert!(context.tokens.iter().all(
            |token| !matches!(&token.kind, TokenKind::Identifier(name) if name == "REDIRECT")
        ));
        assert!(context
            .tokens
            .iter()
            .any(|token| matches!(&token.kind, TokenKind::Identifier(name) if name == "fscanf")));
        assert!(context.tokens.iter().any(
            |token| matches!(&token.kind, TokenKind::StringLiteral(value) if value == "__isoc99_fscanf")
        ));
    }
}
