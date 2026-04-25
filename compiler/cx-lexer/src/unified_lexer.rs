use crate::line_lexer::lex_line;
use crate::preprocessor::{generate_lexable_slice, handle_comment, handle_directive};
use cx_tokens::token::{SpecifierType, Token, TokenKind};
use cx_util::CXResult;
use cx_util::char_iter::CharIter;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub(crate) enum Macro {
    Object(Box<[Token]>),
    Function {
        params: Box<[String]>,
        body: Box<[Token]>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct ConditionalFrame {
    pub(crate) parent_active: bool,
    pub(crate) branch_active: bool,
    pub(crate) any_branch_taken: bool,
    pub(crate) else_seen: bool,
}

pub(crate) struct Lexer<'a> {
    pub(crate) source: &'a str,
    pub(crate) file_path: PathBuf,
    pub(crate) include_dirs: Vec<PathBuf>,
    pub(crate) char_iter: CharIter<'a>,
    pub(crate) macros: HashMap<String, Macro>,
    pub(crate) once_files: HashSet<PathBuf>,
    pub(crate) conditionals: Vec<ConditionalFrame>,
    pub(crate) tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str, source_path: &Path, include_dirs: &[PathBuf]) -> Self {
        Lexer {
            source,
            file_path: source_path.to_path_buf(),
            include_dirs: include_dirs.to_vec(),
            char_iter: CharIter::new(source),
            macros: builtin_macros(),
            once_files: HashSet::new(),
            conditionals: Vec::new(),
            tokens: Vec::new(),
        }
    }

    pub(crate) fn is_active(&self) -> bool {
        self.conditionals
            .last()
            .map(|frame| frame.branch_active)
            .unwrap_or(true)
    }

    pub(crate) fn lex_source(&mut self) -> CXResult<()> {
        let source = self.file_path.to_string_lossy().to_string();

        while self.char_iter.has_next() {
            if let Some((mut lexable_iter, active)) = self.interpret_directive_line()? {
                if !active {
                    continue;
                }

                let tokens_in_line = lex_line(&mut lexable_iter, source.clone())?;

                self.tokens.extend(self.expand_macros(tokens_in_line));
            } else {
                break;
            }
        }

        if let Some(frame) = self.conditionals.last() {
            return log_lexer_error!(
                self.file_path.as_path(),
                self.source,
                self.char_iter.current_iter,
                self.char_iter.current_iter,
                "Unclosed preprocessor conditional starting in {} branch",
                if frame.parent_active {
                    "active"
                } else {
                    "inactive"
                }
            );
        }

        Ok(())
    }

    pub(crate) fn independent_lex(
        &mut self,
        str: &str,
        source_path: &Path,
    ) -> CXResult<Vec<Token>> {
        let mut sublexer = Lexer::new(str, source_path, &self.include_dirs);
        std::mem::swap(&mut sublexer.macros, &mut self.macros);
        std::mem::swap(&mut sublexer.once_files, &mut self.once_files);
        sublexer.lex_source()?;
        std::mem::swap(&mut sublexer.macros, &mut self.macros);
        std::mem::swap(&mut sublexer.once_files, &mut self.once_files);
        Ok(sublexer.tokens)
    }

    // returns text that the lexer can lex over, i.e. not comments or preprocessor directives
    fn interpret_directive_line(&mut self) -> CXResult<Option<(CharIter<'_>, bool)>> {
        loop {
            self.char_iter.skip_whitespace();

            if !self.char_iter.has_next() {
                return Ok(None);
            }

            match self.char_iter.peek() {
                Some('#') => {
                    handle_directive(self)?;
                    continue;
                }

                Some('/') => {
                    if handle_comment(self) {
                        continue;
                    }
                }

                _ => (),
            }

            let active = self.is_active();
            return Ok(generate_lexable_slice(self).map(|slice| (slice, active)));
        }
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
                    let Some((args, next_index)) = parse_macro_args(&base_tokens, index + 1) else {
                        expanded.push(token.clone());
                        index += 1;
                        continue;
                    };

                    for body_token in body.iter() {
                        if let TokenKind::Identifier(identifier) = &body_token.kind
                            && let Some(param_index) =
                                params.iter().position(|param| param == identifier)
                        {
                            expanded.extend(retarget_tokens(args[param_index].clone(), token));
                            continue;
                        }

                        expanded
                            .extend(retarget_tokens(std::iter::once(body_token.clone()), token));
                    }

                    index = next_index;
                }
            }
        }

        expanded
    }
}

fn builtin_macros() -> HashMap<String, Macro> {
    let mut macros = HashMap::new();

    for name in ["__restrict", "__restrict__", "__extension__"] {
        macros.insert(name.to_string(), Macro::Object(Box::new([])));
    }

    macros.insert(
        "__const".to_string(),
        Macro::Object(Box::new([Token {
            kind: TokenKind::Specifier(SpecifierType::Const),
            line: 0,
            start_index: 0,
            end_index: 0,
            file_origin: "".into(),
        }])),
    );

    macros.insert(
        "__attribute__".to_string(),
        Macro::Function {
            params: Box::new(["x".to_string()]),
            body: Box::new([]),
        },
    );

    macros
}

fn retarget_tokens(tokens: impl IntoIterator<Item = Token>, expansion_site: &Token) -> Vec<Token> {
    tokens
        .into_iter()
        .map(|mut t| {
            t.start_index = expansion_site.start_index;
            t.end_index = expansion_site.end_index;
            t.line = expansion_site.line;
            t.file_origin = expansion_site.file_origin.clone();
            t
        })
        .collect()
}

fn parse_macro_args(tokens: &[Token], open_paren_index: usize) -> Option<(Vec<Vec<Token>>, usize)> {
    if !matches!(
        tokens.get(open_paren_index).map(|token| &token.kind),
        Some(TokenKind::Punctuator(
            cx_tokens::token::PunctuatorType::OpenParen
        ))
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
            TokenKind::Punctuator(cx_tokens::token::PunctuatorType::OpenParen) => {
                depth += 1;
                current.push(token.clone());
            }
            TokenKind::Punctuator(cx_tokens::token::PunctuatorType::CloseParen) => {
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
