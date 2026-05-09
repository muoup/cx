use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
};

use cx_tokens::token::{PunctuatorType, Token, TokenKind};
use cx_util::CXResult;

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
    tokens: Vec<Token>,
}

impl LexingContext {
    pub(crate) fn new(source: String, source_path: &Path, include_dirs: &[PathBuf]) -> Self {
        Self {
            include_dirs: include_dirs.to_vec(),
            macros: builtin_macros(),
            once_files: HashSet::new(),
            sources: vec![SourceFrame::new(source, source_path)],
            tokens: Vec::new(),
        }
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
        self.tokens.extend(self.expand_macros(tokens));
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

    fn finish_current_source(&self) -> CXResult<()> {
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

        Ok(())
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

fn retarget_tokens(tokens: impl IntoIterator<Item = Token>, expansion_site: &Token) -> Vec<Token> {
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

fn parse_macro_args(tokens: &[Token], open_paren_index: usize) -> Option<(Vec<Vec<Token>>, usize)> {
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
