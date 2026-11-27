use crate::{
    assert_token_matches,
    log::{parse_point_error, parse_underline_error},
    next_kind, parse::try_parse_qualified_name, try_next,
};
use chl_log::catalogue::parse::*;
use chl_log::CHLResult;
use chl_namespace::module::NamespacePath;
use chl_namespace::scope::ImportScope;
use chl_preparse_data::{Import, ImportPath, PreparseContents};
use chl_tokens::{
    identifier, intrinsic, keyword, operator, punctuator, specifier,
    token::{KeywordType, PunctuatorType, SpecifierType, Token, TokenKind},
    TokenIter, TokenRange,
};
use chl_util::identifier::CHLIdent;

#[derive(Debug, Clone)]
pub struct PreparseConfig {
    pub module_mode: bool,
    pub scope: ImportScope,
}

pub(crate) struct PreparseData<'a> {
    pub(crate) config: &'a PreparseConfig,
    pub(crate) contents: &'a mut PreparseContents,
    pub(crate) tokens: TokenIter<'a>,
    pub(crate) visibility_mode: chl_preparse_data::VisibilityMode,
    pub(crate) include_states: Vec<PreparseIncludeState>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PreparseIncludeState {
    visibility: chl_preparse_data::VisibilityMode,
}

pub(crate) fn iterate_tokens(data: &mut PreparseData) -> CHLResult<()> {
    let mut depth = 0;
    while data.tokens.has_next() {
        consume_token(data, &mut depth)?;
    }

    Ok(())
}

fn consume_token(data: &mut PreparseData, depth: &mut usize) -> CHLResult<()> {
    let Some(next_token) = data.tokens.next() else {
        return Ok(());
    };
    let next_kind = next_token.kind.clone();
    track_nesting(depth, &next_kind);

    match &next_kind {
        TokenKind::IncludeBegin => {
            data.include_states.push(PreparseIncludeState {
                visibility: data.visibility_mode,
            });
        }

        TokenKind::IncludeEnd => {
            let Some(state) = data.include_states.pop() else {
                return parse_point_error(&data.tokens, &UNEXPECTED_END, Some("included source".into()));
            };
            data.visibility_mode = state.visibility;
        }

        keyword!(Struct) | keyword!(Union) | keyword!(Enum) => {
            let Some(identifier!(ident)) = next_kind!(data.tokens).ok() else {
                data.tokens.back();
                return Ok(());
            };
            let ident = CHLIdent::new(ident.as_str());
            let visibility = data.visibility_mode;

            data.contents.module_symbols.add_tag(ident, visibility);
        }

        keyword!(Comptime) if *depth == 0 => {
            if let Some((name, kind, rhs)) = scan_comptime_type(&data.tokens) {
                if let Some(name) = name {
                    let symbols = &mut data.contents.module_symbols;
                    match kind {
                        ComptimeType::Alias => symbols.add_type(name, data.visibility_mode),
                        ComptimeType::Generator => {
                            symbols.add_generator(name, data.visibility_mode)
                        }
                    }
                }
                if let Some(rhs) = rhs {
                    while data.tokens.index < rhs {
                        consume_token(data, depth)?;
                    }
                    let mut end = data.tokens.with_index(rhs);
                    scan_boundary(&mut end, 0, |kind| matches!(kind, punctuator!(Semicolon)));
                    while data.tokens.index < end.index {
                        let kind = &data.tokens.slice[data.tokens.index].kind;
                        if matches!(kind, TokenKind::IncludeBegin | TokenKind::IncludeEnd)
                            || !data.include_states.is_empty()
                        {
                            consume_token(data, depth)?;
                        } else {
                            track_nesting(depth, kind);
                            data.tokens.index += 1;
                        }
                    }
                }
            }
        }

        keyword!(Typedef) => {
            let mut last_ident = None;
            let mut depth = 0usize;
            let mut pointer_declarator = false;
            let mut after_open_paren = false;

            while let Some(token) = data.tokens.next() {
                let starts_group = std::mem::replace(
                    &mut after_open_paren,
                    matches!(token.kind, punctuator!(OpenParen)),
                );

                track_nesting(&mut depth, &token.kind);
                match &token.kind {
                    punctuator!(Semicolon) if depth == 0 => break,
                    operator!(Asterisk) if depth == 1 && starts_group => pointer_declarator = true,
                    identifier!(ident) if depth == 0 => {
                        last_ident = Some(CHLIdent::new(ident.as_str()))
                    }
                    identifier!(ident) if depth == 1 && pointer_declarator => {
                        last_ident = Some(CHLIdent::new(ident.as_str()));
                        pointer_declarator = false;
                    }
                    _ => {}
                }
            }

            let Some(ident) = last_ident else {
                return Ok(());
            };

            let visibility = data.visibility_mode;
            data.contents.module_symbols.add_type(ident, visibility);
        }

        keyword!(Import) => {
            data.tokens.back();
            let import = parse_import(&mut data.tokens, &data.config.scope)?;
            for path in &import.paths {
                if path.resolved == data.contents.module_symbols.namespace {
                    return parse_underline_error(
                        &CANNOT_IMPORT_CURRENT_MODULE,
                        (path.written.to_string(),),
                        &path.span,
                    );
                }

                if let Some(alias) = &import.alias {
                    data.contents
                        .add_namespace_alias(alias.clone(), path.resolved.clone());
                }
            }

            data.contents.imports.push(import);
        }

        specifier!(Public) if is_extern_c_section_after_access(data) => {
            parse_extern_c_mod(data, chl_preparse_data::VisibilityMode::Public)?;
        }

        specifier!(Private) if is_extern_c_section_after_access(data) => {
            parse_extern_c_mod(data, chl_preparse_data::VisibilityMode::Private)?;
        }

        specifier!(Public) => {
            data.visibility_mode = chl_preparse_data::VisibilityMode::Public;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Private) => {
            data.visibility_mode = chl_preparse_data::VisibilityMode::Private;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Extern) if is_extern_c_section(data) => {
            data.tokens.back();
            parse_extern_c_mod(data, chl_preparse_data::VisibilityMode::Private)?;
        }

        _ => (),
    }

    Ok(())
}

#[derive(Clone, Copy)]
enum ComptimeType {
    Alias,
    Generator,
}

fn scan_comptime_type(
    tokens: &TokenIter,
) -> Option<(Option<CHLIdent>, ComptimeType, Option<usize>)> {
    let mut tokens = tokens.clone();
    let trailing = match &tokens.next()?.kind {
        intrinsic!(Auto) => true,
        keyword!(Type) => false,
        _ => return None,
    };
    let name = try_parse_qualified_name(&mut tokens).ok()??.root_name();
    match &tokens.next()?.kind {
        punctuator!(OpenParen) => {}
        punctuator!(Colon)
            if trailing
                && matches!(&tokens.next()?.kind, keyword!(Type))
                && matches!(&tokens.next()?.kind, TokenKind::Assignment(None)) =>
        {
            return Some((name, ComptimeType::Alias, None));
        }
        TokenKind::Assignment(None) if !trailing => return Some((name, ComptimeType::Alias, None)),
        _ => return None,
    }

    let body_start = |kind: &TokenKind| {
        matches!(
            kind,
            punctuator!(Semicolon)
                | punctuator!(OpenBrace)
                | punctuator!(ThickArrow)
                | operator!(Arrow)
                | TokenKind::Assignment(_)
        )
    };
    let mut delimiter = scan_boundary(&mut tokens, 1, body_start);
    if trailing {
        if !matches!(delimiter, Some(operator!(Arrow)))
            || !matches!(&tokens.next()?.kind, keyword!(Type))
        {
            return None;
        }
        delimiter = scan_boundary(&mut tokens, 0, body_start);
    }
    let rhs = matches!(delimiter, Some(TokenKind::Assignment(None))).then_some(tokens.index);
    Some((name, ComptimeType::Generator, rhs))
}

fn scan_boundary<'a>(
    tokens: &mut TokenIter<'a>,
    mut depth: usize,
    stop: impl Fn(&TokenKind) -> bool,
) -> Option<&'a TokenKind> {
    while let Some(token) = tokens.next() {
        if depth == 0 && stop(&token.kind) {
            return Some(&token.kind);
        }
        track_nesting(&mut depth, &token.kind);
    }
    None
}

fn track_nesting(depth: &mut usize, kind: &TokenKind) {
    match kind {
        punctuator!(OpenParen) | punctuator!(OpenBrace) | punctuator!(OpenBracket) => *depth += 1,
        punctuator!(CloseParen) | punctuator!(CloseBrace) | punctuator!(CloseBracket) => {
            *depth = depth.saturating_sub(1);
        }
        _ => {}
    }
}

fn is_extern_c_section_after_access(data: &PreparseData) -> bool {
    matches!(
        (
            data.tokens
                .slice
                .get(data.tokens.index)
                .map(|token| &token.kind),
            data.tokens
                .slice
                .get(data.tokens.index + 1)
                .map(|token| &token.kind),
        ),
        (
            Some(TokenKind::Specifier(
                SpecifierType::Extern
            )),
            Some(TokenKind::StringLiteral(abi))
        ) if abi == "C"
    )
}

fn is_extern_c_section(data: &PreparseData) -> bool {
    matches!(
        (
            data.tokens.peek_prev().map(|token| &token.kind),
            data.tokens.peek().map(|token| &token.kind),
        ),
        (
            Some(TokenKind::Specifier(
                SpecifierType::Extern
            )),
            Some(TokenKind::StringLiteral(abi))
        ) if abi == "C"
    )
}

fn parse_extern_c_mod(
    data: &mut PreparseData,
    visibility: chl_preparse_data::VisibilityMode,
) -> CHLResult<()> {
    assert_token_matches!(data.tokens, specifier!(Extern), "'extern'");
    assert_token_matches!(
        data.tokens,
        TokenKind::StringLiteral(abi),
        "\"C\""
    );
    let abi = abi.clone();

    if abi != "C" {
        return parse_point_error(&data.tokens, &UNSUPPORTED_FEATURE, (format!("extern ABI '{abi}'"), "the parser".into()));
    }

    assert_token_matches!(data.tokens, punctuator!(Colon), "':'");

    data.visibility_mode = visibility;

    Ok(())
}

#[derive(Debug, Clone, Copy)]
enum ImportFrameState {
    ExpectItem,
    AfterPath,
    ExpectPathContinuation,
    AfterGroup,
}

struct ImportFrame {
    prefix: Vec<CHLIdent>,
    item: Vec<CHLIdent>,
    item_start: usize,
    state: ImportFrameState,
    saw_item: bool,
}

impl ImportFrame {
    fn path(&self, scope: &ImportScope, tokens: &[Token], end: usize) -> ImportPath {
        let mut segments = self.prefix.clone();
        segments.extend(self.item.iter().cloned());
        let written = NamespacePath::new(segments);
        ImportPath {
            resolved: scope.resolve(&written),
            written,
            span: TokenRange::from_tokens(self.item_start, end, tokens),
        }
    }
}

fn parse_import(tokens: &mut TokenIter, scope: &ImportScope) -> CHLResult<Import> {
    assert_token_matches!(tokens, keyword!(Import), "'import'");

    let paths = parse_import_tree(tokens, scope)?;
    let alias = if try_next!(tokens, keyword!(As)) {
        Some(parse_import_alias(tokens)?)
    } else {
        None
    };

    assert_token_matches!(tokens, punctuator!(Semicolon), "';'");

    Ok(Import { paths, alias })
}

fn parse_import_tree(tokens: &mut TokenIter, scope: &ImportScope) -> CHLResult<Vec<ImportPath>> {
    let mut paths = Vec::new();
    let mut frames = vec![ImportFrame {
        prefix: Vec::new(),
        item: Vec::new(),
        item_start: tokens.index,
        state: ImportFrameState::ExpectItem,
        saw_item: false,
    }];

    loop {
        let token_index = tokens.index;
        let Some(next_token) = tokens.peek() else {
            return parse_point_error(tokens, &UNEXPECTED_END, Some("import".into()));
        };

        if frames.len() == 1
            && matches!(
                &next_token.kind,
                TokenKind::Keyword(KeywordType::As)
                    | TokenKind::Punctuator(PunctuatorType::Semicolon)
            )
        {
            let frame = frames
                .first()
                .expect("import parser should have a root frame");
            match frame.state {
                ImportFrameState::AfterPath => {
                    paths.push(frame.path(scope, tokens.slice, token_index));
                }
                ImportFrameState::AfterGroup => {}
                ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                    return parse_point_error(tokens, &EXPECTED_SYNTAX, ("an import path segment".into(), Some("in import path".into())));
                }
            }

            return Ok(paths);
        }

        let Some(tok) = tokens.next().cloned() else {
            return parse_point_error(tokens, &UNEXPECTED_END, Some("import".into()));
        };

        let frame_state = frames
            .last()
            .expect("import parser should have an active frame")
            .state;
        let is_root = frames.len() == 1;

        match &tok.kind {
            identifier!(ident) => match frame_state {
                ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                    let frame = frames
                        .last_mut()
                        .expect("import parser should have an active frame");
                    if frame.item.is_empty() {
                        frame.item_start = token_index;
                    }
                    frame.item.push(CHLIdent::new(ident.as_str()));
                    frame.state = ImportFrameState::AfterPath;
                    frame.saw_item = true;
                }
                ImportFrameState::AfterPath | ImportFrameState::AfterGroup => {
                    return parse_point_error(tokens, &EXPECTED_SYNTAX, ("',' or '}'".into(), Some("after import path item".into())));
                }
            },

            operator!(ScopeRes) if matches!(frame_state, ImportFrameState::AfterPath) => {
                let frame = frames
                    .last_mut()
                    .expect("import parser should have an active frame");
                frame.state = ImportFrameState::ExpectPathContinuation;
            }

            punctuator!(OpenBrace)
                if matches!(frame_state, ImportFrameState::ExpectPathContinuation) =>
            {
                let frame = frames
                    .last_mut()
                    .expect("import parser should have an active frame");
                let mut prefix = frame.prefix.clone();
                prefix.append(&mut frame.item);
                frame.state = ImportFrameState::AfterGroup;

                frames.push(ImportFrame {
                    prefix,
                    item: Vec::new(),
                    item_start: tokens.index,
                    state: ImportFrameState::ExpectItem,
                    saw_item: false,
                });
            }

            operator!(Comma) => {
                if is_root {
                    return parse_point_error(tokens, &REQUIRED_CONTEXT, ("top-level import paths".into(), "'{' and '}'".into()));
                }

                let frame = frames
                    .last_mut()
                    .expect("import parser should have an active frame");

                match frame_state {
                    ImportFrameState::AfterPath => {
                        paths.push(frame.path(scope, tokens.slice, token_index));
                    }
                    ImportFrameState::AfterGroup => {}
                    ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                        return parse_point_error(tokens, &EXPECTED_SYNTAX, ("an import path item".into(), Some("before ','".into())));
                    }
                }

                frame.item.clear();
                frame.state = ImportFrameState::ExpectItem;
            }

            punctuator!(CloseBrace) => {
                if is_root {
                return parse_point_error(tokens, &UNEXPECTED_TOKEN, (Some("}".into()), Some("import".into())));
                }

                let frame = frames
                    .pop()
                    .expect("import parser should have a nested frame");

                match frame_state {
                    ImportFrameState::AfterPath => {
                        paths.push(frame.path(scope, tokens.slice, token_index));
                    }
                    ImportFrameState::AfterGroup => {}
                    ImportFrameState::ExpectItem if frame.saw_item => {}
                    ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                        return parse_point_error(tokens, &EXPECTED_SYNTAX, ("an import path item".into(), Some("before '}'".into())));
                    }
                }

                let parent = frames
                    .last_mut()
                    .expect("import parser should have a parent frame");
                parent.item.clear();
                parent.state = ImportFrameState::AfterGroup;
            }

            _ => {
                return parse_point_error(tokens, &UNEXPECTED_TOKEN, (Some(tok.to_string()), Some("import".into())));
            }
        }
    }
}

fn parse_import_alias(tokens: &mut TokenIter) -> CHLResult<NamespacePath> {
    if try_next!(tokens, keyword!(Hole)) {
        return Ok(NamespacePath::root());
    }
    let Some(ident) = try_parse_qualified_name(tokens)? else {
        return parse_point_error(tokens, &EXPECTED_SYNTAX, ("an import alias".into(), None));
    };

    Ok(ident.namespace.child(ident.name))
}
