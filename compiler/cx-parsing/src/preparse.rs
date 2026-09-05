use crate::{
    assert_token_matches, log::parse_point_error, next_kind, parse::try_parse_qualified_name,
    try_next,
};
use cx_log::CXResult;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_pipeline_data::CompilerConfig;
use cx_preparse_data::{Import, PreparseContents};
use cx_tokens::{
    identifier, keyword, operator, punctuator, specifier, token::TokenKind, TokenIter,
};
use cx_util::{
    identifier::CXIdent,
};

#[derive(Debug, Clone, Copy)]
pub struct PreparseConfig {
    pub module_mode: bool,
}

impl PreparseConfig {
    pub fn from_compiler_config(config: &CompilerConfig) -> Self {
        Self {
            module_mode: config.module_mode,
        }
    }
}

pub(crate) struct PreparseData<'a> {
    #[allow(dead_code)]
    pub(crate) config: &'a PreparseConfig,
    pub(crate) contents: &'a mut PreparseContents,
    pub(crate) tokens: TokenIter<'a>,
    pub(crate) visibility_mode: cx_preparse_data::VisibilityMode,
    pub(crate) include_states: Vec<PreparseIncludeState>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct PreparseIncludeState {
    visibility: cx_preparse_data::VisibilityMode,
}

pub(crate) fn iterate_tokens(data: &mut PreparseData) -> CXResult<()> {
    while data.tokens.has_next() {
        consume_token(data)?;
    }

    Ok(())
}

fn consume_token(data: &mut PreparseData) -> CXResult<()> {
    let Some(next_token) = data.tokens.next() else {
        return Ok(());
    };
    let next_kind = next_token.kind.clone();

    match &next_kind {
        cx_tokens::token::TokenKind::IncludeBegin => {
            data.include_states.push(PreparseIncludeState {
                visibility: data.visibility_mode,
            });
        }

        cx_tokens::token::TokenKind::IncludeEnd => {
            let Some(state) = data.include_states.pop() else {
                return parse_point_error(&data.tokens, "Unexpected end of included source");
            };
            data.visibility_mode = state.visibility;
        }

        keyword!(Struct) | keyword!(Union) | keyword!(Enum) => {
            let Some(identifier!(ident)) = next_kind!(data.tokens).ok() else {
                data.tokens.back();
                return Ok(());
            };
            let ident = CXIdent::new(ident.as_str());
            let visibility = data.visibility_mode;

            data.contents.module_symbols.add_tag(ident, visibility);
        }

        keyword!(Typedef) => {
            let mut last_ident = None;
            let mut depth = 0usize;
            let mut pointer_declarator = false;

            while let Some(token) = data.tokens.next() {
                match &token.kind {
                    punctuator!(OpenBrace) | punctuator!(OpenParen) | punctuator!(OpenBracket) => {
                        depth += 1
                    }
                    punctuator!(CloseBrace)
                    | punctuator!(CloseParen)
                    | punctuator!(CloseBracket) => depth = depth.saturating_sub(1),
                    punctuator!(Semicolon) if depth == 0 => break,
                    operator!(Asterisk) if depth == 1 => pointer_declarator = true,
                    identifier!(ident) if depth == 0 => {
                        last_ident = Some(CXIdent::new(ident.as_str()))
                    }
                    identifier!(ident) if depth == 1 && pointer_declarator => {
                        last_ident = Some(CXIdent::new(ident.as_str()));
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
            let import = parse_import(&mut data.tokens)?;

            for name in &import.names {
                let import_namespace = name.namespace.clone().child(name.name.clone());

                if import_namespace == data.contents.module_symbols.namespace {
                    return parse_point_error(
                        &data.tokens,
                        format!("Cannot import current module '{}'", name),
                    );
                }

                if let Some(alias) = &import.alias {
                    data.contents
                        .add_namespace_alias(alias.clone(), import_namespace);
                }
            }

            data.contents.imports.push(import);
        }

        specifier!(Public) if is_extern_c_section_after_access(data) => {
            parse_extern_c_mod(data, cx_preparse_data::VisibilityMode::Public)?;
        }

        specifier!(Private) if is_extern_c_section_after_access(data) => {
            parse_extern_c_mod(data, cx_preparse_data::VisibilityMode::Private)?;
        }

        specifier!(Public) => {
            data.visibility_mode = cx_preparse_data::VisibilityMode::Public;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Private) => {
            data.visibility_mode = cx_preparse_data::VisibilityMode::Private;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Extern) if is_extern_c_section(data) => {
            data.tokens.back();
            parse_extern_c_mod(data, cx_preparse_data::VisibilityMode::Private)?;
        }

        _ => (),
    }

    Ok(())
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
            Some(cx_tokens::token::TokenKind::Specifier(
                cx_tokens::token::SpecifierType::Extern
            )),
            Some(cx_tokens::token::TokenKind::StringLiteral(abi))
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
            Some(cx_tokens::token::TokenKind::Specifier(
                cx_tokens::token::SpecifierType::Extern
            )),
            Some(cx_tokens::token::TokenKind::StringLiteral(abi))
        ) if abi == "C"
    )
}

fn parse_extern_c_mod(
    data: &mut PreparseData,
    visibility: cx_preparse_data::VisibilityMode,
) -> CXResult<()> {
    assert_token_matches!(data.tokens, specifier!(Extern), "'extern'");
    assert_token_matches!(
        data.tokens,
        cx_tokens::token::TokenKind::StringLiteral(abi),
        "\"C\""
    );
    let abi = abi.clone();

    if abi != "C" {
        return parse_point_error(&data.tokens, format!("Unsupported extern ABI '{}'", abi));
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
    prefix: Vec<CXIdent>,
    item: Vec<CXIdent>,
    state: ImportFrameState,
    saw_item: bool,
}

fn parse_import(tokens: &mut TokenIter) -> CXResult<Import> {
    assert_token_matches!(tokens, keyword!(Import), "'import'");

    let names = parse_import_tree(tokens)?;
    let alias = if try_next!(tokens, keyword!(As)) {
        Some(parse_import_alias(tokens)?)
    } else {
        None
    };

    assert_token_matches!(tokens, punctuator!(Semicolon), "';'");

    Ok(Import { names, alias })
}

fn parse_import_tree(tokens: &mut TokenIter) -> CXResult<Vec<QualifiedName>> {
    let mut names = Vec::new();
    let mut frames = vec![ImportFrame {
        prefix: Vec::new(),
        item: Vec::new(),
        state: ImportFrameState::ExpectItem,
        saw_item: false,
    }];

    loop {
        let Some(next_token) = tokens.peek() else {
            return parse_point_error(
                tokens,
                "Reached end of token stream when parsing import!".to_string(),
            );
        };

        if frames.len() == 1
            && matches!(
                &next_token.kind,
                TokenKind::Keyword(cx_tokens::token::KeywordType::As)
                    | TokenKind::Punctuator(cx_tokens::token::PunctuatorType::Semicolon)
            )
        {
            let frame = frames
                .first()
                .expect("import parser should have a root frame");
            match frame.state {
                ImportFrameState::AfterPath => push_import_name(&mut names, frame, tokens),
                ImportFrameState::AfterGroup => Ok(()),
                ImportFrameState::ExpectItem if !frame.saw_item => {
                    parse_point_error(tokens, "Import path cannot be empty")
                }
                ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                    parse_point_error(tokens, "Expected import path item")
                }
            }?;

            return Ok(names);
        }

        let Some(tok) = tokens.next().cloned() else {
            return parse_point_error(
                tokens,
                "Reached end of token stream when parsing import!".to_string(),
            );
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
                    frame.item.push(CXIdent::new(ident.as_str()));
                    frame.state = ImportFrameState::AfterPath;
                    frame.saw_item = true;
                }
                ImportFrameState::AfterPath | ImportFrameState::AfterGroup => {
                    return parse_point_error(
                        tokens,
                        "Expected ',' or '}' after import path item".to_string(),
                    );
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
                prefix.extend(frame.item.drain(..));
                frame.state = ImportFrameState::AfterGroup;

                frames.push(ImportFrame {
                    prefix,
                    item: Vec::new(),
                    state: ImportFrameState::ExpectItem,
                    saw_item: false,
                });
            }

            operator!(Comma) => {
                if is_root {
                    return parse_point_error(
                        tokens,
                        "Top-level import paths must be enclosed in '{' and '}'".to_string(),
                    );
                }

                let frame = frames
                    .last_mut()
                    .expect("import parser should have an active frame");

                match frame_state {
                    ImportFrameState::AfterPath => {
                        push_import_name(&mut names, frame, tokens)?;
                    }
                    ImportFrameState::AfterGroup => {}
                    ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                        return parse_point_error(
                            tokens,
                            "Expected import path item before ','".to_string(),
                        );
                    }
                }

                frame.item.clear();
                frame.state = ImportFrameState::ExpectItem;
            }

            punctuator!(CloseBrace) => {
                if is_root {
                    return parse_point_error(tokens, "Unexpected '}' in import statement");
                }

                let frame = frames
                    .pop()
                    .expect("import parser should have a nested frame");

                match frame_state {
                    ImportFrameState::AfterPath => {
                        push_import_name(&mut names, &frame, tokens)?;
                    }
                    ImportFrameState::AfterGroup => {}
                    ImportFrameState::ExpectItem if frame.saw_item => {}
                    ImportFrameState::ExpectItem | ImportFrameState::ExpectPathContinuation => {
                        return parse_point_error(
                            tokens,
                            "Expected import path item before '}'".to_string(),
                        );
                    }
                }

                let parent = frames
                    .last_mut()
                    .expect("import parser should have a parent frame");
                parent.item.clear();
                parent.state = ImportFrameState::AfterGroup;
            }

            _ => {
                return parse_point_error(
                    tokens,
                    format!("Reached invalid token in import path: {:?}", tok),
                );
            }
        }
    }
}

fn push_import_name(
    names: &mut Vec<QualifiedName>,
    frame: &ImportFrame,
    tokens: &TokenIter,
) -> CXResult<()> {
    if frame.item.is_empty() {
        return parse_point_error(tokens, "Import path cannot be empty");
    }

    let mut segments = frame.prefix.clone();
    segments.extend(frame.item.iter().cloned());
    let name = segments.pop().expect("import path should have a name");
    names.push(QualifiedName::new(NamespacePath::new(segments), name));
    Ok(())
}

fn parse_import_alias(tokens: &mut TokenIter) -> CXResult<NamespacePath> {
    let Some(ident) = try_parse_qualified_name(tokens)? else {
        return parse_point_error(tokens, "Expected identifier for import alias".to_string());
    };

    if ident.namespace.is_root() && ident.name.as_str() == "_" {
        Ok(NamespacePath::root())
    } else {
        Ok(ident.namespace.child(ident.name))
    }
}
