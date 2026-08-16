use crate::{
    assert_token_matches, log::parse_point_error, next_kind, parse::try_parse_qualified_name,
};
use cx_log::CXResult;
use cx_pipeline_data::CompilerConfig;
use cx_preparse_data::PreparseContents;
use cx_tokens::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_util::{identifier::CXIdent, module_path::ModulePath, namespace::NamespacePath};

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
            let ParsedImport { path, alias } = parse_import(&mut data.tokens)?;
            let import_namespace = NamespacePath::from(path.clone());

            if import_namespace == data.contents.module_symbols.namespace {
                return parse_point_error(
                    &data.tokens,
                    format!("Cannot import current module '{}'", path),
                );
            }

            if let Some(alias) = alias {
                data.contents
                    .add_namespace_alias(alias, import_namespace.clone());
            }

            let import_path = path;
            data.contents.imports.push(import_path);
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

struct ParsedImport {
    path: ModulePath,
    alias: Option<NamespacePath>,
}

fn parse_import(tokens: &mut TokenIter) -> CXResult<ParsedImport> {
    assert_token_matches!(tokens, keyword!(Import), "'import'");

    let mut import_path = String::new();
    let mut alias = None;

    loop {
        let Some(tok) = tokens.next().cloned() else {
            return parse_point_error(
                tokens,
                "Reached end of token stream when parsing import!".to_string(),
            );
        };

        match &tok.kind {
            punctuator!(Semicolon) => break,
            keyword!(As) => {
                alias = Some(parse_import_alias(tokens)?);
                assert_token_matches!(tokens, punctuator!(Semicolon), "';'");
                break;
            }
            operator!(ScopeRes) => import_path.push('/'),
            identifier!(ident) => import_path.push_str(ident),

            _ => {
                return parse_point_error(
                    tokens,
                    format!("Reached invalid token in import path: {:?}", tok),
                );
            }
        }
    }

    if import_path.is_empty() {
        return parse_point_error(tokens, "Import path cannot be empty".to_string());
    }

    Ok(ParsedImport {
        path: ModulePath::new(import_path),
        alias,
    })
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
