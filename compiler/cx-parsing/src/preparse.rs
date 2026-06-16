use crate::{assert_token_matches, next_kind, parse::try_parse_qualified_name};
use cx_log::{log_error, CXResult};
use cx_pipeline_data::CompilerConfig;
use cx_preparse_data::{symbol_data::PreparseModuleSymbols, PreparseContents};
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
    pub(crate) extern_c_mode: bool,
}

impl PreparseData<'_> {
    fn current_symbols_mut(&mut self) -> &mut PreparseModuleSymbols {
        if self.extern_c_mode {
            &mut self.contents.root_symbols
        } else {
            &mut self.contents.module_symbols
        }
    }
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
        keyword!(Struct) | keyword!(Union) | keyword!(Enum) => {
            let Some(identifier!(ident)) = next_kind!(data.tokens).ok() else {
                data.tokens.back();
                return Ok(());
            };
            let ident = CXIdent::new(ident.as_str());
            let visibility = data.visibility_mode;

            data.current_symbols_mut().add_type(ident, visibility);
        }

        keyword!(Typedef) => {
            let mut last_ident = None;
            let mut depth = 0usize;

            while let Some(token) = data.tokens.next() {
                match &token.kind {
                    punctuator!(OpenBrace) | punctuator!(OpenParen) | punctuator!(OpenBracket) => {
                        depth += 1
                    }
                    punctuator!(CloseBrace)
                    | punctuator!(CloseParen)
                    | punctuator!(CloseBracket) => depth = depth.saturating_sub(1),
                    punctuator!(Semicolon) if depth == 0 => break,
                    identifier!(ident) if depth == 0 => {
                        last_ident = Some(CXIdent::new(ident.as_str()))
                    }
                    _ => {}
                }
            }

            let Some(ident) = last_ident else {
                return Ok(());
            };

            let visibility = data.visibility_mode;
            data.current_symbols_mut().add_type(ident, visibility);
        }

        keyword!(Import) => {
            data.tokens.back();
            let ParsedImport { path, alias } = parse_import(&mut data.tokens)?;
            let import_namespace = NamespacePath::from(path.clone());

            if import_namespace == data.contents.module_symbols.namespace {
                return log_preparse_error!(data.tokens, "Cannot import current module '{}'", path);
            }

            if let Some(alias) = alias {
                data.contents
                    .add_namespace_alias(alias, import_namespace.clone());
            }

            let import_path = path;
            data.contents.imports.push(import_path);
        }

        specifier!(Public) => {
            data.visibility_mode = cx_preparse_data::VisibilityMode::Public;
            data.extern_c_mode = false;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Private) => {
            data.visibility_mode = cx_preparse_data::VisibilityMode::Private;
            data.extern_c_mode = false;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Extern) if is_extern_c_section(data) => {
            data.tokens.back();
            parse_extern_c_mod(data)?;
        }

        _ => (),
    }

    Ok(())
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

fn parse_extern_c_mod(data: &mut PreparseData) -> CXResult<()> {
    assert_token_matches!(data.tokens, specifier!(Extern), "'extern'");
    assert_token_matches!(
        data.tokens,
        cx_tokens::token::TokenKind::StringLiteral(abi),
        "\"C\""
    );

    if abi != "C" {
        return log_preparse_error!(data.tokens, "Unsupported extern ABI '{}'", abi);
    }

    assert_token_matches!(data.tokens, punctuator!(Colon), "':'");

    data.visibility_mode = cx_preparse_data::VisibilityMode::Private;
    data.extern_c_mode = true;

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
        let Some(tok) = tokens.next() else {
            return log_preparse_error!(tokens, "Reached end of token stream when parsing import!");
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

            _ => log_error!("Reached invalid token in import path: {:?}", tok),
        }
    }

    if import_path.is_empty() {
        return log_preparse_error!(tokens, "Import path cannot be empty");
    }

    Ok(ParsedImport {
        path: ModulePath::new(import_path),
        alias,
    })
}

fn parse_import_alias(tokens: &mut TokenIter) -> CXResult<NamespacePath> {
    let Some(ident) = try_parse_qualified_name(tokens)? else {
        return log_preparse_error!(
            tokens,
            "Expected identifier for import alias"
        );
    };

    if ident.namespace.is_root() && ident.name.as_str() == "_" {
        Ok(NamespacePath::root())
    } else {
        Ok(ident.namespace.child(ident.name))
    }
}
