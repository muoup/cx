use crate::{assert_token_matches, next_kind};
use cx_pipeline_data::CompilerConfig;
use cx_preparse_data::{symbol_data::PreparseModuleSymbols, PreparseContents};
use cx_tokens::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_util::{identifier::CXIdent, log_error, module_path::ModulePath, CXResult};

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
}

impl PreparseData<'_> {
    fn current_symbols_mut(&mut self) -> &mut PreparseModuleSymbols {
        let Some(token) = self.tokens.prev() else {
            return &mut self.contents.module_symbols;
        };

        if token.file_origin.as_ref() == self.tokens.file.as_path() {
            &mut self.contents.module_symbols
        } else {
            &mut self.contents.root_symbols
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

    match &next_token.kind {
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
            let import_path = parse_import(&mut data.tokens)?;
            data.contents.imports.push(import_path);
        }

        specifier!(Public) => {
            data.visibility_mode = cx_preparse_data::VisibilityMode::Public;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        specifier!(Private) => {
            data.visibility_mode = cx_preparse_data::VisibilityMode::Private;
            assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
        }

        _ => (),
    }

    Ok(())
}

fn parse_import(tokens: &mut TokenIter) -> CXResult<ModulePath> {
    assert_token_matches!(tokens, keyword!(Import), "'import'");

    let mut import_path = String::new();

    loop {
        let Some(tok) = tokens.next() else {
            return log_preparse_error!(tokens, "Reached end of token stream when parsing import!");
        };

        match &tok.kind {
            punctuator!(Semicolon) => break,
            operator!(ScopeRes) => import_path.push('/'),
            identifier!(ident) => import_path.push_str(ident),

            _ => log_error!("Reached invalid token in import path: {:?}", tok),
        }
    }

    Ok(ModulePath::new(import_path))
}
