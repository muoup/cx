use cx_ast::{assert_token_matches, next_kind, try_next};
use cx_pipeline_data::CompilerConfig;
use cx_preparse_data::PreparseContents;
use cx_tokens::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_util::{
    identifier::CXIdent, log_error, module_path::ModulePath, namespace::NamespacePath, CXResult,
};

use crate::parse::try_parse_ident;

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

pub fn preparse(
    config: &PreparseConfig,
    tokens: TokenIter,
    module: String,
    path: NamespacePath,
) -> CXResult<PreparseContents> {
    let mut contents = PreparseContents::new(module, path);

    let mut data = PreparseData {
        contents: &mut contents,
        config,
        tokens,
        visibility_mode: cx_preparse_data::VisibilityMode::Private,
    };

    while data.tokens.has_next() {
        iterate_tokens(&mut data)?;
    }

    Ok(contents)
}

fn iterate_tokens(data: &mut PreparseData) -> CXResult<()> {
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

            data.contents
                .module_symbols
                .add_type(CXIdent::new(ident.as_str()), data.visibility_mode);
        }

        keyword!(Typedef) => {
            while !try_next!(data.tokens, punctuator!(Semicolon)) && data.tokens.has_next() {
                data.tokens.next();
            }

            let Some(ident) = try_parse_ident(&mut data.tokens) else {
                return Ok(());
            };

            data.contents.module_symbols.add_type(ident.clone(), data.visibility_mode);
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
