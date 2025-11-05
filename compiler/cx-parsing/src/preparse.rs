use cx_lexer_data::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_parsing_data::{
    assert_token_matches,
    ast::VisibilityMode,
    data::{CXLinkageMode, ModuleResource},
    next_kind, peek_kind, PreparseContents,
};
use cx_util::{identifier::CXIdent, log_error, CXError, CXResult};

pub(crate) struct PreparseData<'a> {
    pub(crate) contents: &'a mut PreparseContents,
    pub(crate) tokens: TokenIter<'a>,
    pub(crate) visibility_mode: VisibilityMode,
}

pub fn preparse(tokens: TokenIter) -> CXResult<PreparseContents> {
    let mut contents = PreparseContents::default();

    let mut data = PreparseData {
        contents: &mut contents,
        tokens,
        visibility_mode: VisibilityMode::Private,
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
        keyword!(Struct) | keyword!(Enum) => {
            let Some(identifier!(ident)) = next_kind!(data.tokens).ok() else {
                return Ok(());
            };

            data.contents.type_idents.push(ModuleResource::new(
                CXIdent::from(ident.as_str()),
                data.visibility_mode,
                CXLinkageMode::Standard,
            ));
        }

        keyword!(Union) => {
            if peek_kind!(data.tokens, keyword!(Class)) {
                data.tokens.next();
            };

            let Some(identifier!(ident)) = next_kind!(data.tokens).ok() else {
                return Ok(());
            };

            data.contents.type_idents.push(ModuleResource::new(
                CXIdent::from(ident.as_str()),
                data.visibility_mode,
                CXLinkageMode::Standard,
            ));
        }

        keyword!(Typedef) => {
            while !peek_kind!(data.tokens, punctuator!(Semicolon)) && data.tokens.has_next() {
                data.tokens.next();
            }

            if !peek_kind!(data.tokens, punctuator!(Semicolon)) {
                return Err(CXError::new("Expected semicolon"));
            }

            let Some(identifier!(ident)) = next_kind!(data.tokens).ok() else {
                return Ok(());
            };

            data.contents.type_idents.push(ModuleResource::new(
                CXIdent::from(ident.as_str()),
                data.visibility_mode,
                CXLinkageMode::Standard,
            ));
        }

        keyword!(Import) => {
            data.tokens.back();
            let import_path = parse_import(&mut data.tokens)?;
            data.contents.imports.push(import_path);
        }

        specifier!(Public) => {
            data.visibility_mode = VisibilityMode::Public;
            assert_token_matches!(data.tokens, punctuator!(Colon));
        }

        specifier!(Private) => {
            data.visibility_mode = VisibilityMode::Private;
            assert_token_matches!(data.tokens, punctuator!(Colon));
        }

        _ => (),
    }

    Ok(())
}

fn parse_import(tokens: &mut TokenIter) -> CXResult<String> {
    assert_token_matches!(tokens, keyword!(Import));

    let mut import_path = String::new();

    loop {
        let Some(tok) = tokens.next() else {
            log_preparse_error!(tokens, "Reached end of token stream when parsing import!");
        };

        match &tok.kind {
            punctuator!(Semicolon) => break,
            operator!(ScopeRes) => import_path.push('/'),
            identifier!(ident) => import_path.push_str(ident),

            _ => log_error!("Reached invalid token in import path: {:?}", tok),
        }
    }

    Ok(import_path)
}
