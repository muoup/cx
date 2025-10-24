use cx_lexer_data::{identifier, keyword, operator, punctuator, specifier, TokenIter};
use cx_parsing_data::{assert_token_matches, next_kind, parse::parser::VisibilityMode, peek_kind, preparse::naive_types::ModuleResource, PreparseContents};
use cx_util::{identifier::CXIdent, log_error, CXResult};

use crate::declarations::decl_parsing::preparse_decl_stmt;

pub(crate) struct PreparseData<'a> {
    pub(crate) contents: &'a mut PreparseContents,
    pub(crate) tokens: TokenIter<'a>,
    pub(crate) visibility_mode: VisibilityMode,
}

pub fn preparse(tokens: TokenIter) -> Option<PreparseContents> {
    let mut contents = PreparseContents::default();

    let mut data = PreparseData {
        contents: &mut contents,
        tokens,
        visibility_mode: VisibilityMode::Private,
    };

    while data.tokens.has_next() {
        let Some(stmt) = preparse_decl_stmt(&mut data.tokens) else {
            log_preparse_error!(data.tokens, "Failed to preparse statement")
        };

        stmt.add_to(&mut data);
    }

    Some(contents)
}

fn consume_token(tokens: &mut TokenIter, data: &mut PreparseData) -> CXResult<()> {
    match next_kind!(tokens)? {
        keyword!(Struct) | keyword!(Enum) => {
            let Some(identifier!(ident)) = next_kind!(tokens) else {
                return Some(());
            };

            data.contents
                .type_idents
                .push(ModuleResource::with_visibility(
                    CXIdent::from(ident.as_str()),
                    data.visibility_mode,
                ));
        },
        
        keyword!(Union) => {
            if peek_kind!(tokens, keyword!(Class)) {
                tokens.next();
            };
            
            let Some(identifier!(ident)) = next_kind!(tokens) else {
                return Some(());
            };
            
            data.contents
                .type_idents
                .push(ModuleResource::with_visibility(
                    CXIdent::from(ident.as_str()),
                    data.visibility_mode,
                ));
        },
        
        keyword!(Typedef) => {
            while !peek_kind!(tokens, punctuator!(Semicolon)) && tokens.has_next() {
                tokens.next();
            }
          
            if !peek_kind!(tokens, punctuator!(Semicolon)) {
                return None;
            }
            
            let Some(identifier!(ident)) = next_kind!(tokens) else {
                return Some(());
            };
            
            data.contents
                .type_idents
                .push(ModuleResource::with_visibility(
                    CXIdent::from(ident.as_str()),
                    data.visibility_mode,
                ));
        },
        
        keyword!(Import) => {
            tokens.back();
            let import_path = parse_import(tokens)?;
            data.contents.imports.push(import_path);
        },
        
        specifier!(Public) => {
            data.visibility_mode = VisibilityMode::Public;
            assert_token_matches!(tokens, punctuator!(Colon));
        },
        
        specifier!(Private) => {
            data.visibility_mode = VisibilityMode::Private;
            assert_token_matches!(tokens, punctuator!(Colon));
        },

        _ => (),
    }
    
    Some(())
}

pub(crate) fn iterate_tokens(tokens: &mut TokenIter, data: &mut PreparseData) -> CXResult<()> {
    while tokens.has_next() {
        consume_token(tokens, data)?;
    }

    Some(())
}

pub(crate) fn parse_import(tokens: &mut TokenIter) -> CXResult<String> {
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

    Some(import_path)
}