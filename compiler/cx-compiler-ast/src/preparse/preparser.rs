use cx_data_lexer::token::KeywordType;
use cx_data_ast::{assert_token_matches, peek_next_kind, try_next};
use crate::preparse::typing::{parse_enum, parse_initializer, parse_params, parse_struct, parse_union};
use cx_data_lexer::token::TokenKind;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_ast::parse::ast::CXFunctionPrototype;
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_ast::parse::template::CXTemplateTypeGen;
use cx_data_lexer::{keyword, operator, punctuator, specifier, TokenIter};
use cx_util::point_log_error;
use crate::parse::global_scope::{parse_global_expr, parse_global_stmt};
use crate::preparse::importing::parse_import;
use crate::preparse::PreparseData;

pub(crate) enum PreparseResult {
    TypeDefinition(String, CXType),
    FunctionDefinition(CXFunctionPrototype),
    DestructorDefinition(String),
    
    Import(String),
    TemplateDefinition(String, CXTemplateTypeGen),
    
    Nothing
}

pub(crate) fn preparse_stmt(data: &mut PreparseData) -> Option<PreparseResult> {
    match data.tokens.peek()?.kind {
        keyword!(Template) => parse_template(data),
        
        keyword!(Import) => parse_import(&mut data.tokens),
        keyword!(Struct, Enum, Union) => parse_plain_typedef(data),
        keyword!(Typedef) => parse_typedef(&mut data.tokens),
        
        operator!(Tilda) => {
            data.tokens.next();
            assert_token_matches!(data.tokens, TokenKind::Identifier(name));
            let name = name.clone();
            goto_statement_end(&mut data.tokens);
            
            Some(PreparseResult::DestructorDefinition(name))
        },
        
        specifier!(Public) => {
            data.tokens.next();
            data.visibility_mode = VisibilityMode::Public;
            try_next!(data.tokens, punctuator!(Colon));
            
            Some(PreparseResult::Nothing)
        },
        
        specifier!(Private) => {
            data.tokens.next();
            data.visibility_mode = VisibilityMode::Private;
            try_next!(data.tokens, punctuator!(Colon));
            
            Some(PreparseResult::Nothing)
        },

        _ => preparse_global_expr(data),
    }
}

pub(crate) fn parse_template(data: &mut PreparseData) -> Option<PreparseResult> {
    assert_token_matches!(data.tokens, keyword!(Template));
    assert_token_matches!(data.tokens, operator!(Less));
    
    let mut type_decls = Vec::new();
    
    loop {
        assert_token_matches!(data.tokens, TokenKind::Identifier(template_name));
        let template_name = template_name.clone();
        
        assert_token_matches!(data.tokens, punctuator!(Colon));
        assert_token_matches!(data.tokens, keyword!(Type));
    
        type_decls.push(template_name);
        
        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }
    
    assert_token_matches!(data.tokens, operator!(Greater));
    
    let stmt = preparse_stmt(data)
        .expect("PARSER ERROR: Failed to parse global expression in template declaration!");
    
    match stmt {
        PreparseResult::FunctionDefinition(signature) => {
            let template = CXTemplateTypeGen::function_template(type_decls, signature.clone());
            let prototype_name = signature.name.to_string();
            
            Some(PreparseResult::TemplateDefinition(prototype_name, template))
        },
        
        PreparseResult::TypeDefinition(name, type_) => {
            let template = CXTemplateTypeGen::type_template(type_decls, type_);
            
            Some(PreparseResult::TemplateDefinition(name, template))
        },
        
        _ => todo!()
    }
}

pub(crate) fn parse_plain_typedef(data: &mut PreparseData) -> Option<PreparseResult> {
    let starting_index = data.tokens.index;
    
    let (name, type_) = match &data.tokens.peek()?.kind {
        keyword!(Struct) => parse_struct(&mut data.tokens)?,
        keyword!(Enum) => parse_enum(&mut data.tokens)?,
        keyword!(Union) => parse_union(&mut data.tokens)?,

        tok => todo!("parse_plain_typedef: {tok:?}")
    };
    
    if matches!(type_, CXTypeKind::Identifier { .. }) {
        data.tokens.index = starting_index; // Reset to the start if we didn't parse a type declaration
        return preparse_global_expr(data);
    }
    
    try_next!(data.tokens, punctuator!(Semicolon));
    
    Some(
        PreparseResult::TypeDefinition(
            name.unwrap().as_string(), 
            type_.to_val_type()
        )
    )
}

pub(crate) fn parse_typedef(tokens: &mut TokenIter) -> Option<PreparseResult> {
    assert_token_matches!(tokens, keyword!(Typedef));
    
    let (Some(name), mut type_) = parse_initializer(tokens)? else {
        point_log_error!(tokens, "Expected name for typedef expression");    
    };
    
    assert_token_matches!(tokens, punctuator!(Semicolon));
    
    Some(
        PreparseResult::TypeDefinition(
            name.data,
            type_
        )
    )
}

pub(crate) fn preparse_global_expr(data: &mut PreparseData) -> Option<PreparseResult> {
    let (Some(name), return_type) = parse_initializer(&mut data.tokens)? else {
        point_log_error!(data.tokens, "Invalid global expression, name not found!");
    };
    
    match peek_next_kind!(data.tokens)? {
        punctuator!(OpenParen) => {
            // We are parsing a function declaration
            let params = parse_params(&mut data.tokens)?;
            let signature = CXFunctionPrototype {
                name, return_type,
                params: params.params, 
                var_args: params.var_args
            };

            goto_statement_end(&mut data.tokens);
            Some(PreparseResult::FunctionDefinition(signature))
        },
        
        _ => {
            eprintln!("Found: {:?}", data.tokens.peek());
            todo!("global variables")
        }
    } 
}

pub fn goto_statement_end(tokens: &mut TokenIter) -> Option<()> {
    let mut bracket_stack = 0;
    
    while let Some(token) = tokens.next() {
        match token.kind {
            punctuator!(OpenBrace) => bracket_stack += 1,
            punctuator!(CloseBrace) => {
                bracket_stack -= 1;
                
                if bracket_stack == 0 {
                    try_next!(tokens, punctuator!(Semicolon));
                    break;
                }
            },
            punctuator!(Semicolon) if bracket_stack == 0 => { break },
            
            _ => ()
        }
    }

    Some(())
}

pub fn parse_intrinsic(tokens: &mut TokenIter) -> Option<CXIdent> {
    let mut ss = String::new();

    while let Some(TokenKind::Intrinsic(ident)) = tokens.peek().map(|tok| &tok.kind) {
        ss.push_str(format!("{ident:?}").to_lowercase().as_str());
        tokens.next();
    }

    if ss.is_empty() {
        return None;
    }

    Some(
        CXIdent {
            data: ss
        }
    )
}

pub fn parse_std_ident(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };

    tokens.next();

    Some(
        CXIdent {
            data: ident
        }
    )
}
