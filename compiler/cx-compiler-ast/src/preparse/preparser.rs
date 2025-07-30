use crate::preparse::typing::{parse_enum, parse_initializer, parse_params, parse_struct, parse_union};
use cx_data_ast::lex::token::TokenKind;
use cx_data_ast::parse::parser::{TokenIter, VisibilityMode};
use cx_data_ast::parse::value_type::CXTypeKind;
use cx_data_ast::{assert_token_matches, keyword, next_kind, operator, peek_next_kind, punctuator, specifier, try_next, PreparseContents};
use cx_data_ast::parse::ast::CXFunctionPrototype;
use cx_data_bytecode::mangling::mangle_destructor;
use cx_util::point_log_error;
use crate::parse::global_scope::parse_global_expr;
use crate::preparse::importing::parse_import;
use crate::preparse::PreparseData;

pub(crate) fn preparse_stmt(data: &mut PreparseData, contents: &mut PreparseContents) -> Option<()> {
    match data.tokens.peek()?.kind {
        keyword!(Import) => parse_import(&mut data.tokens, contents)?,
        keyword!(Struct, Enum, Union) => parse_plain_typedef(data, contents)?,
        keyword!(Typedef) => parse_typedef(&mut data.tokens, contents)?,
        
        specifier!(Public, Private) => {
            data.tokens.next(); // Consume the public specifier
            try_next!(data.tokens, punctuator!(Colon));
        },

        _ => preparse_global_expr(data, contents)?,
    }
    
    Some(())
}

pub(crate) fn parse_plain_typedef(data: &mut PreparseData, contents: &mut PreparseContents) -> Option<()> {
    let starting_index = data.tokens.index;
    
    let (name, type_) = match &data.tokens.peek()?.kind {
        keyword!(Struct) => parse_struct(&mut data.tokens)?,
        keyword!(Enum) => parse_enum(&mut data.tokens)?,
        keyword!(Union) => parse_union(&mut data.tokens)?,

        tok => todo!("parse_plain_typedef: {tok:?}")
    };
    
    if matches!(type_, CXTypeKind::Identifier { .. }) {
        data.tokens.index = starting_index; // Reset to the start if we didn't parse a type declaration
        return preparse_global_expr(data, contents);
    }
    
    try_next!(data.tokens, punctuator!(Semicolon));
    
    let mut full_type = type_.to_val_type();
    full_type.visibility_mode = VisibilityMode::Public; // Default visibility for typedefs is public
    
    contents.type_definitions.insert(name.unwrap().as_string(), full_type);
    Some(())
}

pub(crate) fn parse_typedef(tokens: &mut TokenIter, contents: &mut PreparseContents) -> Option<()> {
    assert_token_matches!(tokens, keyword!(Typedef));
    
    let (Some(name), type_) = parse_initializer(tokens)? else {
        point_log_error!(tokens, "Expected name for typedef expression");    
    };
    
    assert_token_matches!(tokens, punctuator!(Semicolon));
    
    contents.type_definitions.insert(name.data, type_);
    Some(())
}

pub(crate) fn preparse_global_expr(data: &mut PreparseData, contents: &mut PreparseContents) -> Option<()> {
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
            
            contents.function_definitions.insert(signature.name.to_string(), signature);
            goto_statement_end(&mut data.tokens);
        },
        
        _ => {
            eprintln!("Found: {:?}", data.tokens.peek());
            todo!("global variables")
        }
    } 
    
    Some(())
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