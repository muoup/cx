use cx_data_ast::{assert_token_matches, next_kind, peek_next, preparse::{naive_types::{CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, ModuleResource, PredeclarationType}, CXNaiveFnIdent}};
use cx_data_lexer::{identifier, keyword, operator, punctuator, token::TokenKind, TokenIter};
use cx_util::{identifier::CXIdent, log_error, CXResult};

use crate::{declarations::type_parsing::{parse_enum, parse_initializer, parse_params, parse_struct, parse_template_args, parse_union, TypeDeclaration}, preparse::{preparse_global_expr, PreparseData}};

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

            _ => log_error!("Reached invalid token in import path: {:?}", tok)
        }
    };
    
    Some(import_path)
}

pub(crate) fn parse_plain_typedef(data: &mut PreparseData) -> CXResult<()> {
    let starting_index = data.tokens.index;
    
    let Some(decl_start) = next_kind!(data.tokens) else {
        panic!("EOF Reached")
    };
    
    let decl = match decl_start {
        keyword!(Struct) => parse_struct(&mut data.tokens)?,
        keyword!(Enum) => parse_enum(&mut data.tokens)?,
        keyword!(Union) => parse_union(&mut data.tokens)?,

        tok => todo!("parse_plain_typedef: {tok:?}")
    };

    if let TypeDeclaration::Standard { name: Some(name), type_ } = decl {
        if matches!(type_.kind, CXNaiveTypeKind::Identifier { predeclaration, ..  } 
            if predeclaration != PredeclarationType::None) {
            
            // If we reach some `struct [name]` `enum [name]` or `union [name]`
            // that is actually used either to declare a variable or a function,
            // we can't quite differentiate it from a non-typedef declaration
            // until we realize it isn't one, so we have to backtrack and try parsing
            // it as a full expression.
                
            data.tokens.index = starting_index;
            return preparse_global_expr(data);
        }      
    }
    
    assert_token_matches!(data.tokens, punctuator!(Semicolon));
    
    Some(())
}

pub(crate) fn parse_typedef(data: &mut PreparseData) -> CXResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef));
    let start_index = data.tokens.index;

    let Some((name, type_)) = parse_initializer(&mut data.tokens) else {
        log_preparse_error!(data.tokens.with_index(start_index), "Could not parse typedef.");
    };

    let Some(name) = name else {
        log_preparse_error!(data.tokens.with_index(start_index), "Typedef must have a name!");
    };
    
    assert_token_matches!(data.tokens, punctuator!(Semicolon));
    data.contents.type_definitions.insert_standard(name.as_string(), ModuleResource::with_visibility(type_, data.visibility_mode));
    
    Some(())
}

pub fn try_function_parse(tokens: &mut TokenIter, return_type: CXNaiveType, name: CXIdent) -> CXResult<Option<CXNaivePrototype>> {
    match tokens.peek()?.kind {
        // e.g:
        // int main()
        //         ^
        punctuator!(OpenParen) => {
            let Some(args) = parse_params(tokens) else {
                log_preparse_error!(tokens, "Failed to parse parameters in function declaration!");
            };
            
            Some(
                Some(
                    CXNaivePrototype {
                        return_type,
                        name: CXNaiveFnIdent::Standard(name),
                        params: args.params,
                        var_args: args.var_args,
                        this_param: args.contains_this
                    }
                )
            )
        },

        // e.g:
        // void vec<int>::push()
        //         ^
        operator!(Less) => {
            let Some(params) = parse_template_args(tokens) else {
                return Some(None);
            };
            
            let _type = name;

            assert_token_matches!(tokens, operator!(ScopeRes));
            assert_token_matches!(tokens, identifier!(name));
            let fn_name = CXIdent::from(name.as_str());
            
            if !peek_next!(tokens, punctuator!(OpenParen)) {
                log_preparse_error!(tokens, "Expected '(' after template arguments in function declaration!");
            };

            let Some(args) = parse_params(tokens) else {
                log_preparse_error!(tokens, "Failed to parse parameters in function declaration!");
            };
            
            let name = CXNaiveFnIdent::MemberFunction {
                _type: CXNaiveTypeKind::TemplatedIdentifier {
                    name: CXIdent::from(_type.as_str()),
                    input: params
                }.to_type(), 
                function_name: fn_name
            };

            Some(
                Some(
                    CXNaivePrototype {
                        return_type, name,
                        params: args.params,
                        var_args: args.var_args,
                        this_param: args.contains_this
                    }
                )
            )
        }

        // e.g:
        // void renderer::draw()
        //              ^
        operator!(ScopeRes) => {
            tokens.next();
            let _type = name.as_string();
            assert_token_matches!(tokens, TokenKind::Identifier(name));
            let name = CXNaiveFnIdent::MemberFunction {
                _type: CXNaiveTypeKind::Identifier {
                    name: CXIdent::from(_type.as_str()),
                    predeclaration: PredeclarationType::None
                }.to_type(),
                function_name: CXIdent::from(name.as_str())
            };
            let Some(params) = parse_params(tokens) else {
                log_preparse_error!(tokens, "Failed to parse parameters in member function declaration!");
            };

            Some(
                Some(
                    CXNaivePrototype {
                        return_type, name,
                        params: params.params,
                        var_args: params.var_args,
                        this_param: params.contains_this
                    }
                )
            )
        },

        _ => Some(None)
    }
}