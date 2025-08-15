use cx_data_ast::{assert_token_matches, peek_next, try_next};
use cx_data_ast::parse::{CXFunctionIdentifier, CXObjectIdentifier};
use crate::preparse::typing::{parse_enum, parse_initializer, parse_params, parse_struct, parse_template_args, parse_union};
use cx_data_lexer::token::{OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::macros::error_pointer;
use cx_data_ast::parse::parser::{VisibilityMode};
use cx_data_ast::preparse::pp_type::{CXFunctionTemplate, CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, CXTypeTemplate, PredeclarationType};
use cx_data_lexer::{keyword, operator, punctuator, specifier, TokenIter};
use cx_util::{log_error, point_log_error, CXResult};
use crate::preparse::importing::parse_import;
use crate::preparse::PreparseData;

pub(crate) enum PreparseResult {
    TypeDefinition(String, CXNaiveType),
    FunctionDefinition(CXNaivePrototype),
    TypeTemplate(CXTypeTemplate),
    FunctionTemplate(CXFunctionTemplate),
    DestructorDefinition(String),
    
    Import(String),

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
        
        punctuator!(Semicolon) => {
            data.tokens.next();
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
            Some(
                PreparseResult::FunctionTemplate(
                    CXFunctionTemplate {
                        inputs: type_decls,
                        shell: signature
                    }
                )
            )
        },
        
        PreparseResult::TypeDefinition(name, type_) => {
            Some(
                PreparseResult::TypeTemplate(
                    CXTypeTemplate {
                        name: CXIdent::from(name),
                        inputs: type_decls,
                        shell: type_
                    }
                )
            )
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

    if matches!(type_, CXNaiveTypeKind::Identifier { predeclaration, .. } 
        if predeclaration != PredeclarationType::None) {
        data.tokens.index = starting_index; // Reset to the start if we didn't parse a type declaration
        return preparse_global_expr(data);
    }
    
    assert_token_matches!(data.tokens, punctuator!(Semicolon));
    
    Some(
        PreparseResult::TypeDefinition(
            name.unwrap().as_string(), 
            type_.to_type()
        )
    )
}

pub(crate) fn parse_typedef(tokens: &mut TokenIter) -> Option<PreparseResult> {
    assert_token_matches!(tokens, keyword!(Typedef));
    
    let (Some(name), type_) = parse_initializer(tokens)? else {
        point_log_error!(tokens, "Expected name for typedef expression");    
    };
    
    assert_token_matches!(tokens, punctuator!(Semicolon));
    
    Some(
        PreparseResult::TypeDefinition (
            name.to_string(),
            type_
        )
    )
}

pub fn try_function_parse(tokens: &mut TokenIter, return_type: CXNaiveType, name: CXIdent) -> CXResult<Option<CXNaivePrototype>> {
    match tokens.peek()?.kind {
        // e.g:
        // int main()
        //         ^
        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            let Some(args) = parse_params(tokens) else {
                point_log_error!(tokens, "PARSER ERROR: Failed to parse parameters in function declaration!");
            };
            
            Some(
                Some(
                    CXNaivePrototype {
                        return_type,
                        name: CXFunctionIdentifier::Standard(name),
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
        TokenKind::Operator(OperatorType::Less) => {
            let Some(params) = parse_template_args(tokens) else {
                return Some(None);
            };
            
            let _type = name;

            assert_token_matches!(tokens, TokenKind::Operator(OperatorType::ScopeRes));
            assert_token_matches!(tokens, TokenKind::Identifier(name));
            let fn_name = CXIdent::from(name.as_str());
            
            if !peek_next!(tokens, TokenKind::Punctuator(PunctuatorType::OpenParen)) {
                point_log_error!(tokens, "PARSER ERROR: Expected '(' after template arguments in function declaration!");
            };

            let Some(args) = parse_params(tokens) else {
                point_log_error!(tokens, "PARSER ERROR: Failed to parse parameters in function declaration!");
            };
            let name = CXFunctionIdentifier::MemberFunction {
                object: CXObjectIdentifier::Templated {
                    name: CXIdent::from(_type.as_str()),
                    template_input: params
                }, 
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
        TokenKind::Operator(OperatorType::ScopeRes) => {
            tokens.next();
            let _type = name.as_string();
            assert_token_matches!(tokens, TokenKind::Identifier(name));
            let name = CXFunctionIdentifier::MemberFunction {
                object: CXObjectIdentifier::Standard(CXIdent::from(_type.as_str())),
                function_name: CXIdent::from(name.as_str())
            };
            let Some(params) = parse_params(tokens) else {
                point_log_error!(tokens, "PARSER ERROR: Failed to parse parameters in member function declaration!");
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

pub(crate) fn preparse_global_expr(data: &mut PreparseData) -> Option<PreparseResult> {
    let (Some(name), return_type) = parse_initializer(&mut data.tokens)? else {
        point_log_error!(data.tokens, "Invalid global expression, name not found!");
    };
    
    if let Some(method) = try_function_parse(&mut data.tokens, return_type, name.clone())? {
        goto_statement_end(&mut data.tokens);
        
        return Some(PreparseResult::FunctionDefinition(method))
    };
    
    eprintln!("Found: {:?}", data.tokens.peek());
    todo!("global variables")
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
        CXIdent::from(ss)
    )
}

pub fn parse_std_ident(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };

    tokens.next();

    Some(CXIdent::from(ident))
}
