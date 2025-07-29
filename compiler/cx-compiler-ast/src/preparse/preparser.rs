use crate::preparse::typing::{parse_enum, parse_initializer, parse_struct, parse_union};
use cx_data_ast::lex::token::TokenKind;
use cx_data_ast::parse::parser::TokenIter;
use cx_data_ast::parse::value_type::CXTypeKind;
use cx_data_ast::{assert_token_matches, keyword, punctuator, try_next, PreparseContents};
use cx_util::point_log_error;
use crate::preparse::importing::parse_import;

pub(crate) fn preparse_stmt(tokens: &mut TokenIter, contents: &mut PreparseContents) -> Option<()> {
    match tokens.peek()?.kind {
        keyword!(Import) => parse_import(tokens, contents)?,
        keyword!(Struct, Enum, Union) => parse_plain_typedef(tokens, contents)?,
        keyword!(Typedef) => parse_typedef(tokens, contents)?,

        _ => goto_statement_end(tokens)?,
    }
    
    Some(())
}

pub(crate) fn parse_plain_typedef(tokens: &mut TokenIter, contents: &mut PreparseContents) -> Option<()> {
    match &tokens.peek()?.kind {
        keyword!(Struct) => {
            let type_ = parse_struct(tokens)?;

            // parse_struct returned some "struct [identifier]", which is a placeholder
            // type that need to be processed by the type parser.
            // alternatively parse_struct returned a nameless struct declaration, which
            // is an effective no-op.
            let CXTypeKind::Structured { name: Some(name), .. } = &type_ else {
                goto_statement_end(tokens);

                // this is janky but returning a None here indicates an error,
                // not that no type needs to be parsed.
                return Some(());
            };

            try_next!(tokens, punctuator!(Semicolon));
            contents.type_definitions.insert(name.to_string(), type_.to_val_type());
        },

        keyword!(Enum) => {
            tokens.next();

            let TokenKind::Identifier(name) = &tokens.peek()?.kind else {
                goto_statement_end(tokens);
                return Some(());
            };

            let name = name.clone();

            tokens.back();

            let type_ = parse_enum(tokens)?;

            try_next!(tokens, punctuator!(Semicolon));

            let name = name.clone();

            contents.type_definitions.insert(name.to_string(), type_.to_val_type());
        },

        keyword!(Union) => {
            let type_ = parse_union(tokens)?;

            let CXTypeKind::Union { name: Some(name), .. } = &type_ else {
                goto_statement_end(tokens);
                return Some(());
            };

            try_next!(tokens, punctuator!(Semicolon));

            contents.type_definitions.insert(name.to_string(), type_.to_val_type());
        },

        tok => todo!("parse_plain_typedef: {tok:?}")
    }

    Some(())
}

pub(crate) fn parse_typedef(tokens: &mut TokenIter, contents: &mut PreparseContents) -> Option<()> {
    assert_token_matches!(tokens, keyword!(Typedef));
    
    let (Some(name), type_) = parse_initializer(tokens)? else {
        point_log_error!(tokens, "Expected name for typedef expression");    
    };
    
    contents.type_definitions.insert(name.data, type_);
    Some(())
}

pub(crate) fn goto_statement_end(tokens: &mut TokenIter) -> Option<()> {
    let mut open_bracket_stack = 0;

    while let Some(token) = tokens.next() {
        match token.kind {
            punctuator!(OpenBrace) => open_bracket_stack += 1,
            punctuator!(CloseBrace) => {
                if open_bracket_stack == 0 {
                    try_next!(tokens, punctuator!(Semicolon));
                    return Some(());
                }
                open_bracket_stack -= 1;
            },
            punctuator!(Semicolon) => if open_bracket_stack == 0 { return Some(()) },
            _ => ()
        }
    }

    Some(())
}