use cx_data_ast::{assert_token_matches, keyword, next_kind, peek_next, peek_next_kind, punctuator, try_next};
use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXParameter};
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use cx_data_ast::parse::parser::TokenIter;
use cx_data_ast::parse::value_type::{CXTypeSpecifier, CXTypeKind, CXType, CX_CONST, CX_VOLATILE, PredeclarationType};
use cx_util::{log_error, point_log_error};
use crate::parse::global_scope::ParseParamsResult;
use crate::parse::template::parse_template_args;
use crate::preparse::preparser::goto_statement_end;

fn predeclaration_identifier(name: Option<CXIdent>, predeclaration: PredeclarationType) -> Option<(Option<CXIdent>, CXTypeKind)> {
    Some((name.clone(), CXTypeKind::Identifier { name: name.unwrap(), predeclaration }))
}

pub(crate) fn parse_struct(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXTypeKind)> {
    assert_token_matches!(tokens, keyword!(Struct));

    let name = parse_std_ident(tokens);

    if !try_next!(tokens, punctuator!(OpenBrace)) {
        return predeclaration_identifier(name, PredeclarationType::Struct);
    }

    let mut fields = Vec::new();

    while !try_next!(tokens, punctuator!(CloseBrace)) {
        let Some((name, _type)) = parse_initializer(tokens) else {
            point_log_error!(tokens, "PARSER ERROR: Failed to parse struct member type");
        };

        let Some(name) = name else {
            point_log_error!(tokens, "UNSUPPORTED: Nameless struct member of type {}", _type);
        };

        fields.push((name.data, _type));
        assert_token_matches!(tokens, punctuator!(Semicolon));
    }

    Some((name.clone(), CXTypeKind::Structured { name, fields, has_destructor: false }))
}

pub(crate) fn parse_enum(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXTypeKind)> {
    assert_token_matches!(tokens, keyword!(Enum));
    
    let name = if let Some(TokenKind::Identifier(name)) = peek_next_kind!(tokens) {
        let ident = CXIdent::from(name.as_str());
        tokens.next();
        Some(ident)
    } else {
        None
    };
    
    if !try_next!(tokens, punctuator!(OpenBrace)) {
        return predeclaration_identifier(name, PredeclarationType::Enum);
    }

    tokens.back();
    goto_statement_end(tokens);
    Some((name, CXTypeKind::Integer { signed: true, bytes: 4 }))
}

pub(crate) fn parse_union(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXTypeKind)> {
    assert_token_matches!(tokens, keyword!(Union));

    let name = parse_std_ident(tokens);

    if !try_next!(tokens, punctuator!(OpenBrace)) {
        return predeclaration_identifier(name, PredeclarationType::Union);
    }

    let mut fields = Vec::new();

    while !try_next!(tokens, punctuator!(CloseBrace)) {
        let (name, _type) = parse_initializer(tokens)?;

        let Some(name) = name else {
            point_log_error!(tokens, "UNSUPPORTED: Nameless union member of type {}", _type);
        };

        fields.push((name.data, _type));
        assert_token_matches!(tokens, punctuator!(Semicolon));
    }

    Some((
        name.clone(),
        CXTypeKind::Union {
            name,
            fields,
        }
    ))
}

pub(crate) fn parse_params(tokens: &mut TokenIter) -> Option<ParseParamsResult> {
    assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));

    let mut params = Vec::new();

    while !try_next!(tokens, TokenKind::Punctuator(PunctuatorType::CloseParen)) {
        if try_next!(tokens, TokenKind::Punctuator(PunctuatorType::Ellipsis)) {
            assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            return Some(ParseParamsResult { params, var_args: true });
        }

        if let Some((name, type_)) = parse_initializer(tokens) {
            let name = name;

            params.push(CXParameter { name, _type: type_ });
        } else {
            point_log_error!(tokens, "Failed to parse parameter in function call");
        }

        if !try_next!(tokens, TokenKind::Operator(OperatorType::Comma)) { assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            break;
        }
    }

    Some(ParseParamsResult { params, var_args: false })
}

pub(crate) fn parse_specifier(tokens: &mut TokenIter) -> CXTypeSpecifier {
    let mut spec_acc = 0;

    while let Some(TokenKind::Specifier(spec)) = next_kind!(tokens) {
        match spec {
            SpecifierType::Const => spec_acc |= CX_CONST,
            SpecifierType::Volatile => spec_acc |= CX_VOLATILE,
            SpecifierType::Restrict => spec_acc |= CX_VOLATILE,

            _ => break
        }
    }

    tokens.back();
    spec_acc
}

pub(crate) fn parse_typemods(tokens: &mut TokenIter, acc_type: CXType) -> Option<(Option<CXIdent>, CXType)> {
    let Some(next_tok) = tokens.peek() else {
        return Some((None, acc_type));
    };

    match &next_tok.kind {
        TokenKind::Keyword(KeywordType::Strong) => {
            tokens.next();

            let is_array = match tokens.next()?.kind {
                TokenKind::Operator(OperatorType::Asterisk) => false,
                TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
                    assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseBracket));
                    true
                },

                _ => log_error!("PARSER ERROR: Expected '*' or '[]' after 'strong' keyword")
            };

            let specs = parse_specifier(tokens);
            let acc_type = CXType::new(
                specs,
                CXTypeKind::StrongPointer {
                    inner: Box::new(acc_type),
                    is_array
                }
            );

            parse_typemods(tokens, acc_type)
        },

        TokenKind::Keyword(KeywordType::Weak) => {
            tokens.next();
            assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Asterisk));

            let specs = parse_specifier(tokens);
            let acc_type = CXType::new(
                specs,
                CXTypeKind::PointerTo {
                    inner: Box::new(acc_type),

                    sizeless_array: false,
                    explicitly_weak: true,
                    nullable: true
                }
            );

            parse_typemods(tokens, acc_type)
        },

        TokenKind::Operator(OperatorType::Asterisk) => {
            tokens.next();
            let specs = parse_specifier(tokens);
            let acc_type = acc_type.pointer_to().add_specifier(specs);

            parse_typemods(tokens, acc_type)
        },

        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            tokens.next();
            assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Asterisk));
            let name = parse_std_ident(tokens);
            assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            let ParseParamsResult { params, var_args } = parse_params(tokens)?;

            let prototype = CXFunctionPrototype {
                name: CXIdent::from("INTERNAL_FUNCTION_PTR_TYPE"),
                return_type: acc_type,
                params,
                var_args
            };

            Some((
                name,
                CXTypeKind::Function { prototype: Box::new(prototype) }.to_val_type().pointer_to()
            ))
        },

        TokenKind::Identifier(_) => Some((Some(parse_std_ident(tokens)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

pub(crate) fn parse_suffix_typemod(tokens: &mut TokenIter, acc_type: CXType) -> Option<CXType> {
    let Some(next_tok) = tokens.peek() else {
        return Some(acc_type);
    };

    match &next_tok.kind {
        TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
            tokens.next();

            let _type = match tokens.peek()?.kind {
                TokenKind::Punctuator(PunctuatorType::CloseBracket) => {
                    CXTypeKind::PointerTo {
                        inner: Box::new(acc_type),

                        sizeless_array: true,
                        nullable: true,
                        explicitly_weak: false,
                    }.to_val_type()
                },
                TokenKind::IntLiteral(size) => {
                    tokens.next();
                    CXTypeKind::Array { _type: Box::new(acc_type), size: size as usize }.to_val_type()
                },

                // TODO: reimplement variable length arrays
                _ => todo!("variable length arrays")
            };

            assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseBracket));

            Some(_type)
        },

        _ => Some(acc_type),
    }
}

pub(crate) fn parse_type_base(tokens: &mut TokenIter) -> Option<CXType> {
    let _type = match tokens.peek()?.kind {
        TokenKind::Identifier(_) => {
            let ident = parse_std_ident(tokens)?;
            
            if peek_next!(tokens, TokenKind::Operator(OperatorType::Less)) {
                let params = parse_template_args(tokens)?;
             
                Some(
                    CXTypeKind::TemplatedIdentifier {
                        name: ident,
                        template_input: params,
                    }.to_val_type()
                )
            } else {
                Some(
                    CXTypeKind::Identifier {
                        name: ident,
                        predeclaration: PredeclarationType::None
                    }.to_val_type()
                )
            }
        },

        TokenKind::Intrinsic(_) => Some(
            CXTypeKind::Identifier {
                name: parse_intrinsic(tokens)?,
                predeclaration: PredeclarationType::None
            }.to_val_type()
        ),

        TokenKind::Keyword(KeywordType::Struct) => Some(
            parse_struct(tokens)?.1.to_val_type()
        ),

        TokenKind::Keyword(KeywordType::Enum) => Some(
            parse_enum(tokens)?.1.to_val_type()
        ),

        TokenKind::Keyword(KeywordType::Union) => Some(
            parse_union(tokens)?.1.to_val_type()
        ),

        _ => return None
    };

    let specifiers = parse_specifier(tokens);

    Some(_type?.add_specifier(specifiers))
}

pub(crate) fn parse_base_mods(tokens: &mut TokenIter, acc_type: CXType) -> Option<(Option<CXIdent>, CXType)> {
    let (name, modified_type) = parse_typemods(tokens, acc_type)?;

    Some((name, parse_suffix_typemod(tokens, modified_type)?))
}

pub(crate) fn parse_initializer(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXType)> {
    let prefix_specs = parse_specifier(tokens);
    let type_base = parse_type_base(tokens)?;

    parse_base_mods(tokens, type_base.add_specifier(prefix_specs))
}