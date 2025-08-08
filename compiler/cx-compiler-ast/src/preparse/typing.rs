use cx_data_ast::{assert_token_matches, next_kind, peek_next, peek_next_kind, try_next};
use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXParameter};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::template::CXTemplateInput;
use cx_data_ast::parse::value_type::{CXTypeKind, CXType};
use cx_data_ast::preparse::pp_type::{CXNaiveParameter, CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind, CXTypeSpecifier, PredeclarationType, CX_CONST, CX_RESTRICT, CX_VOLATILE};
use cx_data_lexer::{keyword, punctuator, TokenIter};
use cx_util::{log_error, point_log_error, CXResult};
use crate::parse::typing::parse_contextualized_initializer;
use crate::preparse::preparser::{goto_statement_end, parse_intrinsic, parse_std_ident};

fn predeclaration_identifier(name: Option<CXIdent>, predeclaration: PredeclarationType) -> Option<(Option<CXIdent>, CXNaiveTypeKind)> {
    Some((name.clone(), CXNaiveTypeKind::Identifier { name: name.unwrap(), predeclaration: predeclaration }))
}

pub(crate) fn parse_struct(tokens: &mut TokenIter) -> CXResult<(Option<CXIdent>, CXNaiveTypeKind)> {
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

    Some((name.clone(), CXNaiveTypeKind::Structured { name, fields }))
}

pub(crate) fn parse_enum(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXNaiveTypeKind)> {
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
    Some((name, CXNaiveTypeKind::Identifier { name: CXIdent::from("int"), predeclaration: PredeclarationType::None }))
}

pub(crate) fn parse_union(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXNaiveTypeKind)> {
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
        CXNaiveTypeKind::Union {
            name,
            fields,
        }
    ))
}

pub(crate) struct ParseParamsResult {
    pub(crate) params: Vec<CXNaiveParameter>,
    pub(crate) var_args: bool,
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

            params.push(CXNaiveParameter { name, _type: type_ });
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
            SpecifierType::Restrict => spec_acc |= CX_RESTRICT,

            _ => break
        }
    }

    tokens.back();
    spec_acc
}

pub(crate) fn parse_typemods(tokens: &mut TokenIter, acc_type: CXNaiveType) -> Option<(Option<CXIdent>, CXNaiveType)> {
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
            let acc_type = CXNaiveType::new(
                specs,
                CXNaiveTypeKind::StrongPointer {
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
            let acc_type = CXNaiveType::new(
                specs,
                CXNaiveTypeKind::PointerTo {
                    inner_type: Box::new(acc_type),
                    weak: true,
                }
            );

            parse_typemods(tokens, acc_type)
        },

        TokenKind::Operator(OperatorType::Asterisk) => {
            tokens.next();
            let specs = parse_specifier(tokens);
            let acc_type = acc_type.pointer_to(false, specs);

            parse_typemods(tokens, acc_type)
        },

        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            tokens.next();
            assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Asterisk));
            let name = parse_std_ident(tokens);
            assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            let ParseParamsResult { params, var_args } = parse_params(tokens)?;

            let prototype = CXNaivePrototype {
                name: CXIdent::from("INTERNAL_FUNCTION_PTR_TYPE"),
                return_type: acc_type,
                params: params,
                var_args
            };

            Some((
                name,
                CXNaiveTypeKind::FunctionPointer { prototype: Box::new(prototype) }
                    .to_type()
                    .pointer_to(false, 0)
            ))
        },

        TokenKind::Identifier(_) => Some((Some(parse_std_ident(tokens)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

pub(crate) fn parse_suffix_typemod(tokens: &mut TokenIter, acc_type: CXNaiveType) -> Option<CXNaiveType> {
    let Some(next_tok) = tokens.peek() else {
        return Some(acc_type);
    };

    match &next_tok.kind {
        TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
            tokens.next();

            let _type = match tokens.peek()?.kind {
                TokenKind::Punctuator(PunctuatorType::CloseBracket) => {
                    CXNaiveTypeKind::ImplicitSizedArray(Box::new(acc_type))
                        .to_type()
                },
                TokenKind::IntLiteral(size) => {
                    tokens.next();
                    CXNaiveTypeKind::ExplicitSizedArray(Box::new(acc_type), size as usize)
                        .to_type()
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

pub(crate) fn parse_template_args(tokens: &mut TokenIter) -> Option<CXNaiveTemplateInput> {
    assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Less));

    let mut input_types = Vec::new();

    loop {
        let Some((None, _type)) = parse_initializer(tokens) else {
            point_log_error!(tokens, "PARSER ERROR: Expected type declaration in template arguments!");
        };

        input_types.push(_type);

        if !try_next!(tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Greater));

    Some(CXNaiveTemplateInput { params: input_types })
}

pub(crate) fn parse_type_base(tokens: &mut TokenIter) -> Option<CXNaiveType> {
    let _type = match tokens.peek()?.kind {
        TokenKind::Identifier(_) => {
            let ident = parse_std_ident(tokens)?;
            
            if peek_next!(tokens, TokenKind::Operator(OperatorType::Less)) {
                let params = parse_template_args(tokens)?;
             
                Some(
                    CXNaiveTypeKind::TemplatedIdentifier {
                        name: ident,
                        input: params,
                    }.to_type()
                )
            } else {
                Some(
                    CXNaiveTypeKind::Identifier {
                        name: ident,
                        predeclaration: PredeclarationType::None
                    }.to_type()
                )
            }
        },

        TokenKind::Intrinsic(_) => Some(
            CXNaiveTypeKind::Identifier {
                name: parse_intrinsic(tokens)?,
                predeclaration: PredeclarationType::None
            }.to_type()
        ),

        TokenKind::Keyword(KeywordType::Struct) => Some(
            parse_struct(tokens)?.1
                .to_type()
        ),

        TokenKind::Keyword(KeywordType::Enum) => Some(
            parse_enum(tokens)?.1
                .to_type()
        ),

        TokenKind::Keyword(KeywordType::Union) => Some(
            parse_union(tokens)?.1
                .to_type()
        ),

        _ => return None
    };

    let specifiers = parse_specifier(tokens);

    Some(_type?.add_specifier(specifiers))
}

pub(crate) fn parse_base_mods(tokens: &mut TokenIter, acc_type: CXNaiveType) -> Option<(Option<CXIdent>, CXNaiveType)> {
    let (name, modified_type) = parse_typemods(tokens, acc_type)?;

    Some((name, parse_suffix_typemod(tokens, modified_type)?))
}

pub(crate) fn parse_initializer(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXNaiveType)> {
    let prefix_specs = parse_specifier(tokens);
    let type_base = parse_type_base(tokens)?;

    parse_base_mods(tokens, type_base.add_specifier(prefix_specs))
}