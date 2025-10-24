use cx_parsing_data::preparse::naive_types::{
    CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, CXTypeSpecifier, PredeclarationType, CX_CONST,
    CX_RESTRICT, CX_VOLATILE,
};
use cx_parsing_data::preparse::NaiveFnIdent;
use cx_parsing_data::{assert_token_matches, next_kind, peek_kind, try_next};
use cx_lexer_data::token::{OperatorType, PunctuatorType, SpecifierType, TokenKind};
use cx_lexer_data::{identifier, intrinsic, keyword, operator, punctuator, TokenIter};
use cx_util::identifier::CXIdent;
use cx_util::{log_error, point_log_error, CXResult};

use crate::declarations::data_parsing::{
    parse_intrinsic, parse_std_ident, parse_template_args, try_parse_template,
};
use crate::declarations::function_parsing::{parse_params, ParseParamsResult};
use crate::declarations::TypeDeclaration;

fn predeclaration_identifier(
    name: Option<CXIdent>,
    predeclaration: PredeclarationType,
) -> Option<TypeDeclaration> {
    Some(TypeDeclaration {
        name: name.clone(),
        template_prototype: None,
        type_: CXNaiveType::new(
            0,
            CXNaiveTypeKind::Identifier {
                name: name.unwrap(),
                predeclaration,
            },
        ),
    })
}

pub(crate) fn parse_struct_def(tokens: &mut TokenIter) -> CXResult<TypeDeclaration> {
    assert_token_matches!(tokens, keyword!(Struct));

    let name = parse_std_ident(tokens);
    let template_prototype = try_parse_template(tokens);

    if !try_next!(tokens, punctuator!(OpenBrace)) {
        return predeclaration_identifier(name, PredeclarationType::Struct);
    }

    let mut fields = Vec::new();

    while !try_next!(tokens, punctuator!(CloseBrace)) {
        let Some((name, _type)) = parse_initializer(tokens) else {
            log_preparse_error!(tokens, "Failed to parse struct member type");
        };

        let Some(name) = name else {
            log_preparse_error!(
                tokens,
                "UNSUPPORTED: Nameless struct member of type {}",
                _type
            );
        };

        fields.push((name.to_string(), _type));
        assert_token_matches!(tokens, punctuator!(Semicolon));
    }

    Some(TypeDeclaration {
        name: name.clone(),
        template_prototype: template_prototype.clone(),
        type_: CXNaiveType::new(
            0,
            CXNaiveTypeKind::Structured {
                name: name.clone(),
                fields: fields.clone(),
            },
        ),
    })
}

pub(crate) fn parse_enum_def(tokens: &mut TokenIter) -> Option<TypeDeclaration> {
    assert_token_matches!(tokens, keyword!(Enum));

    let name = parse_std_ident(tokens);

    if !try_next!(tokens, punctuator!(OpenBrace)) {
        return predeclaration_identifier(name, PredeclarationType::Enum);
    }

    tokens.back();
    tokens.goto_statement_end()?;
    tokens.back();

    Some(TypeDeclaration {
        name: name.clone(),
        template_prototype: None,
        type_: CXNaiveType::new(
            0,
            CXNaiveTypeKind::Identifier {
                name: CXIdent::from("int"),
                predeclaration: PredeclarationType::None,
            },
        ),
    })
}

pub(crate) fn parse_tagged_union_def(tokens: &mut TokenIter) -> Option<TypeDeclaration> {
    assert_token_matches!(tokens, keyword!(Union));
    assert_token_matches!(tokens, keyword!(Class));

    let name = parse_std_ident(tokens)?;
    let template_prototype = try_parse_template(tokens);

    assert_token_matches!(tokens, punctuator!(OpenBrace));

    let mut variants = Vec::new();

    loop {
        let Some(name) = parse_std_ident(tokens) else {
            point_log_error!(tokens, "Expected variant name in tagged union");
        };

        assert_token_matches!(tokens, operator!(ScopeRes));

        match parse_initializer(tokens) {
            // Success Path = Valid Type + No Name
            Some((None, _type)) => variants.push((name.to_string(), _type)),

            Some((Some(_), _)) => {
                log_preparse_error!(tokens, "Tagged union variant may not have a named type")
            }
            None => log_preparse_error!(tokens, "Failed to parse tagged union variant type"),
        }

        if !try_next!(tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, punctuator!(CloseBrace));

    Some(TypeDeclaration {
        name: Some(name.clone()),
        template_prototype,
        type_: CXNaiveType::new(0, CXNaiveTypeKind::TaggedUnion { name, variants }),
    })
}

pub(crate) fn parse_union_def(tokens: &mut TokenIter) -> Option<TypeDeclaration> {
    assert_token_matches!(tokens, keyword!(Union));

    if peek_kind!(tokens, keyword!(Class)) {
        tokens.back();
        return parse_tagged_union_def(tokens);
    }

    let name = parse_std_ident(tokens);
    let template_prototype = try_parse_template(tokens);

    if !try_next!(tokens, punctuator!(OpenBrace)) {
        return predeclaration_identifier(name, PredeclarationType::Union);
    }

    let mut fields = Vec::new();

    loop {
        let (name, _type) = parse_initializer(tokens)?;

        let Some(name) = name else {
            log_preparse_error!(
                tokens,
                "UNSUPPORTED: Nameless union member of type {}",
                _type
            );
        };

        fields.push((name.to_string(), _type));

        if !try_next!(tokens, punctuator!(Semicolon)) {
            break;
        }
    }

    assert_token_matches!(tokens, punctuator!(CloseBrace));

    Some(TypeDeclaration {
        name: name.clone(),
        type_: CXNaiveType::new(
            0,
            CXNaiveTypeKind::Union {
                name: name.clone(),
                fields,
            },
        ),
        template_prototype,
    })
}

pub(crate) fn parse_specifier(tokens: &mut TokenIter) -> CXTypeSpecifier {
    let mut spec_acc: CXTypeSpecifier = 0;

    while let Some(TokenKind::Specifier(spec)) = next_kind!(tokens) {
        match spec {
            SpecifierType::Const => spec_acc |= CX_CONST,
            SpecifierType::Volatile => spec_acc |= CX_VOLATILE,
            SpecifierType::Restrict => spec_acc |= CX_RESTRICT,

            _ => break,
        }
    }

    tokens.back();
    spec_acc
}

pub(crate) fn parse_typemods(
    tokens: &mut TokenIter,
    acc_type: CXNaiveType,
) -> Option<(Option<CXIdent>, CXNaiveType)> {
    let Some(next_tok) = tokens.peek() else {
        return Some((None, acc_type));
    };
    let start_index = tokens.index;

    match &next_tok.kind {
        keyword!(Strong) => {
            tokens.next();

            let is_array = match tokens.next()?.kind {
                TokenKind::Operator(OperatorType::Asterisk) => false,
                TokenKind::Punctuator(PunctuatorType::OpenBracket) => {
                    assert_token_matches!(
                        tokens,
                        TokenKind::Punctuator(PunctuatorType::CloseBracket)
                    );
                    true
                }

                _ => log_error!("Expected '*' or '[]' after 'strong' keyword"),
            };

            let specs = parse_specifier(tokens);
            let acc_type = CXNaiveType::new(
                specs,
                CXNaiveTypeKind::StrongPointer {
                    inner: Box::new(acc_type),
                    is_array,
                },
            );

            parse_typemods(tokens, acc_type)
        }

        keyword!(Weak) => {
            tokens.next();
            assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Asterisk));

            let specs = parse_specifier(tokens);
            let acc_type = CXNaiveType::new(
                specs,
                CXNaiveTypeKind::PointerTo {
                    inner_type: Box::new(acc_type),
                    weak: true,
                },
            );

            parse_typemods(tokens, acc_type)
        }

        operator!(Asterisk) => {
            tokens.next();
            let specs = parse_specifier(tokens);
            let acc_type = acc_type.pointer_to(false, specs);

            parse_typemods(tokens, acc_type)
        }

        punctuator!(OpenParen) => {
            tokens.next();
            if next_kind!(tokens) != Some(operator!(Asterisk)) {
                tokens.index = start_index;
                return Some((None, acc_type));
            }
            let name = parse_std_ident(tokens);
            assert_token_matches!(tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));
            let ParseParamsResult {
                params,
                var_args,
                contains_this,
            } = parse_params(tokens)?;

            let prototype = CXNaivePrototype {
                name: NaiveFnIdent::Standard(CXIdent::from("__internal_fnptr")),
                return_type: acc_type,
                params,
                var_args,
                this_param: contains_this,
            };

            Some((
                name,
                CXNaiveTypeKind::FunctionPointer {
                    prototype: Box::new(prototype),
                }
                .to_type()
                .pointer_to(false, 0),
            ))
        }

        identifier!() => Some((Some(parse_std_ident(tokens)?), acc_type)),

        _ => Some((None, acc_type)),
    }
}

pub(crate) fn parse_suffix_typemod(
    tokens: &mut TokenIter,
    acc_type: CXNaiveType,
) -> Option<CXNaiveType> {
    let Some(next_tok) = tokens.peek() else {
        return Some(acc_type);
    };

    match &next_tok.kind {
        punctuator!(OpenBracket) => {
            tokens.next();

            let _type = match tokens.peek()?.kind {
                punctuator!(CloseBracket) => {
                    CXNaiveTypeKind::ImplicitSizedArray(Box::new(acc_type)).to_type()
                }
                TokenKind::IntLiteral(size) => {
                    tokens.next();
                    CXNaiveTypeKind::ExplicitSizedArray(Box::new(acc_type), size as usize).to_type()
                }

                // TODO: reimplement variable length arrays
                _ => todo!("variable length arrays"),
            };

            assert_token_matches!(tokens, punctuator!(CloseBracket));

            Some(_type)
        }

        _ => Some(acc_type),
    }
}

pub(crate) fn parse_type_base(tokens: &mut TokenIter) -> Option<CXNaiveType> {
    let _type = match tokens.peek()?.kind {
        identifier!() => {
            let ident = parse_std_ident(tokens)?;

            if peek_kind!(tokens, operator!(Less)) {
                let params = parse_template_args(tokens)?;

                Some(
                    CXNaiveTypeKind::TemplatedIdentifier {
                        name: ident,
                        input: params,
                    }
                    .to_type(),
                )
            } else {
                Some(
                    CXNaiveTypeKind::Identifier {
                        name: ident,
                        predeclaration: PredeclarationType::None,
                    }
                    .to_type(),
                )
            }
        }

        intrinsic!() => Some(
            CXNaiveTypeKind::Identifier {
                name: parse_intrinsic(tokens)?,
                predeclaration: PredeclarationType::None,
            }
            .to_type(),
        ),

        keyword!(Struct) => Some(parse_struct_def(tokens)?.type_),
        keyword!(Enum) => Some(parse_enum_def(tokens)?.type_),
        keyword!(Union) => Some(parse_union_def(tokens)?.type_),

        _ => return None,
    };

    let specifiers = parse_specifier(tokens);

    Some(_type?.add_specifier(specifiers))
}

pub(crate) fn parse_base_mods(
    tokens: &mut TokenIter,
    acc_type: CXNaiveType,
) -> Option<(Option<CXIdent>, CXNaiveType)> {
    let (name, modified_type) = parse_typemods(tokens, acc_type)?;

    Some((name, parse_suffix_typemod(tokens, modified_type)?))
}

pub(crate) fn parse_initializer(tokens: &mut TokenIter) -> Option<(Option<CXIdent>, CXNaiveType)> {
    let prefix_specs = parse_specifier(tokens);
    let type_base = parse_type_base(tokens)?;

    parse_base_mods(tokens, type_base.add_specifier(prefix_specs))
}
