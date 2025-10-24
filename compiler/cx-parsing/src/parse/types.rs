use cx_lexer_data::token::{OperatorType, PunctuatorType, SpecifierType, TokenKind};
use cx_lexer_data::{identifier, intrinsic, keyword, operator, punctuator, TokenIter};
use cx_parsing_data::parse::parser::ParserData;
use cx_parsing_data::preparse::naive_types::{
    CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, CXTypeSpecifier, PredeclarationType, CX_CONST,
    CX_RESTRICT, CX_VOLATILE,
};
use cx_parsing_data::preparse::templates::CXTemplatePrototype;
use cx_parsing_data::preparse::NaiveFnIdent;
use cx_parsing_data::{assert_token_matches, next_kind, peek_kind, try_next};
use cx_util::identifier::CXIdent;
use cx_util::{log_error, point_log_error, CXResult};

use crate::parse::functions::{parse_params, ParseParamsResult};
use crate::parse::templates::{parse_template_args, try_parse_template};
use crate::parse::{parse_intrinsic, parse_std_ident};

fn predeclaration_type(
    data: &mut ParserData,
    name: Option<CXIdent>,
    predeclaration: PredeclarationType,
) -> CXResult<CXNaiveType> {
    let Some(name) = name else {
        log_parse_error!(data, "Invalid token.");
    };

    Some(
        CXNaiveTypeKind::Identifier {
            name,
            predeclaration,
        }
        .to_type(),
    )
}

fn defined_type(
    data: &mut ParserData,
    name: Option<CXIdent>,
    type_: CXNaiveType,
    template_prototype: Option<CXTemplatePrototype>,
    predeclaration: PredeclarationType,
) -> CXResult<CXNaiveType> {
    if let Some(name) = name {
        // If structure definition has a name, add it to the type map and return
        // the identifier pointer to that type

        data.add_type(name.as_string(), type_, template_prototype.clone());
        return Some(
            CXNaiveTypeKind::Identifier {
                name,
                predeclaration,
            }
            .to_type(),
        );
    } else {
        // If the structure definition is anonymous, it can only be parsed as
        // an in-place type.

        return Some(type_);
    }
}

pub(crate) fn parse_struct_def(data: &mut ParserData) -> CXResult<CXNaiveType> {
    assert_token_matches!(data.tokens, keyword!(Struct));

    let name = parse_std_ident(&mut data.tokens);
    let template_prototype = try_parse_template(&mut data.tokens);

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Struct);
    }

    let mut fields = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let Some((name, _type)) = parse_initializer(data) else {
            log_preparse_error!(data.tokens, "Failed to parse struct member type");
        };

        let Some(name) = name else {
            log_preparse_error!(
                data.tokens,
                "UNSUPPORTED: Nameless struct member of type {}",
                _type
            );
        };

        fields.push((name.to_string(), _type));
        assert_token_matches!(data.tokens, punctuator!(Semicolon));
    }

    defined_type(
        data,
        name.clone(),
        CXNaiveTypeKind::Structured { name, fields }.to_type(),
        template_prototype,
        PredeclarationType::Struct,
    )
}

pub(crate) fn parse_enum_def(data: &mut ParserData) -> Option<CXNaiveType> {
    assert_token_matches!(data.tokens, keyword!(Enum));

    let name = parse_std_ident(&mut data.tokens);

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Enum);
    }

    data.tokens.back();
    data.tokens.goto_statement_end()?;
    data.tokens.back();

    defined_type(
        data,
        name,
        CXNaiveTypeKind::Identifier {
            name: CXIdent::from("int"),
            predeclaration: PredeclarationType::None,
        }
        .to_type(),
        None,
        PredeclarationType::Enum,
    )
}

pub(crate) fn parse_tagged_union_def(data: &mut ParserData) -> Option<CXNaiveType> {
    assert_token_matches!(data.tokens, keyword!(Union));
    assert_token_matches!(data.tokens, keyword!(Class));

    let name = parse_std_ident(&mut data.tokens)?;
    let template_prototype = try_parse_template(&mut data.tokens);

    assert_token_matches!(data.tokens, punctuator!(OpenBrace));

    let mut variants = Vec::new();

    loop {
        let Some(name) = parse_std_ident(&mut data.tokens) else {
            point_log_error!(data.tokens, "Expected variant name in tagged union");
        };

        assert_token_matches!(data.tokens, operator!(ScopeRes));

        match parse_initializer(&mut data.tokens) {
            // Success Path = Valid Type + No Name
            Some((None, _type)) => variants.push((name.to_string(), _type)),

            Some((Some(_), _)) => {
                log_preparse_error!(
                    data.tokens,
                    "Tagged union variant may not have a named type"
                )
            }
            None => log_preparse_error!(data.tokens, "Failed to parse tagged union variant type"),
        }

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, punctuator!(CloseBrace));

    defined_type(
        data,
        Some(name.clone()),
        CXNaiveTypeKind::TaggedUnion {
            name: name.clone(),
            variants: variants.clone(),
        }
        .to_type(),
        template_prototype.clone(),
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_union_def(data: &mut ParserData) -> Option<CXNaiveType> {
    assert_token_matches!(data.tokens, keyword!(Union));

    if peek_kind!(data.tokens, keyword!(Class)) {
        data.tokens.back();
        return parse_tagged_union_def(data);
    }

    let name = parse_std_ident(&mut data.tokens);
    let template_prototype = try_parse_template(&mut data.tokens);

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Union);
    }

    let mut fields = Vec::new();

    loop {
        let (name, _type) = parse_initializer(&mut data.tokens)?;

        let Some(name) = name else {
            log_preparse_error!(
                data.tokens,
                "UNSUPPORTED: Nameless union member of type {}",
                _type
            );
        };

        fields.push((name.to_string(), _type));

        if !try_next!(data.tokens, punctuator!(Semicolon)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, punctuator!(CloseBrace));

    defined_type(
        data,
        name.clone(),
        CXNaiveTypeKind::Structured { name, fields }.to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
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

pub(crate) fn parse_type_base(data: &mut ParserData) -> Option<CXNaiveType> {
    let _type = match data.tokens.peek()?.kind {
        identifier!() => {
            let ident = parse_std_ident(&mut data.tokens)?;

            if peek_kind!(data.tokens, operator!(Less)) {
                let params = parse_template_args(&mut data.tokens)?;

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

        keyword!(Struct) => parse_struct_def(data),
        keyword!(Enum) => parse_enum_def(tokens),
        keyword!(Union) => parse_union_def(tokens),

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

pub(crate) fn parse_initializer(data: &mut ParserData) -> Option<(Option<CXIdent>, CXNaiveType)> {
    let prefix_specs = parse_specifier(&mut data.tokens);
    let type_base = parse_type_base(data)?;

    parse_base_mods(&mut data.tokens, type_base.add_specifier(prefix_specs))
}
