use crate::parse::ParserData;
use cx_tokens::token::{PunctuatorType, SpecifierType, TokenKind};
use cx_tokens::{identifier, intrinsic, keyword, operator, punctuator, TokenIter};
use cx_ast::ast::CXGlobalVariable;
use cx_ast::data::{
    CXFunctionKind, CXPrototype, CXType, CXTypeKind, CXTemplatePrototype,
    CXTypeSpecifier, PredeclarationType, CX_CONST, CX_RESTRICT, CX_VOLATILE,
};
use cx_ast::{assert_token_matches, next_kind, peek_kind, peek_next_kind, try_next};
use cx_util::identifier::CXIdent;
use cx_util::CXResult;

use crate::parse::functions::{parse_params, ParseParamsResult};
use crate::parse::templates::{parse_template_args, try_parse_template};
use crate::parse::{parse_intrinsic, parse_std_ident};

fn predeclaration_type(
    data: &mut ParserData,
    name: Option<CXIdent>,
    predeclaration: PredeclarationType,
) -> CXResult<CXType> {
    let Some(name) = name else {
        return log_parse_error!(data, "Invalid token.");
    };

    Ok(CXTypeKind::Identifier {
        name,
        predeclaration,
    }
    .to_type())
}

fn defined_type(
    data: &mut ParserData,
    name: Option<CXIdent>,
    _type: CXType,
    template_prototype: Option<CXTemplatePrototype>,
    predeclaration: PredeclarationType,
) -> CXResult<CXType> {
    if let Some(name) = name {
        // If structure definition has a name, add it to the type map and return
        // the identifier pointer to that type

        data.add_type(name.as_string(), _type, template_prototype);

        Ok(CXTypeKind::Identifier {
            name,
            predeclaration,
        }
        .to_type())
    } else {
        // If the structure definition is anonymous, it can only be parsed as
        // an in-place type.

        Ok(_type)
    }
}

pub(crate) fn parse_struct_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Struct));

    let name = parse_std_ident(&mut data.tokens).ok();
    let template_prototype = try_parse_template(&mut data.tokens)?;

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Struct);
    }

    let mut fields = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let Ok((name, _type)) = parse_initializer(data) else {
            return log_preparse_error!(data.tokens, "Failed to parse struct member type");
        };

        let Some(name) = name else {
            return log_preparse_error!(
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
        CXTypeKind::Structured { name, fields }.to_type(),
        template_prototype,
        PredeclarationType::Struct,
    )
}

pub(crate) fn parse_enum_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Enum));

    if peek_kind!(data.tokens, keyword!(Union)) {
        data.tokens.back();
        return parse_tagged_union_def(data);
    }

    let name = parse_std_ident(&mut data.tokens).ok();

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Enum);
    }

    let mut idx = 0;

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let variant_name = parse_std_ident(&mut data.tokens)?;

        if try_next!(data.tokens, TokenKind::Assignment(None)) {
            match next_kind!(data.tokens)? {
                TokenKind::IntLiteral(val) => {
                    idx = *val;
                }

                _ => {
                    return log_preparse_error!(
                        data.tokens,
                        "Enum variant value must be an integer literal"
                    )
                }
            }
        }

        data.add_global_variable(
            variant_name.as_string(),
            CXGlobalVariable::EnumConstant(idx as i32),
        );
        idx += 1;

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseBrace));
            break;
        }
    }

    defined_type(
        data,
        name,
        CXTypeKind::Identifier {
            name: CXIdent::new("int"),
            predeclaration: PredeclarationType::None,
        }
        .to_type(),
        None,
        PredeclarationType::Enum,
    )
}

pub(crate) fn parse_tagged_union_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Enum));
    assert_token_matches!(data.tokens, keyword!(Union));

    let name = parse_std_ident(&mut data.tokens)?;
    let template_prototype = try_parse_template(&mut data.tokens)?;

    assert_token_matches!(data.tokens, punctuator!(OpenBrace));

    let mut variants = Vec::new();

    loop {
        let Ok(name) = parse_std_ident(&mut data.tokens) else {
            return log_preparse_error!(data.tokens, "Expected variant name in tagged union");
        };

        assert_token_matches!(data.tokens, operator!(ScopeRes));

        match parse_initializer(data) {
            // Success Path = Valid Type + No Name
            Ok((None, _type)) => variants.push((name.to_string(), _type)),

            Ok((Some(_), _)) => {
                return log_preparse_error!(
                    data.tokens,
                    "Tagged union variant may not have a named type"
                )
            }

            _ => {
                return log_preparse_error!(
                    data.tokens,
                    "Failed to parse tagged union variant type"
                )
            }
        }

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, punctuator!(CloseBrace));

    defined_type(
        data,
        Some(name.clone()),
        CXTypeKind::TaggedUnion {
            name: name.clone(),
            variants: variants.clone(),
        }
        .to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_union_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Union));

    let name = parse_std_ident(&mut data.tokens).ok();
    let template_prototype = try_parse_template(&mut data.tokens)?;

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Union);
    }

    let mut fields = Vec::new();

    loop {
        let (name, _type) = parse_initializer(data)?;

        let Some(name) = name else {
            return log_preparse_error!(
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
        CXTypeKind::Structured { name, fields }.to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_specifier(tokens: &mut TokenIter) -> CXTypeSpecifier {
    let mut spec_acc: CXTypeSpecifier = 0;

    while let Ok(TokenKind::Specifier(spec)) = next_kind!(tokens) {
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

pub(crate) fn parsetype_mods(
    data: &mut ParserData,
    acc_type: CXType,
) -> CXResult<(Option<CXIdent>, CXType)> {
    let Some(next_tok) = data.tokens.peek() else {
        return Ok((None, acc_type));
    };
    let start_index = data.tokens.index;

    match &next_tok.kind {
        keyword!(Weak) => {
            data.tokens.next();
            assert_token_matches!(data.tokens, operator!(Asterisk));

            let specs = parse_specifier(&mut data.tokens);
            let acc_type = CXType::new(
                specs,
                CXTypeKind::PointerTo {
                    inner_type: Box::new(acc_type),
                    weak: true,
                },
            );

            parsetype_mods(data, acc_type)
        }

        operator!(Asterisk) => {
            data.tokens.next();
            let specs = parse_specifier(&mut data.tokens);
            let acc_type = acc_type.pointer_to(false, specs);

            parsetype_mods(data, acc_type)
        }

        punctuator!(OpenParen) => {
            data.tokens.next();
            if !matches!(next_kind!(data.tokens), Ok(operator!(Asterisk))) {
                data.tokens.index = start_index;
                return Ok((None, acc_type));
            }
            let name = parse_std_ident(&mut data.tokens).ok();
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen)
            );

            let ParseParamsResult {
                params,
                var_args,
                contract,
                ..
            } = parse_params(data)?;

            let prototype = CXPrototype {
                kind: CXFunctionKind::Standard(CXIdent::new("__internal_fnptr")),
                return_type: acc_type,
                params,
                var_args,
                contract,
            };

            Ok((
                name,
                CXTypeKind::FunctionPointer {
                    prototype: Box::new(prototype),
                }
                .to_type()
                .pointer_to(false, 0),
            ))
        }

        identifier!() => Ok((Some(parse_std_ident(&mut data.tokens)?), acc_type)),

        _ => Ok((None, acc_type)),
    }
}

pub(crate) fn parse_suffixtype_mod(
    tokens: &mut TokenIter,
    acc_type: CXType,
) -> CXResult<CXType> {
    let Some(next_tok) = tokens.peek() else {
        return Ok(acc_type);
    };

    match &next_tok.kind {
        punctuator!(OpenBracket) => {
            tokens.next();

            let _type = match peek_next_kind!(tokens)? {
                punctuator!(CloseBracket) => {
                    CXTypeKind::ImplicitSizedArray(Box::new(acc_type)).to_type()
                }
                TokenKind::IntLiteral(size) => {
                    let size = *size as usize;

                    tokens.next();
                    CXTypeKind::ExplicitSizedArray(Box::new(acc_type), size).to_type()
                }

                _ => {
                    return log_preparse_error!(
                        tokens,
                        "Expected array size (integer literal) or ']' for implicit sized array"
                    )
                }
            };

            assert_token_matches!(tokens, punctuator!(CloseBracket));

            Ok(_type)
        }

        _ => Ok(acc_type),
    }
}

pub(crate) fn parse_type_base(data: &mut ParserData) -> CXResult<CXType> {
    let Some(next_token) = data.tokens.peek() else {
        return log_parse_error!(data, "Expected type base, found end of tokens.");
    };

    let _type = match &next_token.kind {
        identifier!() => {
            let ident = parse_std_ident(&mut data.tokens)?;

            if peek_kind!(data.tokens, operator!(Less)) {
                let params = parse_template_args(data)?;

                Ok(CXTypeKind::TemplatedIdentifier {
                    name: ident,
                    input: params,
                }
                .to_type())
            } else {
                Ok(CXTypeKind::Identifier {
                    name: ident,
                    predeclaration: PredeclarationType::None,
                }
                .to_type())
            }
        }

        intrinsic!() => Ok(CXTypeKind::Identifier {
            name: parse_intrinsic(&mut data.tokens)?,
            predeclaration: PredeclarationType::None,
        }
        .to_type()),

        keyword!(Struct) => parse_struct_def(data),
        keyword!(Enum) => parse_enum_def(data),
        keyword!(Union) => parse_union_def(data),

        _ => {
            return log_parse_error!(
                data,
                "Expected type base (identifier, struct, enum, union, or intrinsic)"
            )
        }
    };

    let specifiers = parse_specifier(&mut data.tokens);

    Ok(_type?.add_specifier(specifiers))
}

pub(crate) fn parse_base_mods(
    data: &mut ParserData,
    acc_type: CXType,
) -> CXResult<(Option<CXIdent>, CXType)> {
    let (name, modified_type) = parsetype_mods(data, acc_type)?;

    Ok((name, parse_suffixtype_mod(&mut data.tokens, modified_type)?))
}

pub(crate) fn parse_initializer(data: &mut ParserData) -> CXResult<(Option<CXIdent>, CXType)> {
    let prefix_specs = parse_specifier(&mut data.tokens);
    let type_base = parse_type_base(data)?;

    parse_base_mods(data, type_base.add_specifier(prefix_specs))
}
