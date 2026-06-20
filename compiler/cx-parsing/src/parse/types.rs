use crate::parse::expressions::parse_expr;
use crate::parse::{try_parse_simple_identifier, ParserData};
use crate::{assert_token_matches, next_kind, peek_kind, try_next};
use cx_ast::ast::global_var::CXEnumDefinition;
use cx_ast::ast::CXASTStmt;
use cx_ast::ast::types::CXMoveSemantics;
use cx_ast::ast::{
    function::{CXFunctionKind, CXFunctionPrototype},
    global_var::{CXEnumVariant, CXGlobalVariable},
    modifiers::{CXLinkageMode, CXTypeQualifiers, CX_CONST, CX_RESTRICT, CX_VOLATILE},
    template::CXTemplatePrototype,
    types::{CXField, CXAggregateAttributes, CXType, CXTypeKind, PredeclarationType},
};
use cx_log::CXResult;
use cx_mir::intrinsic_types::is_intrinsic_type;
use cx_tokens::token::{PunctuatorType, SpecifierType, TokenKind};
use cx_tokens::{
    identifier, intrinsic, keyword, operator, punctuator, specifier, TokenIter, TokenRange,
};
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

use crate::parse::functions::{parse_params, ParseParamsResult};
use crate::parse::templates::{note_templated_types, try_parse_template, unnote_templated_types};
use crate::parse::{parse_intrinsic, try_parse_qualified_name, try_parse_type_identifier};

pub fn is_type_decl(data: &mut ParserData) -> CXResult<bool> {
    let tok = data.tokens.peek().map(|tok| tok.kind.clone());

    if tok.is_none() {
        return Ok(false);
    }

    Ok(match &tok.unwrap() {
        intrinsic!() | specifier!() | keyword!(Struct, Union, Enum) => true,

        identifier!(name) if is_intrinsic_type(name) => true,

        TokenKind::Identifier(_) => {
            let pre_idx = data.tokens.index;
            let Some(ident) = try_parse_qualified_name(&mut data.tokens)? else {
                unreachable!()
            };
            data.tokens.index = pre_idx;

            data.is_type_ident(&ident)?
        }

        _ => false,
    })
}

fn token_range(data: &ParserData, start: usize, end: usize) -> TokenRange {
    TokenRange::new(
        start,
        end.max(start.saturating_add(1)),
        data.file_origin_for_range(start, end),
    )
}

fn parse_type_attributes(data: &mut ParserData, kind_name: &str) -> CXResult<CXAggregateAttributes> {
    let mut attributes = CXAggregateAttributes::default();

    if try_next!(data.tokens, punctuator!(Colon)) {
        loop {
            assert_token_matches!(data.tokens, TokenKind::CompilerIdentifier(attr));

            match attr.as_str() {
                "nocopy" => attributes.semantics = CXMoveSemantics::Nocopy,
                "nodrop" => attributes.semantics = CXMoveSemantics::Nodrop,
                "copy_traits" => {
                    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                    assert_token_matches!(data.tokens, identifier!(type_param));
                    let type_param = type_param.clone();
                    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                    attributes.copy_traits = Some(type_param);
                }
                _ => return log_parse_error!(data, "Unknown {kind_name} attribute '@{}'", attr),
            }

            if !try_next!(data.tokens, operator!(Comma)) {
                break;
            }
        }
    }

    Ok(attributes)
}

fn aggregate_field_from_decl(
    data: &mut ParserData,
    name: Option<CXIdent>,
    _type: CXType,
) -> CXResult<CXField> {
    if try_next!(data.tokens, punctuator!(Colon)) {
        let width = match next_kind!(data.tokens)? {
            TokenKind::IntLiteral(width) if *width >= 0 => *width as usize,
            _ => {
                return log_preparse_error!(
                    data.tokens,
                    "Expected non-negative integer literal bitfield width"
                );
            }
        };

        return Ok(CXField::Bitfield {
            name: name.map(|name| name.to_string()),
            integer_type: _type,
            width,
        });
    }

    let Some(name) = name else {
        return log_preparse_error!(
            data.tokens,
            "UNSUPPORTED: Nameless aggregate member of type {}",
            _type
        );
    };

    Ok(CXField::standard(name.to_string(), _type))
}

fn parse_aggregate_fields(data: &mut ParserData) -> CXResult<Vec<CXField>> {
    let prefix_specs = parse_decl_specifiers(&mut data.tokens);
    let type_base = parse_type_base(data)?.add_specifier(prefix_specs.qualifiers);
    let mut fields = Vec::new();

    loop {
        let (name, _type) = parse_base_mods(data, type_base.clone())?;
        fields.push(aggregate_field_from_decl(data, name, _type)?);

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    Ok(fields)
}

fn predeclaration_type(
    data: &mut ParserData,
    name: Option<QualifiedName>,
    predeclaration: PredeclarationType,
) -> CXResult<CXType> {
    let Some(name) = name else {
        return log_parse_error!(data, "Predeclaration must have a name");
    };

    Ok(CXTypeKind::Identifier {
        name,
        predeclaration,
        template_input: None,
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

        data.add_stmt(CXASTStmt::TypeDefinition {
            name: Some(name.clone()),
            visibility: data.visibility,
            template_prototype,
            _type,
        });

        Ok(CXTypeKind::Identifier {
            name: QualifiedName::new_raw(name),
            predeclaration,
            template_input: None,
        }
        .to_type())
    } else {
        // If the structure definition is anonymous, it can only be parsed as
        // an in-place type.

        Ok(_type)
    }
}

pub(crate) fn parse_struct_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Struct), "'struct'");

    let name = try_parse_qualified_name(&mut data.tokens)?;
    let template_prototype = try_parse_template(&mut data.tokens)?;
    let attributes = parse_type_attributes(data, "struct")?;

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Struct);
    }

    if let Some(template_prototype) = &template_prototype {
        note_templated_types(data, template_prototype)?;
    }

    let mut fields = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        fields.extend(parse_aggregate_fields(data)?);
        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
    }

    let name = match name {
        None => None,
        Some(name) => match name.root_name() {
            Some(name) => Some(name),
            None => return log_parse_error!(data, "Struct name must be a simple identifier"),
        },
    };

    if let Some(template_prototype) = &template_prototype {
        unnote_templated_types(data, template_prototype);
    }

    defined_type(
        data,
        name.clone(),
        CXTypeKind::Structured {
            name,
            attributes,
            fields,
        }
        .to_type(),
        template_prototype,
        PredeclarationType::Struct,
    )
}

pub(crate) fn parse_enum_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Enum), "'enum'");

    if peek_kind!(data.tokens, keyword!(Union)) {
        data.tokens.back();
        return parse_tagged_union_def(data);
    }

    let name = try_parse_qualified_name(&mut data.tokens)?;

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Enum);
    }

    let mut variants = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let Some(variant_name) = try_parse_simple_identifier(&mut data.tokens) else {
            return log_preparse_error!(data.tokens, "Expected enum variant name");
        };

        let value = if try_next!(data.tokens, TokenKind::Assignment(None)) {
            data.change_comma_mode(false);
            let value = parse_expr(data)?;
            data.pop_comma_mode();
            Some(value)
        } else {
            None
        };

        variants.push(CXEnumVariant {
            name: variant_name,
            value,
        });

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
            break;
        }
    }

    let name = match name {
        None => None,
        Some(name) => match name.root_name() {
            Some(name) => Some(name),
            None => return log_parse_error!(data, "Expected name found qualified identifier"),
        },
    };

    data.add_stmt(CXASTStmt::GlobalVariableDefinition {
        visibility: data.visibility,
        variable: CXGlobalVariable::EnumDefinition(CXEnumDefinition {
            name: name.clone(),
            variants: variants.clone(),
        }),
    });

    defined_type(
        data,
        name,
        CXTypeKind::Identifier {
            name: QualifiedName::new_raw(CXIdent::new("int")),
            predeclaration: PredeclarationType::None,
            template_input: None,
        }
        .to_type(),
        None,
        PredeclarationType::Enum,
    )
}

pub(crate) fn parse_tagged_union_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Enum), "'enum'");
    assert_token_matches!(data.tokens, keyword!(Union), "'union'");

    let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
        return log_preparse_error!(data.tokens, "Tagged unions must have a name");
    };

    let template_prototype = try_parse_template(&mut data.tokens)?;
    let attributes = parse_type_attributes(data, "enum union")?;

    assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

    let mut variants = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
            return log_preparse_error!(data.tokens, "Expected variant name in tagged union");
        };

        assert_token_matches!(data.tokens, operator!(ScopeRes), "'::'");

        match parse_initializer(data) {
            // Success Path = Valid Type + No Name
            Ok((None, _type, _)) => variants.push(CXField::standard(name.to_string(), _type)),

            Ok((Some(_), _, _)) => {
                return log_preparse_error!(
                    data.tokens,
                    "Tagged union variant may not have a named type"
                );
            }

            _ => {
                return log_preparse_error!(
                    data.tokens,
                    "Failed to parse tagged union variant type"
                );
            }
        }

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
            break;
        }
    }

    defined_type(
        data,
        Some(name.clone()),
        CXTypeKind::TaggedUnion {
            name: name.clone(),
            attributes,
            variants: variants.clone(),
        }
        .to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_union_def(data: &mut ParserData) -> CXResult<CXType> {
    assert_token_matches!(data.tokens, keyword!(Union), "'union'");

    let name = try_parse_qualified_name(&mut data.tokens)?;
    let template_prototype = try_parse_template(&mut data.tokens)?;

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        return predeclaration_type(data, name, PredeclarationType::Union);
    }

    let mut fields = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        fields.extend(parse_aggregate_fields(data)?);
        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
    }

    let name = match name {
        None => None,
        Some(name) => match name.root_name() {
            Some(name) => Some(name),
            None => return log_parse_error!(data, "Union name must be a simple identifier"),
        },
    };

    defined_type(
        data,
        name.clone(),
        CXTypeKind::Union { name, fields }.to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_specifier(tokens: &mut TokenIter) -> CXTypeQualifiers {
    parse_decl_specifiers(tokens).qualifiers
}

struct ParsedSpecifiers {
    qualifiers: CXTypeQualifiers,
    linkage: CXLinkageMode,
}

fn parse_decl_specifiers(tokens: &mut TokenIter) -> ParsedSpecifiers {
    let mut spec_acc: CXTypeQualifiers = 0;
    let mut linkage = CXLinkageMode::Standard;

    while let Ok(TokenKind::Specifier(spec)) = next_kind!(tokens) {
        match spec {
            SpecifierType::Const => spec_acc |= CX_CONST,
            SpecifierType::Volatile => spec_acc |= CX_VOLATILE,
            SpecifierType::Restrict => spec_acc |= CX_RESTRICT,
            SpecifierType::Extern => linkage = CXLinkageMode::Extern,
            SpecifierType::Static => linkage = CXLinkageMode::Static,
            SpecifierType::Inline | SpecifierType::ThreadLocal => {}

            SpecifierType::Public | SpecifierType::Private => break,
        }
    }

    tokens.back();
    ParsedSpecifiers {
        qualifiers: spec_acc,
        linkage,
    }
}

pub(crate) fn parse_type_mods(
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
            assert_token_matches!(data.tokens, operator!(Asterisk), "'*'");

            let specs = parse_specifier(&mut data.tokens);
            let range = acc_type.range.clone();
            let mut acc_type = CXType::new(
                specs,
                CXTypeKind::PointerTo {
                    inner_type: Box::new(acc_type),
                },
            );
            acc_type.range = range;

            parse_type_mods(data, acc_type)
        }

        operator!(Asterisk) => {
            data.tokens.next();
            let specs = parse_specifier(&mut data.tokens);
            let acc_type = acc_type.pointer_to(specs);

            parse_type_mods(data, acc_type)
        }

        operator!(Ampersand) => {
            data.tokens.next();

            let range = acc_type.range.clone();
            let mut ref_type = CXTypeKind::MemoryReference {
                inner_type: Box::new(acc_type),
            }
            .to_type();
            ref_type.range = range;

            parse_type_mods(data, ref_type)
        }

        punctuator!(OpenParen) => {
            data.tokens.next();
            if !matches!(next_kind!(data.tokens), Ok(operator!(Asterisk))) {
                data.tokens.index = start_index;
                return Ok((None, acc_type));
            }
            let name = try_parse_simple_identifier(&mut data.tokens);
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen),
                "')'"
            );

            let ParseParamsResult {
                params,
                var_args,
                contract,
                ..
            } = parse_params(data)?;

            let prototype = CXFunctionPrototype {
                kind: CXFunctionKind::Standard(CXIdent::new("__internal_fnptr")),
                return_type: acc_type,
                params,
                var_args,
                contract,
                linkage: CXLinkageMode::Standard,
                range: TokenRange::internal(),
            };

            let fn_ptr_type = CXTypeKind::FunctionPointer {
                prototype: Box::new(prototype),
            }
            .to_type()
            .pointer_to(0);

            Ok((name, fn_ptr_type))
        }

        identifier!() => {
            let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
                unreachable!();
            };

            Ok((Some(name), acc_type))
        }

        _ => Ok((None, acc_type)),
    }
}

pub(crate) fn parse_type_suffix_mod(
    data: &mut ParserData,
    mut acc_type: CXType,
) -> CXResult<CXType> {
    let Some(next_tok) = data.tokens.peek() else {
        return Ok(acc_type);
    };

    match &next_tok.kind {
        punctuator!(OpenBracket) => {
            data.tokens.next();

            if try_next!(data.tokens, punctuator!(CloseBracket)) {
                let range = acc_type.range.clone();
                acc_type = CXTypeKind::ImplicitSizedArray(Box::new(acc_type)).to_type();
                acc_type.range = range;
            } else {
                let inner = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseBracket), "']'");

                let range = acc_type.range.clone();
                acc_type =
                    CXTypeKind::ExplicitSizedArray(Box::new(acc_type), Box::new(inner)).to_type();
                acc_type.range = range;
            }

            parse_type_suffix_mod(data, acc_type)
        }

        operator!(Ampersand) => {
            data.tokens.next();

            let range = acc_type.range.clone();
            let mut ref_type = CXTypeKind::MemoryReference {
                inner_type: Box::new(acc_type),
            }
            .to_type();
            ref_type.range = range;

            parse_type_suffix_mod(data, ref_type)
        }

        _ => Ok(acc_type),
    }
}

pub(crate) fn parse_type_base(data: &mut ParserData) -> CXResult<CXType> {
    let start_index = data.tokens.index;
    let Some(next_token) = data.tokens.peek() else {
        return log_parse_error!(data, "Expected type base, found end of tokens.");
    };

    let _type = match &next_token.kind {
        identifier!() => {
            let Some(ident) = try_parse_type_identifier(data)? else {
                unreachable!();
            };

            Ok(ident.into_type(PredeclarationType::None))
        }

        intrinsic!() => Ok(CXTypeKind::Identifier {
            name: QualifiedName::new_raw(parse_intrinsic(&mut data.tokens)?),
            predeclaration: PredeclarationType::None,
            template_input: None,
        }
        .to_type()),

        keyword!(Struct) => parse_struct_def(data),
        keyword!(Enum) => parse_enum_def(data),
        keyword!(Union) => parse_union_def(data),

        tok => {
            return log_parse_error!(
                data,
                "Expected type base (identifier, struct, enum, union, or intrinsic), found: {tok}"
            );
        }
    };

    let specifiers = parse_specifier(&mut data.tokens);

    Ok(_type?.add_specifier(specifiers).with_range(token_range(
        data,
        start_index,
        data.tokens.index,
    )))
}

pub(crate) fn parse_base_mods(
    data: &mut ParserData,
    acc_type: CXType,
) -> CXResult<(Option<CXIdent>, CXType)> {
    let (name, modified_type) = parse_type_mods(data, acc_type)?;

    let modified_type = parse_type_suffix_mod(data, modified_type)?;

    Ok((name, modified_type))
}

pub(crate) fn parse_initializer(
    data: &mut ParserData,
) -> CXResult<(Option<CXIdent>, CXType, CXLinkageMode)> {
    let prefix_specs = parse_decl_specifiers(&mut data.tokens);
    let type_base = parse_type_base(data)?;

    let (name, _type) = parse_base_mods(data, type_base.add_specifier(prefix_specs.qualifiers))?;
    Ok((name, _type, prefix_specs.linkage))
}

pub(crate) fn parse_typedef_initializer(
    data: &mut ParserData,
) -> CXResult<(Option<CXIdent>, CXType)> {
    let (name, return_type, _) = parse_initializer(data)?;

    if name.is_none() || !peek_kind!(data.tokens, punctuator!(OpenParen)) {
        return Ok((name, return_type));
    }

    let ParseParamsResult {
        params,
        var_args,
        contract,
        ..
    } = parse_params(data)?;

    let prototype = CXFunctionPrototype {
        kind: CXFunctionKind::Standard(CXIdent::new("__internal_fnptr")),
        return_type,
        params,
        var_args,
        contract,
        linkage: CXLinkageMode::Standard,
        range: TokenRange::internal(),
    };

    Ok((
        name,
        CXTypeKind::FunctionPointer {
            prototype: Box::new(prototype),
        }
        .to_type(),
    ))
}
