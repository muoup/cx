use crate::parse::expressions::parse_expr;
use crate::parse::{ParserData, try_parse_simple_identifier};
use crate::{assert_token_matches, log::parse_point_error, next_kind, peek_kind, try_next};
use cx_hir::ast::HIRStmt;
use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::global_var::HIREnumDefinition;
use cx_hir::ast::types::HIRMoveSemantics;
use cx_hir::ast::{
    function::{HIRFunctionKind, HIRFunctionPrototype},
    global_var::{HIREnumVariant, HIRGlobalVariable},
    modifiers::{
        HIR_CONST, HIR_RESTRICT, HIR_VOLATILE, HIRSymbolNameScheme, HIRTypeQualifiers, LinkageMode,
    },
    template::HIRTemplatePrototype,
    types::{HIRAggregateAttributes, HIRField, HIRType, HIRTypeKind, PredeclarationType},
};
use cx_log::CXResult;
use cx_thir::intrinsic_types::is_intrinsic_type;
use cx_tokens::token::{PunctuatorType, SpecifierType, TokenKind};
use cx_tokens::{
    TokenIter, TokenRange, identifier, intrinsic, keyword, operator, punctuator, specifier,
};
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

use crate::parse::functions::{ParseParamsResult, parse_params};
use crate::parse::templates::{note_templated_types, try_parse_template, unnote_templated_types};
use crate::parse::{parse_intrinsic, try_parse_qualified_name, try_parse_type_identifier};

pub fn is_type_decl(data: &mut ParserData) -> CXResult<bool> {
    let tok = data.tokens.peek().map(|tok| tok.kind.clone());

    if tok.is_none() {
        return Ok(false);
    }

    Ok(match &tok.unwrap() {
        intrinsic!() | specifier!() | keyword!(Struct, Union, Enum, Register) => true,

        identifier!(name) if is_intrinsic_type(name) => true,

        TokenKind::Identifier(_) => {
            let pre_idx = data.tokens.index;
            let Some(ident) = try_parse_qualified_name(&mut data.tokens)? else {
                unreachable!()
            };
            data.tokens.index = pre_idx;

            data.is_type_ident(&ident)?
                && !matches!(
                    data.tokens.slice.get(pre_idx + 1).map(|token| &token.kind),
                    Some(
                        TokenKind::Assignment(_)
                            | TokenKind::Operator(cx_tokens::token::OperatorType::Access)
                    )
                )
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

fn parse_type_attributes(
    data: &mut ParserData,
    kind_name: &str,
) -> CXResult<HIRAggregateAttributes> {
    let mut attributes = HIRAggregateAttributes::default();

    if try_next!(data.tokens, punctuator!(Colon)) {
        loop {
            assert_token_matches!(data.tokens, TokenKind::CompilerIdentifier(attr));
            let attr = attr.clone();

            match attr.as_str() {
                "nocopy" => attributes.semantics = HIRMoveSemantics::Nocopy,
                "nodrop" => attributes.semantics = HIRMoveSemantics::Nodrop,
                "unsafe_move" => attributes.unsafe_move = true,
                "copy_traits" => {
                    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                    assert_token_matches!(data.tokens, identifier!(type_param));
                    let type_param = type_param.clone();
                    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                    attributes.copy_traits = Some(type_param);
                }
                _ => {
                    return parse_point_error(
                        &data.tokens,
                        format!("Unknown {kind_name} attribute '@{}'", attr),
                    );
                }
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
    _type: HIRType,
) -> CXResult<HIRField> {
    if try_next!(data.tokens, punctuator!(Colon)) {
        let width = match next_kind!(data.tokens)? {
            TokenKind::IntLiteral(literal) => literal.magnitude as usize,
            _ => {
                return parse_point_error(
                    &data.tokens,
                    "Expected non-negative integer literal bitfield width".to_string(),
                );
            }
        };

        return Ok(HIRField::Bitfield {
            name: name.map(|name| name.to_string()),
            integer_type: _type,
            width,
        });
    }

    let Some(name) = name else {
        return parse_point_error(
            &data.tokens,
            format!("UNSUPPORTED: Nameless aggregate member of type {}", _type),
        );
    };

    Ok(HIRField::standard(name.to_string(), _type))
}

fn parse_aggregate_fields(data: &mut ParserData) -> CXResult<Vec<HIRField>> {
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
) -> CXResult<HIRType> {
    let Some(name) = name else {
        return parse_point_error(&data.tokens, "Predeclaration must have a name".to_string());
    };
    let is_root_name = name.namespace.is_root();
    let definition_name = name.name.clone();

    let ty = HIRTypeKind::Identifier {
        name,
        predeclaration,
        template_input: None,
    }
    .to_type();

    if matches!(
        predeclaration,
        PredeclarationType::Struct | PredeclarationType::Union
    ) && is_root_name
    {
        data.add_stmt(HIRStmt::TypeDefinition {
            name: Some(definition_name),
            visibility: data.visibility,
            template_prototype: None,
            _type: ty.clone(),
            tag: Some(predeclaration),
        });
    }

    Ok(ty)
}

fn defined_type(
    data: &mut ParserData,
    name: Option<CXIdent>,
    _type: HIRType,
    template_prototype: Option<HIRTemplatePrototype>,
    predeclaration: PredeclarationType,
) -> CXResult<HIRType> {
    if let Some(name) = name {
        // If structure definition has a name, add it to the type map and return
        // the identifier pointer to that type

        data.add_stmt(HIRStmt::TypeDefinition {
            name: Some(name.clone()),
            visibility: data.visibility,
            template_prototype,
            _type,
            tag: Some(predeclaration),
        });

        Ok(HIRTypeKind::Identifier {
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

pub(crate) fn parse_struct_def(data: &mut ParserData) -> CXResult<HIRType> {
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
            None => {
                return parse_point_error(
                    &data.tokens,
                    "Struct name must be a simple identifier".to_string(),
                );
            }
        },
    };

    if let Some(template_prototype) = &template_prototype {
        unnote_templated_types(data, template_prototype);
    }

    defined_type(
        data,
        name.clone(),
        HIRTypeKind::Structured {
            name,
            attributes,
            fields,
        }
        .to_type(),
        template_prototype,
        PredeclarationType::Struct,
    )
}

pub(crate) fn parse_enum_def(data: &mut ParserData) -> CXResult<HIRType> {
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
            return parse_point_error(&data.tokens, "Expected enum variant name".to_string());
        };

        let value = if try_next!(data.tokens, TokenKind::Assignment(None)) {
            data.change_comma_mode(false);
            let value = parse_expr(data)?;
            data.pop_comma_mode();
            Some(value)
        } else {
            None
        };

        variants.push(HIREnumVariant {
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
            None => {
                return parse_point_error(
                    &data.tokens,
                    "Expected name found qualified identifier".to_string(),
                );
            }
        },
    };

    data.add_stmt(HIRStmt::GlobalVariableDefinition {
        visibility: data.visibility,
        variable: HIRGlobalVariable::EnumDefinition(HIREnumDefinition {
            name: name.clone(),
            variants: variants.clone(),
        }),
    });

    defined_type(
        data,
        name,
        HIRTypeKind::Identifier {
            name: QualifiedName::new_raw(CXIdent::new("int")),
            predeclaration: PredeclarationType::None,
            template_input: None,
        }
        .to_type(),
        None,
        PredeclarationType::Enum,
    )
}

pub(crate) fn parse_tagged_union_def(data: &mut ParserData) -> CXResult<HIRType> {
    assert_token_matches!(data.tokens, keyword!(Enum), "'enum'");
    assert_token_matches!(data.tokens, keyword!(Union), "'union'");

    let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
        return parse_point_error(&data.tokens, "Tagged unions must have a name".to_string());
    };

    let template_prototype = try_parse_template(&mut data.tokens)?;
    let attributes = parse_type_attributes(data, "enum union")?;

    assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

    let mut variants = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
            return parse_point_error(
                &data.tokens,
                "Expected variant name in tagged union".to_string(),
            );
        };

        assert_token_matches!(data.tokens, operator!(ScopeRes), "'::'");

        match parse_initializer(data) {
            // Success Path = Valid Type + No Name
            Ok((None, _type, _)) => variants.push(HIRField::standard(name.to_string(), _type)),

            Ok((Some(_), _, _)) => {
                return parse_point_error(
                    &data.tokens,
                    "Tagged union variant may not have a named type".to_string(),
                );
            }

            _ => {
                return parse_point_error(
                    &data.tokens,
                    "Failed to parse tagged union variant type".to_string(),
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
        HIRTypeKind::TaggedUnion {
            name: name.clone(),
            attributes,
            variants: variants.clone(),
        }
        .to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_union_def(data: &mut ParserData) -> CXResult<HIRType> {
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
            None => {
                return parse_point_error(
                    &data.tokens,
                    "Union name must be a simple identifier".to_string(),
                );
            }
        },
    };

    defined_type(
        data,
        name.clone(),
        HIRTypeKind::Union { name, fields }.to_type(),
        template_prototype,
        PredeclarationType::Union,
    )
}

pub(crate) fn parse_specifier(tokens: &mut TokenIter) -> HIRTypeQualifiers {
    parse_decl_specifiers(tokens).qualifiers
}

pub(crate) struct ParsedSpecifiers {
    pub(crate) qualifiers: HIRTypeQualifiers,
    pub(crate) linkage: LinkageMode,
}

pub(crate) fn parse_decl_specifiers(tokens: &mut TokenIter) -> ParsedSpecifiers {
    let mut spec_acc: HIRTypeQualifiers = 0;
    let mut linkage = LinkageMode::Standard;

    while let Ok(TokenKind::Specifier(spec)) = next_kind!(tokens) {
        match spec {
            SpecifierType::Const => spec_acc |= HIR_CONST,
            SpecifierType::Volatile => spec_acc |= HIR_VOLATILE,
            SpecifierType::Restrict => spec_acc |= HIR_RESTRICT,
            SpecifierType::Extern => linkage = LinkageMode::Extern,
            SpecifierType::Static => linkage = LinkageMode::Static,
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
    acc_type: HIRType,
) -> CXResult<(Option<CXIdent>, HIRType)> {
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
            let mut acc_type = HIRType::new(
                specs,
                HIRTypeKind::PointerTo {
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
            let mut ref_type = HIRTypeKind::MemoryReference {
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

            let mut array_suffixes: Vec<Option<HIRExpression>> = Vec::new();
            while try_next!(data.tokens, punctuator!(OpenBracket)) {
                if try_next!(data.tokens, punctuator!(CloseBracket)) {
                    array_suffixes.push(None);
                } else {
                    let size = parse_expr(data)?;
                    assert_token_matches!(data.tokens, punctuator!(CloseBracket), "']'");
                    array_suffixes.push(Some(size));
                }
            }

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

            let prototype = HIRFunctionPrototype {
                kind: HIRFunctionKind::Standard(CXIdent::new("__internal_fnptr")),
                return_type: acc_type,
                params,
                var_args,
                contract,
                linkage: LinkageMode::Standard,
                symbol_naming: HIRSymbolNameScheme::Namespaced,
                range: TokenRange::internal(),
            };

            let fn_ptr_type = HIRTypeKind::FunctionPointer {
                prototype: Box::new(prototype),
            }
            .to_type()
            .pointer_to(0);

            let fn_ptr_type = array_suffixes
                .into_iter()
                .rev()
                .fold(fn_ptr_type, |inner, size| match size {
                    Some(size) => {
                        HIRTypeKind::ExplicitSizedArray(Box::new(inner), Box::new(size)).to_type()
                    }
                    None => HIRTypeKind::ImplicitSizedArray(Box::new(inner)).to_type(),
                });

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
    mut acc_type: HIRType,
) -> CXResult<HIRType> {
    let Some(next_tok) = data.tokens.peek() else {
        return Ok(acc_type);
    };

    match &next_tok.kind {
        punctuator!(OpenBracket) => {
            data.tokens.next();

            if try_next!(data.tokens, punctuator!(CloseBracket)) {
                let range = acc_type.range.clone();
                let nested = parse_type_suffix_mod(data, acc_type)?;
                acc_type = HIRTypeKind::ImplicitSizedArray(Box::new(nested)).to_type();
                acc_type.range = range;
            } else {
                let inner = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseBracket), "']'");

                let range = acc_type.range.clone();
                let nested = parse_type_suffix_mod(data, acc_type)?;
                acc_type =
                    HIRTypeKind::ExplicitSizedArray(Box::new(nested), Box::new(inner)).to_type();
                acc_type.range = range;
            }

            Ok(acc_type)
        }

        operator!(Ampersand) => {
            data.tokens.next();

            let range = acc_type.range.clone();
            let mut ref_type = HIRTypeKind::MemoryReference {
                inner_type: Box::new(acc_type),
            }
            .to_type();
            ref_type.range = range;

            parse_type_suffix_mod(data, ref_type)
        }

        _ => Ok(acc_type),
    }
}

pub(crate) fn parse_type_base(data: &mut ParserData) -> CXResult<HIRType> {
    let start_index = data.tokens.index;
    let Some(next_token) = data.tokens.peek() else {
        return parse_point_error(
            &data.tokens,
            "Expected type base, found end of tokens.".to_string(),
        );
    };

    let _type = match &next_token.kind {
        identifier!() => {
            let Some(ident) = try_parse_type_identifier(data)? else {
                unreachable!();
            };

            Ok(ident.into_type(PredeclarationType::None))
        }

        intrinsic!() => Ok(HIRTypeKind::Identifier {
            name: QualifiedName::new_raw(parse_intrinsic(&mut data.tokens)?),
            predeclaration: PredeclarationType::None,
            template_input: None,
        }
        .to_type()),

        keyword!(Struct) => parse_struct_def(data),
        keyword!(Enum) => parse_enum_def(data),
        keyword!(Union) => parse_union_def(data),

        tok => {
            return parse_point_error(
                &data.tokens,
                format!(
                    "Expected type base (identifier, struct, enum, union, or intrinsic), found: {tok}"
                ),
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
    acc_type: HIRType,
) -> CXResult<(Option<CXIdent>, HIRType)> {
    let (name, modified_type) = parse_type_mods(data, acc_type)?;

    let modified_type = parse_type_suffix_mod(data, modified_type)?;

    Ok((name, modified_type))
}

pub(crate) fn parse_initializer(
    data: &mut ParserData,
) -> CXResult<(Option<CXIdent>, HIRType, LinkageMode)> {
    let prefix_specs = parse_decl_specifiers(&mut data.tokens);
    let type_base = parse_type_base(data)?;

    let (name, _type) = parse_base_mods(data, type_base.add_specifier(prefix_specs.qualifiers))?;
    Ok((name, _type, prefix_specs.linkage))
}

pub(crate) fn parse_typedef_initializer(
    data: &mut ParserData,
) -> CXResult<(Option<CXIdent>, HIRType)> {
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

    let prototype = HIRFunctionPrototype {
        kind: HIRFunctionKind::Standard(CXIdent::new("__internal_fnptr")),
        return_type,
        params,
        var_args,
        contract,
        linkage: LinkageMode::Standard,
        symbol_naming: HIRSymbolNameScheme::Namespaced,
        range: TokenRange::internal(),
    };

    Ok((
        name,
        HIRTypeKind::FunctionPointer {
            prototype: Box::new(prototype),
        }
        .to_type(),
    ))
}
