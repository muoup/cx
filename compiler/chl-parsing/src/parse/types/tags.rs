use super::{
    DeclaratorKind, TypePosition, parse_abstract_type, parse_declaration_head, parse_declarator,
    parse_typed_declarator, starts_type,
};
use crate::{
    assert_token_matches,
    log::{parse_point_error, parse_underline_error},
    next_kind,
    parse::{
        expressions::parse_expr, try_parse_qualified_name, try_parse_simple_identifier, ParserData,
    },
    peek_kind, try_next,
};
use chl_hir::ast::{
    global_var::{HIREnumDefinition, HIREnumVariant, HIRGlobalVariable},
    modifiers::LinkageMode,
    types::{
        HIRAggregateAttributes, HIRField, HIRMoveSemantics, HIRTagKind, HIRType, HIRTypeKind,
        HIRTypeLookup, ANONYMOUS_MEMBER_PREFIX,
    },
    HIRStmt,
};
use chl_log::{catalogue::parse::*, CHLResult};
use chl_namespace::module::QualifiedName;
use chl_tokens::{
    TokenRange, identifier, keyword, operator, punctuator, token::TokenKind,
};
use chl_util::identifier::CHLIdent;

fn parse_type_attributes(
    data: &mut ParserData,
    kind_name: &str,
) -> CHLResult<HIRAggregateAttributes> {
    let mut attributes = HIRAggregateAttributes::default();

    if try_next!(data.tokens, punctuator!(Colon)) {
        loop {
            assert_token_matches!(
                data.tokens,
                TokenKind::CompilerIdentifier(attr),
                "a type attribute"
            );
            
            match attr.as_str() {
                "nocopy" => attributes.semantics = HIRMoveSemantics::Nocopy,
                "nodrop" => attributes.semantics = HIRMoveSemantics::Nodrop,
                "unsafe_move" => attributes.unsafe_move = true,
                "comptime" => attributes.comptime = true,
                "copy_traits" => {
                    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                    assert_token_matches!(
                        data.tokens,
                        identifier!(type_param),
                        "a type parameter name"
                    );
                    let type_param = type_param.clone();
                    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                    attributes.copy_traits = Some(type_param);
                }
                _ => {
                    return parse_point_error(
                        &data.tokens,
                        &UNKNOWN_NAME,
                        (format!("{kind_name} attribute"), format!("@{attr}")),
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
    name: Option<CHLIdent>,
    ty: HIRType,
) -> CHLResult<HIRField> {
    if try_next!(data.tokens, punctuator!(Colon)) {
        let TokenKind::IntLiteral(width) = next_kind!(data.tokens)? else {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                (
                    "a non-negative integer literal".into(),
                    Some("as bitfield width".into()),
                ),
            );
        };

        return Ok(HIRField::Bitfield {
            name: name.map(|name| name.to_string()),
            integer_type: ty,
            width: width.magnitude as usize
        });
    }

    let name = match name {
        Some(name) => name.to_string(),
        None if matches!(
            ty.kind,
            HIRTypeKind::Structured { name: None, .. } | HIRTypeKind::Union { name: None, .. }
        ) =>
        {
            format!("{ANONYMOUS_MEMBER_PREFIX}{}", data.tokens.index)
        }
        None => {
            return parse_point_error(
                &data.tokens,
                &UNSUPPORTED_FEATURE,
                (format!("nameless member of {ty}"), "the parser".into()),
            );
        }
    };

    Ok(HIRField::standard(name, ty))
}

fn parse_aggregate_fields(data: &mut ParserData, scope: TagScope) -> CHLResult<Vec<HIRField>> {
    let mut fields = Vec::new();
    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let (base, _) = parse_declaration_head(data, scope)?;
        loop {
            let (name, ty) = parse_declarator(data, &base, scope, DeclaratorKind::Field)?;
            fields.push(aggregate_field_from_decl(data, name, ty)?);
            if !try_next!(data.tokens, operator!(Comma)) {
                break;
            }
        }
        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
    }
    Ok(fields)
}

#[derive(Clone, Copy)]
pub(crate) enum TagScope {
    Module,
    Alias,
    Value,
}

impl TagScope {
    fn module(self, data: &ParserData) -> bool {
        matches!(self, Self::Module) || data.c_mode || data.in_include()
    }
}

fn tag_reference(name: QualifiedName, tag: HIRTagKind) -> HIRType {
    HIRTypeKind::Identifier {
        name,
        lookup: HIRTypeLookup::Tag(tag),
        args: None,
    }
    .to_type()
}

fn register_tag(
    data: &mut ParserData,
    name: CHLIdent,
    tag: HIRTagKind,
    ty: HIRType,
    range: TokenRange,
) {
    data.add_stmt(HIRStmt::TypeDefinition {
        symbol_naming: data.naming(LinkageMode::Standard),
        name: Some(name),
        visibility: data.visibility,
        ty,
        tag: Some(tag),
        range,
    });
}

pub(super) fn parse_tag(data: &mut ParserData, scope: TagScope) -> CHLResult<HIRType> {
    let start = data.tokens.index;
    let (tag, tagged) = match next_kind!(data.tokens)? {
        keyword!(Struct) => (HIRTagKind::Struct, false),
        keyword!(Union) => (HIRTagKind::Union, false),
        keyword!(Enum) if try_next!(data.tokens, keyword!(Union)) => (HIRTagKind::Union, true),
        keyword!(Enum) => (HIRTagKind::Enum, false),
        _ => unreachable!(),
    };
    let name = try_parse_qualified_name(&mut data.tokens)?;
    let header = data.token_range(start, data.tokens.index);
    let label = if tagged { "enum union" } else { tag.prefix() };
    let underlying = if tag == HIRTagKind::Enum {
        parse_enum_underlying(data)?
    } else {
        None
    };
    
    let attributes = if tag != HIRTagKind::Enum {
        parse_type_attributes(data, label)?
    } else {
        HIRAggregateAttributes::default()
    };

    if !try_next!(data.tokens, punctuator!(OpenBrace)) {
        if tagged || underlying.is_some() {
            return parse_point_error(&data.tokens, &EXPECTED_SYNTAX, ("'{'".into(), None));
        }
        let Some(name) = name else {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                ("a type name".into(), Some("for a predeclaration".into())),
            );
        };
        let ty = tag_reference(name.clone(), tag);
        if scope.module(data)
            && tag != HIRTagKind::Enum
            && (data.c_mode || data.in_include() || peek_kind!(data.tokens, punctuator!(Semicolon)))
        {
            if let Some(root) = name.root_name().filter(|root| !data.ast.definition_stmts.iter().any(|definition| {
                matches!(&definition.stmt, HIRStmt::TypeDefinition { name: Some(existing), tag: Some(kind), .. }
                    if existing == root && *kind == tag)
            })) {
                register_tag(data, root, tag, ty.clone(), header);
            }
        }
        return Ok(ty);
    }

    let name = match name {
        Some(name) => {
            let Some(root) = name.root_name() else {
                let args = (format!("an unqualified {label} name"), None);
                return parse_underline_error(&EXPECTED_SYNTAX, args, &header);
            };
            Some(root)
        }
        None => None,
    };
    let module = scope.module(data);
    if !module
        && (matches!(scope, TagScope::Value) && name.is_some()
            || matches!(scope, TagScope::Alias) && tag == HIRTagKind::Enum)
    {
        let feature = format!(
            "{} {label} definition",
            if name.is_some() {
                "a named"
            } else {
                "an anonymous"
            }
        );
        return parse_underline_error(
            &INVALID_CONTEXT,
            (feature, "a type expression".into()),
            &header,
        );
    }

    if tag == HIRTagKind::Enum {
        let variants = parse_enum_variants(data)?;
        let range = data.token_range(start, data.tokens.index);
        let ty = match &name {
            Some(name) => tag_reference(QualifiedName::new_raw(name.clone()), tag),
            None => underlying.clone().unwrap_or_else(|| {
                HIRTypeKind::Identifier {
                    name: QualifiedName::new_raw(CHLIdent::new("int")),
                    lookup: HIRTypeLookup::Standard,
                    args: None,
                }
                .to_type()
            }),
        };
        data.add_stmt(HIRStmt::GlobalVariableDefinition {
            visibility: data.visibility,
            variable: HIRGlobalVariable::EnumDefinition(HIREnumDefinition {
                name,
                underlying,
                variants,
                symbol_naming: data.naming(LinkageMode::Standard),
                range: range.clone(),
            }),
        });
        return Ok(ty.with_range(range));
    }

    let kind = if tagged {
        let variants = parse_tagged_variants(data, scope)?;
        HIRTypeKind::TaggedUnion {
            name: name.clone(),
            attributes,
            variants,
        }
    } else {
        let fields = parse_aggregate_fields(data, scope)?;
        if tag == HIRTagKind::Struct {
            HIRTypeKind::Structured {
                name: name.clone(),
                attributes,
                fields,
            }
        } else {
            HIRTypeKind::Union {
                name: name.clone(),
                attributes,
                fields,
            }
        }
    };

    let ty = kind
        .to_type()
        .with_range(data.token_range(start, data.tokens.index));
    let Some(name) = name else { return Ok(ty) };
    if module {
        let reference = tag_reference(QualifiedName::new_raw(name.clone()), tag);
        register_tag(data, name, tag, ty, header);
        Ok(reference)
    } else {
        Ok(HIRTypeKind::ScopedTag(tag, Box::new(ty)).to_type())
    }
}

fn parse_enum_underlying(data: &mut ParserData) -> CHLResult<Option<HIRType>> {
    let colon = data.tokens.index;
    if try_next!(data.tokens, punctuator!(Colon)) && starts_type(data, TypePosition::Cast)? {
        return parse_abstract_type(data, TagScope::Alias).map(Some);
    }
    data.tokens.index = colon;
    Ok(None)
}

fn parse_enum_variants(data: &mut ParserData) -> CHLResult<Vec<HIREnumVariant>> {
    let mut variants = Vec::new();
    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let start = data.tokens.index;
        let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                ("an enum variant".into(), None),
            );
        };
        let value = if try_next!(data.tokens, TokenKind::Assignment(None)) {
            data.change_comma_mode(false);
            let value = parse_expr(data);
            data.pop_comma_mode();
            Some(value?)
        } else {
            None
        };
        variants.push(HIREnumVariant {
            name,
            value,
            range: data.token_range(start, data.tokens.index),
        });
        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
            break;
        }
    }
    Ok(variants)
}

fn parse_tagged_variants(data: &mut ParserData, scope: TagScope) -> CHLResult<Vec<HIRField>> {
    let mut variants = Vec::new();
    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                ("a tagged union variant name".into(), None),
            );
        };
        assert_token_matches!(data.tokens, operator!(ScopeRes), "'::'");
        let (None, ty, _) = parse_typed_declarator(data, scope)? else {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                (
                    "an unnamed type".into(),
                    Some("for tagged union variant".into()),
                ),
            );
        };
        variants.push(HIRField::standard(name.to_string(), ty));
        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseBrace), "'}'");
            break;
        }
    }
    Ok(variants)
}
