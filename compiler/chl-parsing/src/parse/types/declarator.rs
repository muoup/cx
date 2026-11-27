use chl_hir::ast::{
    expression::HIRExpression,
    function::{HIRFunctionKind, HIRFunctionPrototype},
    modifiers::{HIRSymbolNameScheme, HIRTypeQualifiers, LinkageMode},
    types::{HIRType, HIRTypeKind},
};
use chl_log::{catalogue::parse::*, CHLResult};
use chl_tokens::{
    identifier, keyword, operator, punctuator, specifier, token::TokenKind, TokenRange,
};
use chl_util::identifier::CHLIdent;

use super::{parse_decl_specifiers, parse_specifier, parse_type_base, ParsedSpecifiers, TagScope};
use crate::{
    assert_token_matches,
    log::parse_point_error,
    next_kind,
    parse::{
        expressions::parse_expr, functions::parse_params, try_parse_simple_identifier, ParserData,
    },
    peek_kind, try_next,
};

pub(crate) enum DeclaratorBase {
    Type(HIRType),
    Fn,
}

#[derive(Clone, Copy)]
pub(crate) enum DeclaratorKind {
    Named,
    Abstract,
    AsTarget,
    Field,
}

pub(crate) fn parse_declaration_head(
    data: &mut ParserData,
    scope: TagScope,
) -> CHLResult<(DeclaratorBase, ParsedSpecifiers)> {
    let mut specifiers = parse_decl_specifiers(&mut data.tokens);
    let start = data.tokens.index;
    let base = if try_next!(data.tokens, keyword!(Fn)) {
        DeclaratorBase::Fn
    } else {
        DeclaratorBase::Type(parse_type_base(data, scope)?)
    };
    let suffix = parse_decl_specifiers(&mut data.tokens);
    specifiers.noreturn |= suffix.noreturn;
    let base = match base {
        DeclaratorBase::Type(ty) => DeclaratorBase::Type(
            ty.add_specifier(specifiers.qualifiers | suffix.qualifiers)
                .with_range(data.token_range(start, data.tokens.index)),
        ),
        DeclaratorBase::Fn => DeclaratorBase::Fn,
    };
    Ok((base, specifiers))
}

pub(crate) fn parse_declarator(
    data: &mut ParserData,
    base: &DeclaratorBase,
    scope: TagScope,
    kind: DeclaratorKind,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    let named = matches!(kind, DeclaratorKind::Named | DeclaratorKind::Field);
    let DeclaratorBase::Type(ty) = base else {
        return parse_native_fn(data, scope, named);
    };
    let mut ty = ty.clone();
    while !(matches!(kind, DeclaratorKind::AsTarget) && indirection_is_infix(data)) {
        let Some(indirection) = parse_indirection(data, true)? else {
            break;
        };
        ty = indirection.apply(ty);
    }

    let mut name = None;
    let open = data.tokens.index;
    if try_next!(data.tokens, punctuator!(OpenParen)) {
        if try_next!(data.tokens, operator!(Asterisk)) {
            name = named
                .then(|| try_parse_simple_identifier(&mut data.tokens))
                .flatten();
            let bounds = parse_array_bounds(data)?;
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            ty = parse_function_type(data, scope, Some(ty), false)?.pointer_to(0);
            ty = array_type(ty, bounds);
        } else if named
            && peek_kind!(data.tokens, identifier!())
            && matches!(
                data.tokens
                    .slice
                    .get(data.tokens.index + 1)
                    .map(|token| &token.kind),
                Some(punctuator!(CloseParen))
            )
        {
            name = try_parse_simple_identifier(&mut data.tokens);
            data.tokens.next();
        } else {
            data.tokens.index = open;
        }
    } else if named {
        name = try_parse_simple_identifier(&mut data.tokens);
    }

    ty = array_type(ty, parse_array_bounds(data)?);
    if matches!(kind, DeclaratorKind::Named)
        && !data.c_mode
        && name.is_some()
        && matches!(ty.kind, HIRTypeKind::Auto)
        && try_next!(data.tokens, punctuator!(Colon))
    {
        ty = parse_abstract_type(data, scope)?.add_specifier(ty.specifiers);
    }
    Ok((name, ty))
}

fn indirection_is_infix(data: &ParserData) -> bool {
    let [operator, operand, tail @ ..] = &data.tokens.slice[data.tokens.index..] else {
        return false;
    };
    if !matches!(operator.kind, operator!(Asterisk) | operator!(Ampersand)) {
        return false;
    }
    match operand.kind {
        identifier!()
        | TokenKind::IntLiteral(_)
        | TokenKind::FloatLiteral(_)
        | TokenKind::StringLiteral(_)
        | TokenKind::CompilerIdentifier(_) => true,
        punctuator!(OpenParen) => {
            !matches!(tail.first().map(|token| &token.kind), Some(operator!(Asterisk)))
        }
        _ => false,
    }
}

pub(crate) fn parse_abstract_type(data: &mut ParserData, scope: TagScope) -> CHLResult<HIRType> {
    let (base, _) = parse_declaration_head(data, scope)?;
    Ok(parse_declarator(data, &base, scope, DeclaratorKind::Abstract)?.1)
}

pub(crate) fn parse_typed_declarator(
    data: &mut ParserData,
    scope: TagScope,
) -> CHLResult<(Option<CHLIdent>, HIRType, ParsedSpecifiers)> {
    let (base, specifiers) = parse_declaration_head(data, scope)?;
    let (name, ty) = parse_declarator(data, &base, scope, DeclaratorKind::Named)?;
    Ok((name, ty, specifiers))
}

pub(crate) fn parse_typedef_declarator(
    data: &mut ParserData,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    let (name, mut ty, specifiers) = parse_typed_declarator(data, TagScope::Module)?;
    if name.is_some() && peek_kind!(data.tokens, punctuator!(OpenParen)) {
        ty = parse_function_type(data, TagScope::Module, Some(ty), specifiers.noreturn)?;
    }
    Ok((name, ty))
}

enum Indirection {
    Pointer(HIRTypeQualifiers),
    Reference(Option<CHLIdent>),
}

impl Indirection {
    fn apply(self, ty: HIRType) -> HIRType {
        match self {
            Self::Pointer(qualifiers) => ty.pointer_to(qualifiers),
            Self::Reference(lifetime) => {
                let range = ty.range.clone();
                HIRTypeKind::MemoryReference {
                    inner_type: Box::new(ty),
                    lifetime,
                }
                .to_type()
                .with_range(range)
            }
        }
    }
}

fn parse_indirection(data: &mut ParserData, extended: bool) -> CHLResult<Option<Indirection>> {
    Ok(Some(
        if extended && try_next!(data.tokens, punctuator!(Apostrophe)) {
            let lifetime = match next_kind!(data.tokens)? {
                identifier!(name) => CHLIdent::from(name.as_str()),
                keyword!(Hole) => CHLIdent::new("_"),
                specifier!(Static) => CHLIdent::new("static"),
                _ => {
                    return parse_point_error(
                        &data.tokens,
                        &EXPECTED_SYNTAX,
                        ("a lifetime identifier".into(), None),
                    )
                }
            };
            assert_token_matches!(data.tokens, operator!(Ampersand), "'&'");
            Indirection::Reference(Some(lifetime))
        } else if try_next!(data.tokens, operator!(Ampersand)) {
            Indirection::Reference(None)
        } else if try_next!(data.tokens, operator!(Asterisk)) {
            Indirection::Pointer(parse_specifier(&mut data.tokens))
        } else if extended && try_next!(data.tokens, keyword!(Weak)) {
            assert_token_matches!(data.tokens, operator!(Asterisk), "'*'");
            Indirection::Pointer(parse_specifier(&mut data.tokens))
        } else {
            return Ok(None);
        },
    ))
}

fn parse_array_bounds(data: &mut ParserData) -> CHLResult<Vec<Option<HIRExpression>>> {
    let mut bounds = Vec::new();
    while try_next!(data.tokens, punctuator!(OpenBracket)) {
        if try_next!(data.tokens, punctuator!(CloseBracket)) {
            bounds.push(None);
        } else {
            bounds.push(Some(parse_expr(data)?));
            assert_token_matches!(data.tokens, punctuator!(CloseBracket), "']'");
        }
    }
    Ok(bounds)
}

fn array_type(base: HIRType, bounds: Vec<Option<HIRExpression>>) -> HIRType {
    bounds.into_iter().rev().fold(base, |inner, size| {
        let range = inner.range.clone();
        let inner = Box::new(inner);
        match size {
            Some(size) => HIRTypeKind::ExplicitSizedArray(inner, Box::new(size)),
            None => HIRTypeKind::ImplicitSizedArray(inner),
        }
        .to_type()
        .with_range(range)
    })
}

fn parse_native_fn(
    data: &mut ParserData,
    scope: TagScope,
    named: bool,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    let mut indirections = Vec::new();
    while let Some(indirection) = parse_indirection(data, false)? {
        indirections.push(indirection);
    }
    let mut name = named
        .then(|| try_parse_simple_identifier(&mut data.tokens))
        .flatten();
    let bounds = if named {
        parse_array_bounds(data)?
    } else {
        Vec::new()
    };
    if !peek_kind!(data.tokens, punctuator!(OpenParen)) {
        return parse_point_error(
            &data.tokens,
            &EXPECTED_SYNTAX,
            ("a parameter list".into(), Some("after 'fn'".into())),
        );
    }
    let function = parse_function_type(data, scope, None, false)?;
    let ty = indirections
        .into_iter()
        .fold(function, |ty, op| op.apply(ty));

    if named && name.is_none() && bounds.is_empty() {
        if let Some(trailing) = try_parse_simple_identifier(&mut data.tokens) {
            if !peek_kind!(data.tokens, punctuator!(OpenParen)) {
                data.tokens.back();
                return parse_point_error(&data.tokens, &FN_TRAILING_NAME, trailing.to_string());
            }
            name = Some(trailing);
        }
    }
    Ok((name, array_type(ty, bounds)))
}

fn parse_function_type(
    data: &mut ParserData,
    scope: TagScope,
    return_type: Option<HIRType>,
    noreturn: bool,
) -> CHLResult<HIRType> {
    let start = data.tokens.index;
    let args = parse_params(data, scope)?;
    let c_form = return_type.is_some();
    let return_type = match return_type {
        Some(ty) => ty,
        None => {
            assert_token_matches!(data.tokens, operator!(Arrow), "'->' after 'fn' parameters");
            parse_abstract_type(data, scope)?
        }
    };
    let prototype = HIRFunctionPrototype {
        kind: HIRFunctionKind::Standard(CHLIdent::new("__internal_fnptr")),
        comptime: false,
        return_type,
        noreturn: noreturn || args.noreturn,
        params: args.params,
        var_args: args.var_args,
        contract: args.contract,
        linkage: LinkageMode::Standard,
        symbol_naming: HIRSymbolNameScheme::Namespaced,
        range: TokenRange::internal(),
    };
    let ty = HIRTypeKind::Function {
        prototype: Box::new(prototype),
        c_form,
    }
    .to_type();
    Ok(if c_form {
        ty
    } else {
        ty.with_range(data.token_range(start, data.tokens.index))
    })
}

pub(crate) fn parse_variable_initializer(
    data: &mut ParserData,
) -> CHLResult<Option<HIRExpression>> {
    if !try_next!(data.tokens, TokenKind::Assignment(None)) {
        return Ok(None);
    }

    data.change_comma_mode(false);
    let initializer = parse_expr(data);
    data.pop_comma_mode();
    initializer.map(Some)
}
