use crate::parse::expressions::{parse_argument, parse_expr};
use crate::parse::{try_parse_simple_identifier, ParserData};
use crate::{
    assert_token_matches,
    log::parse_point_error,
    next_kind, peek_kind, try_next,
};
use chl_hir::ast::expression::HIRExpression;
use chl_hir::ast::{
    function::{HIRFunctionKind, HIRFunctionPrototype},
    modifiers::{
        HIRSymbolNameScheme, HIRTypeQualifiers, LinkageMode, HIR_CONST, HIR_RESTRICT, HIR_VOLATILE,
    },
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
};
use chl_log::catalogue::parse::*;
use chl_log::CHLResult;
use chl_namespace::module::QualifiedName;
use chl_hir::intrinsic_types::is_intrinsic_type;
use chl_tokens::token::{AttributeType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use chl_tokens::{
    identifier, intrinsic, keyword, operator, punctuator, specifier, TokenIter, TokenRange,
};
use chl_util::identifier::CHLIdent;

use crate::parse::functions::{parse_params, ParseParamsResult};
use crate::parse::{parse_intrinsic, try_parse_qualified_name};

pub(super) enum TypePosition {
    Statement,
    Expression,
}

pub(super) fn starts_type(data: &mut ParserData, position: TypePosition) -> CHLResult<bool> {
    fn is_used_as_value(data: &ParserData) -> bool {
        matches!(
            data.tokens
                .slice
                .get(data.tokens.index + 1)
                .map(|token| &token.kind),
            Some(
                TokenKind::Assignment(_)
                    | TokenKind::Operator(OperatorType::Access | OperatorType::Arrow)
            )
        )
    }

    let Some(token) = data.tokens.peek() else {
        return Ok(false);
    };

    Ok(match &token.kind {
        intrinsic!() | specifier!() | keyword!(Struct, Union, Enum, Register) => true,

        keyword!(Type, Fn) => true,
        TokenKind::CompilerIdentifier(name) if name == "strong" => !data.c_mode && !data.in_include(),
        operator!(Caret) => !data.c_mode,

        identifier!(name) if is_intrinsic_type(name) => !is_used_as_value(data),

        identifier!() => {
            let start = data.tokens.index;
            let name = try_parse_qualified_name(&mut data.tokens);
            let end = std::mem::replace(&mut data.tokens.index, start);
            let name = name?.expect("an identifier starts a qualified name");

            if data.is_generator(&name)? {
                let scoped = matches!(position, TypePosition::Statement)
                    && matches!(application_suffix(&data.tokens, end), Some(operator!(ScopeRes)));
                !scoped && matches!(data.tokens.slice.get(end).map(|token| &token.kind), Some(punctuator!(OpenParen)))
            } else {
                is_unimported_generator(data, &name, end)?
                    || (data.is_type_ident(&name)? && !is_used_as_value(data))
            }
        }

        _ => false,
    })
}

// No expression applies a name and then names another, so an undeclared name applied before a
// name is a generator from a module that was not imported
fn is_unimported_generator(data: &ParserData, name: &QualifiedName, end: usize) -> CHLResult<bool> {
    Ok(!data.c_mode
        && data.is_undeclared(name)?
        && matches!(application_suffix(&data.tokens, end), Some(identifier!())))
}

pub(super) fn application_suffix<'a>(tokens: &TokenIter<'a>, open: usize) -> Option<&'a TokenKind> {
    if !matches!(tokens.slice.get(open)?.kind, punctuator!(OpenParen)) {
        return None;
    }
    let mut depth = 0usize;
    for (index, token) in tokens.slice.iter().enumerate().skip(open) {
        match &token.kind {
            punctuator!(OpenParen) => depth += 1,
            punctuator!(CloseParen) => {
                depth -= 1;
                if depth == 0 {
                    return tokens.slice.get(index + 1).map(|token| &token.kind);
                }
            }
            _ => {}
        }
    }
    None
}

fn token_range(data: &ParserData, start: usize, end: usize) -> TokenRange {
    data.token_range(start, end.max(start.saturating_add(1)))
}

pub(crate) fn parse_specifier(tokens: &mut TokenIter) -> HIRTypeQualifiers {
    parse_decl_specifiers(tokens).qualifiers
}

#[derive(Clone, Copy)]
pub(crate) struct ParsedSpecifiers {
    pub(crate) qualifiers: HIRTypeQualifiers,
    pub(crate) linkage: LinkageMode,
    pub(crate) attributes: DeclarationAttributes,
}

/// Attributes attached to a declaration, gathered from `TokenKind::Attribute` tokens.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct DeclarationAttributes {
    pub(crate) noreturn: bool,
}

impl DeclarationAttributes {
    pub(crate) fn record(&mut self, attribute: AttributeType) {
        match attribute {
            AttributeType::Noreturn => self.noreturn = true,
        }
    }

    pub(crate) fn merge(self, other: DeclarationAttributes) -> DeclarationAttributes {
        DeclarationAttributes {
            noreturn: self.noreturn || other.noreturn,
        }
    }

    /// A function that never returns is declared as returning `unreachable`,
    /// regardless of the return type written in the source.
    pub(crate) fn apply_to_return_type(&self, return_type: HIRType) -> HIRType {
        if !self.noreturn {
            return return_type;
        }

        let mut unreachable = HIRTypeKind::Identifier {
            name: QualifiedName::root(CHLIdent::new("unreachable")),
            lookup: HIRTypeLookup::Standard,
            args: None,
        }
        .to_type();
        unreachable.range = return_type.range;
        unreachable
    }
}

pub(crate) fn parse_attributes(tokens: &mut TokenIter, attributes: &mut DeclarationAttributes) {
    while let Some(TokenKind::Attribute(attribute)) = tokens.peek().map(|token| &token.kind) {
        attributes.record(*attribute);
        tokens.next();
    }
}

pub(crate) fn parse_decl_specifiers(tokens: &mut TokenIter) -> ParsedSpecifiers {
    let mut spec_acc: HIRTypeQualifiers = 0;
    let mut linkage = LinkageMode::Standard;
    let mut attributes = DeclarationAttributes::default();

    loop {
        match next_kind!(tokens) {
            Ok(TokenKind::Specifier(spec)) => match spec {
                SpecifierType::Const => spec_acc |= HIR_CONST,
                SpecifierType::Volatile => spec_acc |= HIR_VOLATILE,
                SpecifierType::Restrict => spec_acc |= HIR_RESTRICT,
                SpecifierType::Extern => linkage = LinkageMode::Extern,
                SpecifierType::Static => linkage = LinkageMode::Static,
                SpecifierType::Inline | SpecifierType::ThreadLocal => {}

                SpecifierType::Public | SpecifierType::Private => break,
            },
            Ok(TokenKind::Attribute(attribute)) => attributes.record(*attribute),
            _ => break,
        }
    }

    tokens.back();
    ParsedSpecifiers {
        qualifiers: spec_acc,
        linkage,
        attributes,
    }
}

// With 'named' unset only the pointer and reference suffixes of an abstract type are read
fn parse_declarator_mods(
    data: &mut ParserData,
    acc_type: HIRType,
    named: bool,
    scope: TagScope,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
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

            parse_declarator_mods(data, acc_type, named, scope)
        }

        operator!(Asterisk) => {
            data.tokens.next();
            let specs = parse_specifier(&mut data.tokens);
            let acc_type = acc_type.pointer_to(specs);

            parse_declarator_mods(data, acc_type, named, scope)
        }

        punctuator!(Apostrophe) => {
            data.tokens.next();

            let lifetime_kind = next_kind!(data.tokens)?.clone();
            let lifetime = match lifetime_kind {
                TokenKind::Identifier(lifetime) => CHLIdent::from(lifetime.as_str()),
                keyword!(Hole) => CHLIdent::new("_"),
                TokenKind::Specifier(SpecifierType::Static) => CHLIdent::new("static"),
                _ => {
                    return parse_point_error(
                        &data.tokens,
                        &EXPECTED_SYNTAX,
                        ("a lifetime identifier".into(), None),
                    );
                }
            };

            assert_token_matches!(data.tokens, operator!(Ampersand), "'&'");

            let range = acc_type.range.clone();
            let ref_type = HIRTypeKind::MemoryReference {
                inner_type: Box::new(acc_type),
                lifetime: Some(lifetime),
            }
            .to_type()
            .with_range(range);

            parse_declarator_mods(data, ref_type, named, scope)
        }

        operator!(Ampersand) => {
            data.tokens.next();

            let range = acc_type.range.clone();
            let ref_type = HIRTypeKind::MemoryReference {
                inner_type: Box::new(acc_type),
                lifetime: None,
            }
            .to_type()
            .with_range(range);

            parse_declarator_mods(data, ref_type, named, scope)
        }

        punctuator!(OpenParen) if named => {
            data.tokens.next();
            if peek_kind!(data.tokens, identifier!())
                && matches!(
                    data.tokens
                        .slice
                        .get(data.tokens.index + 1)
                        .map(|token| &token.kind),
                    Some(punctuator!(CloseParen))
                )
            {
                let name = try_parse_simple_identifier(&mut data.tokens);
                data.tokens.next();
                return Ok((name, acc_type));
            }
            if !matches!(next_kind!(data.tokens), Ok(operator!(Asterisk))) {
                data.tokens.index = start_index;
                return Ok((None, acc_type));
            }
            let name = try_parse_simple_identifier(&mut data.tokens);

            let bounds = parse_array_bounds(data)?;

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
            } = parse_params(data, scope)?;

            let prototype = HIRFunctionPrototype {
                kind: HIRFunctionKind::Standard(CHLIdent::new("__internal_fnptr")),
                comptime: false,
                return_type: acc_type,
                params,
                var_args,
                contract,
                linkage: LinkageMode::Standard,
                symbol_naming: HIRSymbolNameScheme::Namespaced,
                range: TokenRange::internal(),
            };

            let fn_ptr_type = HIRTypeKind::Function {
                prototype: Box::new(prototype),
                c_form: true,
            }
            .to_type()
            .pointer_to(0);

            Ok((name, array_type(fn_ptr_type, bounds)))
        }

        identifier!() if named => {
            let Some(name) = try_parse_simple_identifier(&mut data.tokens) else {
                unreachable!();
            };

            Ok((Some(name), acc_type))
        }

        _ => Ok((None, acc_type)),
    }
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

pub(crate) fn parse_type_base(data: &mut ParserData, scope: TagScope) -> CHLResult<HIRType> {
    let start_index = data.tokens.index;
    let Some(next_token) = data.tokens.peek() else {
        return parse_point_error(&data.tokens, &UNEXPECTED_END, Some("type".into()));
    };

    let ty = match &next_token.kind {
        identifier!() => {
            let Some(name) = try_parse_qualified_name(&mut data.tokens)? else {
                unreachable!();
            };

            let end = data.tokens.index;
            let args = if peek_kind!(data.tokens, punctuator!(OpenParen))
                && (data.is_generator(&name)? || is_unimported_generator(data, &name, end)?)
            {
                Some(parse_args(data)?)
            } else {
                None
            };

            Ok(HIRTypeKind::Identifier {
                name,
                lookup: HIRTypeLookup::Standard,
                args,
            }
            .to_type())
        }

        intrinsic!(Auto) if !data.c_mode => {
            data.tokens.next();
            Ok(HIRTypeKind::Auto.to_type())
        }

        keyword!(Type) => {
            data.tokens.next();
            Ok(HIRTypeKind::Universe.to_type())
        }

        keyword!(Fn) => {
            data.tokens.next();
            Ok(fn_declarator_marker())
        }

        TokenKind::CompilerIdentifier(name) if name == "strong" && !data.c_mode && !data.in_include() => {
            data.tokens.next();
            Ok(HIRTypeKind::Strong(Box::new(parse_abstract_type(data, scope)?)).to_type())
        }

        // '^' takes the whole abstract type after it, so '^const T&' stages a 'const T&'
        operator!(Caret) if !data.c_mode => {
            data.tokens.next();
            Ok(HIRTypeKind::Staged(Box::new(parse_abstract_type(data, scope)?)).to_type())
        }

        intrinsic!() => Ok(HIRTypeKind::Identifier {
            name: QualifiedName::new_raw(parse_intrinsic(&mut data.tokens)?),
            lookup: HIRTypeLookup::Standard,
            args: None,
        }
        .to_type()),

        keyword!(Struct, Enum, Union) => parse_tag(data, scope),

        _ => {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                ("a type".into(), None),
            );
        }
    };

    let specifiers = parse_specifier(&mut data.tokens);

    Ok(ty?
        .add_specifier(specifiers)
        .with_range(token_range(data, start_index, data.tokens.index)))
}

// The parenthesized arguments of a call or of a generator, 'opt(int)'
pub(crate) fn parse_args(data: &mut ParserData) -> CHLResult<Vec<HIRExpression>> {
    fn arguments(data: &mut ParserData) -> CHLResult<Vec<HIRExpression>> {
        let mut args = Vec::new();

        while !try_next!(data.tokens, punctuator!(CloseParen)) {
            args.push(parse_argument(data)?);

            if !try_next!(data.tokens, operator!(Comma)) {
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                break;
            }
        }

        Ok(args)
    }

    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
    data.change_comma_mode(false);
    let args = arguments(data);
    data.pop_comma_mode();
    args
}

// 'fn' is parsed as a base type whose declarator carries the signature, its indirections and
// name sitting where C puts them: 'fn* table[4](int) -> int'. Bare 'fn' is the function type itself
fn fn_declarator_marker() -> HIRType {
    HIRTypeKind::Identifier {
        name: QualifiedName::new_raw(CHLIdent::new("@fn")),
        lookup: HIRTypeLookup::Standard,
        args: None,
    }
    .to_type()
}

fn is_fn_declarator_marker(ty: &HIRType) -> bool {
    matches!(
        &ty.kind,
        HIRTypeKind::Identifier { name, args: None, .. }
            if name.namespace.is_root() && name.name.as_str() == "@fn"
    )
}

fn parse_fn_signature(data: &mut ParserData, scope: TagScope) -> CHLResult<HIRType> {
    let start_index = data.tokens.index;
    let ParseParamsResult {
        params,
        var_args,
        contract,
        ..
    } = parse_params(data, scope)?;
    assert_token_matches!(data.tokens, operator!(Arrow), "'->' after 'fn' parameters");
    let return_type = parse_abstract_type(data, scope)?;

    let prototype = HIRFunctionPrototype {
        kind: HIRFunctionKind::Standard(CHLIdent::new("__internal_fnptr")),
        comptime: false,
        return_type,
        params,
        var_args,
        contract,
        linkage: LinkageMode::Standard,
        symbol_naming: HIRSymbolNameScheme::Namespaced,
        range: TokenRange::internal(),
    };

    Ok(HIRTypeKind::Function {
        prototype: Box::new(prototype),
        c_form: false,
    }
    .to_type()
    .with_range(token_range(data, start_index, data.tokens.index)))
}

enum FnIndirection {
    Pointer(HIRTypeQualifiers),
    Reference,
}

fn parse_fn_indirections(data: &mut ParserData) -> Vec<FnIndirection> {
    let mut indirections = Vec::new();
    loop {
        if try_next!(data.tokens, operator!(Asterisk)) {
            indirections.push(FnIndirection::Pointer(parse_specifier(&mut data.tokens)));
        } else if try_next!(data.tokens, operator!(Ampersand)) {
            indirections.push(FnIndirection::Reference);
        } else {
            return indirections;
        }
    }
}

// Indirections apply innermost first, as in a C declarator: 'fn*& f' is a reference to a pointer
fn parse_fn_type(
    data: &mut ParserData,
    indirections: Vec<FnIndirection>,
    scope: TagScope,
) -> CHLResult<HIRType> {
    let function = parse_fn_signature(data, scope)?;
    let range = function.range.clone();
    Ok(indirections
        .into_iter()
        .fold(function, |inner, indirection| match indirection {
            FnIndirection::Pointer(specifiers) => inner.pointer_to(specifiers),
            FnIndirection::Reference => HIRTypeKind::MemoryReference {
                inner_type: Box::new(inner),
                lifetime: None,
            }
            .to_type(),
        })
        .with_range(range))
}

fn parse_fn_declarator(
    data: &mut ParserData,
    scope: TagScope,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    let indirections = parse_fn_indirections(data);
    let name = try_parse_simple_identifier(&mut data.tokens);

    let bounds = parse_array_bounds(data)?;

    if !peek_kind!(data.tokens, punctuator!(OpenParen)) {
        return parse_point_error(
            &data.tokens,
            &EXPECTED_SYNTAX,
            ("a parameter list".into(), Some("after 'fn'".into())),
        );
    }

    let function = parse_fn_type(data, indirections, scope)?;

    // An anonymous 'fn' type only takes a name as the return type of a function declarator
    if name.is_none() && bounds.is_empty() {
        let Some(trailing) = try_parse_simple_identifier(&mut data.tokens) else {
            return Ok((None, function));
        };
        if peek_kind!(data.tokens, punctuator!(OpenParen)) {
            return Ok((Some(trailing), function));
        }
        data.tokens.back();
        return parse_point_error(&data.tokens, &FN_TRAILING_NAME, trailing.to_string());
    }

    Ok((name, array_type(function, bounds)))
}

// A type with no declarator name: the base and its pointer and reference suffixes
pub(crate) fn parse_abstract_type(data: &mut ParserData, scope: TagScope) -> CHLResult<HIRType> {
    let specifiers = parse_decl_specifiers(&mut data.tokens);
    let base = parse_type_base(data, scope)?.add_specifier(specifiers.qualifiers);

    if is_fn_declarator_marker(&base) {
        let indirections = parse_fn_indirections(data);
        return parse_fn_type(data, indirections, scope);
    }

    let ty = parse_declarator_mods(data, base, false, scope)?.1;
    Ok(array_type(ty, parse_array_bounds(data)?))
}

pub(crate) fn parse_base_mods(
    data: &mut ParserData,
    acc_type: HIRType,
    scope: TagScope,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    if is_fn_declarator_marker(&acc_type) {
        return parse_fn_declarator(data, scope);
    }

    let (name, modified_type) = parse_declarator_mods(data, acc_type, true, scope)?;

    Ok((name, array_type(modified_type, parse_array_bounds(data)?)))
}

pub(crate) fn parse_initializer(
    data: &mut ParserData,
    scope: TagScope,
) -> CHLResult<(Option<CHLIdent>, HIRType, ParsedSpecifiers)> {
    let mut prefix_specs = parse_decl_specifiers(&mut data.tokens);
    let type_base = parse_type_base(data, scope)?;
    parse_attributes(&mut data.tokens, &mut prefix_specs.attributes);

    let (name, ty) = parse_named_declarator(
        data,
        type_base.add_specifier(prefix_specs.qualifiers),
        scope,
    )?;
    Ok((name, ty, prefix_specs))
}

pub(crate) fn parse_named_declarator(
    data: &mut ParserData,
    base: HIRType,
    scope: TagScope,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    let (name, mut ty) = parse_base_mods(data, base, scope)?;
    if !data.c_mode && name.is_some() && matches!(ty.kind, HIRTypeKind::Auto)
        && try_next!(data.tokens, punctuator!(Colon))
    {
        ty = parse_abstract_type(data, scope)?.add_specifier(ty.specifiers);
    }
    Ok((name, ty))
}

pub(crate) fn parse_typedef_initializer(
    data: &mut ParserData,
) -> CHLResult<(Option<CHLIdent>, HIRType)> {
    let (name, return_type, specifiers) = parse_initializer(data, TagScope::Module)?;

    if name.is_none() || !peek_kind!(data.tokens, punctuator!(OpenParen)) {
        return Ok((name, return_type));
    }

    let ParseParamsResult {
        params,
        var_args,
        contract,
        attributes,
    } = parse_params(data, TagScope::Module)?;

    let prototype = HIRFunctionPrototype {
        kind: HIRFunctionKind::Standard(CHLIdent::new("__internal_fnptr")),
        comptime: false,
        return_type: specifiers
            .attributes
            .merge(attributes)
            .apply_to_return_type(return_type),
        params,
        var_args,
        contract,
        linkage: LinkageMode::Standard,
        symbol_naming: HIRSymbolNameScheme::Namespaced,
        range: TokenRange::internal(),
    };

    Ok((
        name,
        HIRTypeKind::Function {
            prototype: Box::new(prototype),
            c_form: true,
        }
        .to_type(),
    ))
}

mod tags;
use tags::parse_tag;
pub(crate) use tags::TagScope;
