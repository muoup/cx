use crate::parse::{expressions::parse_argument, ParserData};
use crate::{assert_token_matches, log::parse_point_error, peek_kind, try_next};
use chl_hir::ast::expression::HIRExpression;
use chl_hir::ast::{
    modifiers::{HIRTypeQualifiers, LinkageMode, HIR_CONST, HIR_RESTRICT, HIR_VOLATILE},
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
};
use chl_hir::intrinsic_types::is_intrinsic_type;
use chl_log::catalogue::parse::*;
use chl_log::CHLResult;
use chl_namespace::module::QualifiedName;
use chl_tokens::token::{AttributeType, SpecifierType, TokenKind};
use chl_tokens::{identifier, intrinsic, keyword, operator, punctuator, specifier, TokenIter};

use crate::parse::{parse_intrinsic, parser::LocalName, try_parse_qualified_name};

pub(super) enum TypePosition {
    Statement,
    Expression,
}

pub(super) fn starts_type(data: &mut ParserData, position: TypePosition) -> CHLResult<bool> {
    let Some(token) = data.tokens.peek() else {
        return Ok(false);
    };

    Ok(match &token.kind {
        intrinsic!() | specifier!() | keyword!(Struct, Union, Enum, Register) => true,

        keyword!(Type, Fn) => true,
        TokenKind::CompilerIdentifier(name) if name == "strong" => {
            !data.c_mode && !data.in_include()
        }
        operator!(Caret) => !data.c_mode,

        identifier!() => {
            let start = data.tokens.index;
            let name = try_parse_qualified_name(&mut data.tokens);
            let end = std::mem::replace(&mut data.tokens.index, start);
            let name = name?.expect("an identifier starts a qualified name");
            let following = data.tokens.slice.get(end).map(|token| &token.kind);

            if matches!(
                following,
                Some(TokenKind::Assignment(_) | operator!(Access) | operator!(Arrow))
            ) {
                return Ok(false);
            }
            if type_application_before_declarator(data, end) {
                return Ok(true);
            }
            if let Some(kind) = data.local_name(&name) {
                return Ok(kind == LocalName::Type);
            }
            if name
                .root_name_ref()
                .is_some_and(|root| is_intrinsic_type(root.as_str()))
            {
                return Ok(true);
            }
            if data.is_generator(&name)? {
                let scoped = matches!(position, TypePosition::Statement)
                    && matches!(
                        application_suffix(&data.tokens, end),
                        Some(operator!(ScopeRes))
                    );
                !scoped && matches!(following, Some(punctuator!(OpenParen)))
            } else {
                data.is_module_type(&name)?
            }
        }

        _ => false,
    })
}

fn type_application_before_declarator(data: &ParserData, end: usize) -> bool {
    !data.c_mode && matches!(application_suffix(&data.tokens, end), Some(identifier!()))
}

fn is_type_application(data: &ParserData, name: &QualifiedName) -> CHLResult<bool> {
    if data.is_generator(name)? {
        return Ok(true);
    }
    if !type_application_before_declarator(data, data.tokens.index) {
        return Ok(false);
    }
    match data.local_name(name) {
        Some(kind) => Ok(kind == LocalName::Value),
        None => Ok(!data.is_module_type(name)?),
    }
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

fn parse_specifier(tokens: &mut TokenIter) -> HIRTypeQualifiers {
    parse_decl_specifiers(tokens).qualifiers
}

#[derive(Clone, Copy)]
pub(crate) struct ParsedSpecifiers {
    pub(crate) qualifiers: HIRTypeQualifiers,
    pub(crate) linkage: LinkageMode,
    pub(crate) noreturn: bool,
}

pub(crate) fn parse_noreturn(tokens: &mut TokenIter) -> bool {
    let mut noreturn = false;
    while try_next!(tokens, TokenKind::Attribute(AttributeType::Noreturn)) {
        noreturn = true;
    }
    noreturn
}

fn parse_decl_specifiers(tokens: &mut TokenIter) -> ParsedSpecifiers {
    let mut spec_acc: HIRTypeQualifiers = 0;
    let mut linkage = LinkageMode::Standard;
    let mut noreturn = false;

    loop {
        match tokens.peek().map(|token| &token.kind) {
            Some(TokenKind::Specifier(spec)) => match spec {
                SpecifierType::Const => spec_acc |= HIR_CONST,
                SpecifierType::Volatile => spec_acc |= HIR_VOLATILE,
                SpecifierType::Restrict => spec_acc |= HIR_RESTRICT,
                SpecifierType::Extern => linkage = LinkageMode::Extern,
                SpecifierType::Static => linkage = LinkageMode::Static,
                SpecifierType::Inline | SpecifierType::ThreadLocal => {}

                SpecifierType::Public | SpecifierType::Private => break,
            },
            Some(TokenKind::Attribute(AttributeType::Noreturn)) => noreturn = true,
            _ => break,
        }
        tokens.next();
    }
    ParsedSpecifiers {
        qualifiers: spec_acc,
        linkage,
        noreturn,
    }
}

fn parse_type_base(data: &mut ParserData, scope: TagScope) -> CHLResult<HIRType> {
    let Some(next_token) = data.tokens.peek() else {
        return parse_point_error(&data.tokens, &UNEXPECTED_END, Some("type".into()));
    };

    match &next_token.kind {
        identifier!() => {
            let Some(name) = try_parse_qualified_name(&mut data.tokens)? else {
                unreachable!();
            };

            let args = if peek_kind!(data.tokens, punctuator!(OpenParen))
                && is_type_application(data, &name)?
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

        TokenKind::CompilerIdentifier(name)
            if name == "strong" && !data.c_mode && !data.in_include() =>
        {
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

        _ => parse_point_error(&data.tokens, &EXPECTED_SYNTAX, ("a type".into(), None)),
    }
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

mod declarator;
pub(crate) use declarator::{
    parse_abstract_type, parse_declaration_head, parse_declarator, parse_typed_declarator,
    parse_typedef_declarator, parse_variable_initializer, DeclaratorKind,
};

mod tags;
use tags::parse_tag;
pub(crate) use tags::TagScope;
