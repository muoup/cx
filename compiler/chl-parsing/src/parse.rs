use chl_hir::ast::{
    expression::{HIRBlockKind, HIRExprKind, HIRExpression},
    function::{HIRFunctionBody, HIRFunctionPrototype},
    global_var::HIRGlobalVariable,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::HIRTypeKind,
    HIRStmt,
};
use chl_log::catalogue::parse::*;
use chl_log::CHLResult;
use chl_preparse_data::VisibilityMode;
use chl_tokens::{
    keyword, operator, punctuator, specifier,
    token::{SpecifierType, Token, TokenKind},
    TokenIter,
};
use chl_util::identifier::CHLIdent;

use crate::{
    assert_token_matches,
    log::parse_point_error,
    parse::{
        expressions::parse_expr,
        functions::try_function_parse,
        parser::{LocalName, ParserData},
        statement::parse_block_item,
        types::{
            parse_abstract_type, parse_declaration_head, parse_declarator, parse_typedef_declarator,
            parse_variable_initializer, DeclaratorKind, TagScope,
        },
    },
    peek_next_kind, try_next,
};

pub(crate) mod parser;

mod expressions;
mod functions;
mod identifier;
mod operators;
mod statement;
mod types;

pub(crate) use identifier::try_parse_qualified_name;

pub fn parse_global_stmt(data: &mut ParserData) -> CHLResult<()> {
    let Some(token) = data.tokens.peek() else {
        return Ok(());
    };

    match &token.kind {
        TokenKind::IncludeBegin => {
            data.tokens.next();
            data.begin_include();
        }
        TokenKind::IncludeEnd => {
            data.tokens.next();
            data.end_include()?;
        }
        keyword!(Import) => {
            data.tokens.goto_statement_end();
        }
        keyword!(Typedef) => parse_typedef(data)?,
        keyword!(Comptime) => {
            data.tokens.next();
            parse_global_expr(data, true)?
        }
        punctuator!(Semicolon) => {
            data.tokens.next();
        }
        specifier!(Extern) | specifier!(Public) | specifier!(Private)
            if is_extern_c_section(data) =>
        {
            parse_extern_c_mod(data)?
        }
        specifier!(Public) | specifier!(Private) => parse_access_mods(data)?,
        _ => parse_global_expr(data, false)?,
    };

    Ok(())
}

fn is_extern_c_section(data: &ParserData) -> bool {
    let access_offset = usize::from(matches!(
        data.tokens
            .slice
            .get(data.tokens.index)
            .map(|token| &token.kind),
        Some(TokenKind::Specifier(
            SpecifierType::Public | SpecifierType::Private
        ))
    ));

    matches!(
        (
            data.tokens
                .slice
                .get(data.tokens.index + access_offset)
                .map(|token| &token.kind),
            data.tokens
                .slice
                .get(data.tokens.index + access_offset + 1)
                .map(|token| &token.kind),
        ),
        (
            Some(TokenKind::Specifier(SpecifierType::Extern)),
            Some(TokenKind::StringLiteral(abi))
        ) if abi == "C"
    )
}

fn parse_extern_c_mod(data: &mut ParserData) -> CHLResult<()> {
    let visibility = if try_next!(data.tokens, specifier!(Public)) {
        VisibilityMode::Public
    } else {
        try_next!(data.tokens, specifier!(Private));
        VisibilityMode::Private
    };

    assert_token_matches!(data.tokens, specifier!(Extern), "'extern'");
    assert_token_matches!(data.tokens, TokenKind::StringLiteral(abi), "\"C\"");
    let abi = abi.clone();

    if abi != "C" {
        return parse_point_error(
            &data.tokens,
            &UNSUPPORTED_FEATURE,
            (format!("extern ABI '{abi}'"), "the parser".into()),
        );
    }

    assert_token_matches!(data.tokens, punctuator!(Colon), "':'");

    data.visibility = visibility;
    data.symbol_naming = HIRSymbolNameScheme::Unmangled;

    Ok(())
}

fn parse_access_mods(data: &mut ParserData) -> CHLResult<()> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier), "an access specifier");

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            if !data.in_include() {
                data.symbol_naming = HIRSymbolNameScheme::Namespaced;
            }
        }
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            if !data.in_include() {
                data.symbol_naming = HIRSymbolNameScheme::Namespaced;
            }
        }

        _ => {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                ("a declaration".into(), Some("in global scope".into())),
            );
        }
    };

    try_next!(data.tokens, punctuator!(Colon));

    Ok(())
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> CHLResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef), "'typedef'");
    let start_index = data.tokens.index;

    let (name, ty) = parse_typedef_declarator(data)?;
    let range = data.token_range(start_index - 1, data.tokens.index);

    let Some(name) = name else {
        return parse_point_error(
            &data.tokens.with_index(start_index),
            &EXPECTED_SYNTAX,
            ("a typedef name".into(), None),
        );
    };

    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    data.add_stmt(HIRStmt::TypeDefinition {
        symbol_naming: data.naming(LinkageMode::Standard),
        name: Some(name),
        visibility: data.visibility,
        ty,
        tag: None,
        range,
    });

    Ok(())
}

fn parse_fn_merge(
    data: &mut ParserData,
    mut prototype: HIRFunctionPrototype,
    inherited_external: bool,
) -> CHLResult<()> {
    functions::linkage::resolve(data, &mut prototype, true);
    if try_next!(data.tokens, punctuator!(Semicolon)) {
        if inherited_external {
            prototype.linkage = LinkageMode::Extern;
        }

        data.add_stmt(HIRStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            body: None,
        });
    } else {
        let body = data.with_name_scope(|data| {
            for parameter in &prototype.params {
                data.note_parameter(parameter);
            }
            parse_function_body(data, &prototype)
        })?;

        data.add_stmt(HIRStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            body: Some(body),
        });
    }

    Ok(())
}

// The names bound by 'type' parameters of the declaration ahead. They are types from the
// return type onwards, which is written before the parameter list that binds them.
fn scan_type_binders(data: &ParserData) -> Vec<(CHLIdent, LocalName)> {
    let mut names = Vec::new();
    let mut depth = 0usize;

    for (offset, token) in data.tokens.slice[data.tokens.index..].iter().enumerate() {
        match &token.kind {
            punctuator!(OpenParen) => depth += 1,
            punctuator!(CloseParen) => depth = depth.saturating_sub(1),
            punctuator!(Semicolon) | punctuator!(OpenBrace) | punctuator!(ThickArrow)
                if depth == 0 =>
            {
                break;
            }
            TokenKind::Assignment(_) if depth == 0 => break,
            keyword!(Type) if depth > 0 => {
                if let Some(TokenKind::Identifier(binder)) = data
                    .tokens
                    .slice
                    .get(data.tokens.index + offset + 1)
                    .map(|token| &token.kind)
                {
                    names.push((CHLIdent::new(binder.clone()), LocalName::Type));
                }
            }
            keyword!(Hole) if depth > 0 && !data.c_mode && !data.in_include() => {
                let start = data.tokens.index + offset + 1;
                if let Some(
                    [Token {
                        kind:
                            punctuator!(OpenParen) | operator!(Comma) | TokenKind::Assignment(None),
                        ..
                    }, _, Token {
                        kind: punctuator!(Colon),
                        ..
                    }, Token {
                        kind: TokenKind::Identifier(name),
                        ..
                    }],
                ) = data.tokens.slice.get(start - 2..start + 2)
                {
                    names.push((CHLIdent::new(name.clone()), LocalName::Inferred));
                }
            }
            _ => {}
        }
    }

    names
}

fn parse_global_expr(data: &mut ParserData, comptime: bool) -> CHLResult<()> {
    data.with_name_scope(|data| {
        for (binder, kind) in scan_type_binders(data) {
            data.note_local_name(binder, kind);
        }
        parse_global_declaration(data, comptime)
    })
}

fn parse_global_declaration(data: &mut ParserData, comptime: bool) -> CHLResult<()> {
    let start = data.tokens.index - usize::from(comptime);
    let (base, specifiers) = parse_declaration_head(data, TagScope::Module)?;
    let linkage = specifiers.linkage;
    let symbol_naming = data.naming(linkage);
    let inherited_external = !data.c_mode
        && symbol_naming == HIRSymbolNameScheme::Unmangled
        && linkage == LinkageMode::Standard;

    let mut declarator_start = start;
    loop {
        let (name, ty) = parse_declarator(data, &base, TagScope::Module, DeclaratorKind::Named)?;
        let range = data.token_range(declarator_start, data.tokens.index);
        let Some(name) = name else {
            if declarator_start == start {
                assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
                return Ok(());
            }
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                ("a variable declaration".into(), Some("after ','".into())),
            );
        };

        if declarator_start == start {
            if let Some(prototype) = try_function_parse(
                data,
                start,
                ty.clone(),
                name.clone(),
                comptime,
                specifiers,
                symbol_naming,
            )? {
                return parse_fn_merge(data, prototype, inherited_external);
            }
        }

        let initializer = parse_variable_initializer(data)?;
        data.add_stmt(HIRStmt::GlobalVariableDefinition {
            visibility: data.visibility,
            variable: HIRGlobalVariable::Standard {
                comptime,
                name,
                ty,
                is_mutable: true,
                linkage: if inherited_external && initializer.is_none() {
                    LinkageMode::Extern
                } else {
                    linkage
                },
                symbol_name_scheme: symbol_naming,
                initializer,
                range,
            },
        });

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
            return Ok(());
        }
        declarator_start = data.tokens.index;
    }
}

pub(crate) fn parse_block(data: &mut ParserData) -> CHLResult<HIRExpression> {
    parse_block_kind(data, HIRBlockKind::Statement)
}

pub(crate) fn parse_expression_block(data: &mut ParserData) -> CHLResult<HIRExpression> {
    parse_block_kind(data, HIRBlockKind::Expression)
}

fn parse_block_kind(data: &mut ParserData, kind: HIRBlockKind) -> CHLResult<HIRExpression> {
    assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

    let start_index = data.tokens.index - 1;
    let exprs = parse_block_statements(data)?;

    Ok(data.expr_from(start_index, HIRExprKind::Block { exprs, kind }))
}

fn parse_block_statements(data: &mut ParserData) -> CHLResult<Box<[HIRExpression]>> {
    data.with_name_scope(|data| {
        let mut body = Vec::new();
        while !try_next!(data.tokens, punctuator!(CloseBrace)) {
            body.push(parse_block_item(data)?);
        }
        Ok(body.into())
    })
}

fn parse_function_body(
    data: &mut ParserData,
    prototype: &HIRFunctionPrototype,
) -> CHLResult<HIRFunctionBody> {
    let start_index = data.tokens.index;
    if try_next!(data.tokens, punctuator!(OpenBrace)) {
        let statements = parse_block_statements(data)?;
        return Ok(HIRFunctionBody::Block {
            statements,
            range: data.token_range(start_index, data.tokens.index),
        });
    }

    if try_next!(data.tokens, punctuator!(ThickArrow)) {
        let expression = parse_expr(data)?;
        assert_token_matches!(
            data.tokens,
            punctuator!(Semicolon),
            "';' after function expression"
        );
        return Ok(HIRFunctionBody::Expression(expression));
    }

    if !data.c_mode
        && prototype.comptime
        && !prototype.noreturn
        && matches!(prototype.return_type.kind, HIRTypeKind::Universe)
        && try_next!(data.tokens, TokenKind::Assignment(None))
    {
        let ty = parse_abstract_type(data, TagScope::Alias)?;
        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';' after type alias");
        return Ok(HIRFunctionBody::TypeAlias(ty));
    }

    parse_point_error(
        &data.tokens,
        &EXPECTED_SYNTAX,
        ("a braced or arrow function body".into(), None),
    )
}

pub fn parse_intrinsic(tokens: &mut TokenIter) -> CHLResult<CHLIdent> {
    let mut words = Vec::new();

    while let Ok(TokenKind::Intrinsic(ident)) = peek_next_kind!(tokens) {
        words.push(ident.as_str().to_string());
        tokens.next();
    }

    if words.is_empty() {
        return parse_point_error(
            tokens,
            &EXPECTED_SYNTAX,
            ("an intrinsic identifier".into(), None),
        );
    }

    // C permits specifiers in any order; the intrinsic table is keyed on this one.
    words.sort_by_key(|word| match word.as_str() {
        "signed" | "unsigned" | "_Complex" => 0,
        "short" | "long" => 1,
        _ => 2,
    });

    Ok(CHLIdent::new(words.join(" ")))
}

pub fn try_parse_simple_identifier(tokens: &mut TokenIter) -> Option<CHLIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().map(|token| &token.kind)? else {
        return None;
    };
    let ident = CHLIdent::new(ident.clone());
    tokens.next();
    Some(ident)
}
