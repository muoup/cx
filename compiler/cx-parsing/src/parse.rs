use cx_hir::ast::{
    expression::{HIRBlockKind, HIRExprKind, HIRExpression},
    function::{HIRFunctionBody, HIRFunctionPrototype},
    global_var::HIRGlobalVariable,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
    HIRStmt,
};
use cx_log::catalogue::parse::*;
use cx_log::CXResult;
use cx_preparse_data::VisibilityMode;
use cx_tokens::{
    keyword, operator, punctuator, specifier,
    token::{OperatorType, PunctuatorType, SpecifierType, TokenKind},
    TokenIter,
};
use cx_util::identifier::CXIdent;
use std::rc::Rc;

use crate::{
    assert_token_matches, log::parse_point_error, next_kind, parse::{
        expressions::parse_expr,
        functions::try_function_parse,
        parser::ParserData,
        statement::parse_stmt,
        types::{parse_base_mods, parse_initializer, parse_typedef_initializer},
    }, peek_next_kind, try_next,
};

pub(crate) mod parser;

mod expressions;
mod functions;
mod identifier;
mod operators;
mod statement;
mod types;

pub(crate) use identifier::try_parse_qualified_name;

pub fn parse_global_stmt(data: &mut ParserData) -> CXResult<()> {
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

fn parse_extern_c_mod(data: &mut ParserData) -> CXResult<()> {
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

fn parse_access_mods(data: &mut ParserData) -> CXResult<()> {
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

pub(crate) fn parse_typedef(data: &mut ParserData) -> CXResult<()> {
    assert_token_matches!(data.tokens, keyword!(Typedef), "'typedef'");
    let start_index = data.tokens.index;

    let (name, ty) = parse_typedef_initializer(data)?;

    let Some(name) = name else {
        return parse_point_error(
            &data.tokens.with_index(start_index),
            &EXPECTED_SYNTAX,
            ("a typedef name".into(), None),
        );
    };

    assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");

    if let HIRTypeKind::Identifier {
        name: type_name,
        lookup,
        args: None,
    } = &ty.kind
    {
        let is_existing_type_alias = *lookup == HIRTypeLookup::Standard
            || data.ast.definition_stmts.iter().any(|definition| {
                matches!(
                    &definition.stmt,
                    HIRStmt::TypeDefinition {
                        name: Some(existing),
                        ..
                    } if existing == &name
                )
            });
        if type_name.namespace.is_root() && type_name.name == name && is_existing_type_alias {
            data.add_stmt(HIRStmt::TypeDefinition {
                name: Some(name),
                visibility: data.visibility,
                ty: ty.clone(),
                tag: None,
            });
            return Ok(());
        }
    }

    data.add_stmt(HIRStmt::TypeDefinition {
        name: Some(name),
        visibility: data.visibility,
        ty: ty.clone(),
        tag: None,
    });

    Ok(())
}

fn parse_fn_merge(
    data: &mut ParserData,
    mut prototype: HIRFunctionPrototype,
    inherited_external: bool,
) -> CXResult<()> {
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
        let body = parse_function_body(data)?;

        data.add_stmt(HIRStmt::FunctionDefinition {
            prototype,
            visibility: data.visibility,
            body: Some(body),
        });
    }

    Ok(())
}

// The names bound by '@type' parameters of the declaration ahead. They are types from the
// return type onwards, which is written before the parameter list that binds them.
fn scan_type_binders(data: &ParserData) -> Vec<CXIdent> {
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
            TokenKind::CompilerIdentifier(name) if depth > 0 && name == "type" => {
                if let Some(TokenKind::Identifier(binder)) = data
                    .tokens
                    .slice
                    .get(data.tokens.index + offset + 1)
                    .map(|token| &token.kind)
                {
                    names.push(CXIdent::new(binder.clone()));
                }
            }
            _ => {}
        }
    }

    names
}

fn parse_global_expr(data: &mut ParserData, comptime: bool) -> CXResult<()> {
    let binders = scan_type_binders(data);
    for binder in &binders {
        data.note_type_name(binder.clone());
    }

    let result = parse_global_declaration(data, comptime);

    for binder in &binders {
        data.unnote_type_name(binder);
    }

    result
}

fn parse_global_declaration(data: &mut ParserData, comptime: bool) -> CXResult<()> {
    let (name, return_type, specifiers) = parse_initializer(data)?;
    let linkage = specifiers.linkage;
    let symbol_naming = if data.c_mode {
        if linkage == LinkageMode::Static {
            HIRSymbolNameScheme::Namespaced
        } else {
            HIRSymbolNameScheme::Unmangled
        }
    } else {
        data.symbol_naming
    };

    let inherited_external = !data.c_mode
        && symbol_naming == HIRSymbolNameScheme::Unmangled
        && linkage == LinkageMode::Standard;

    let Some(name) = name else {
        // Blank statement consisting on just a type, (i.e. struct [name] { [fields] };)

        assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
        return Ok(());
    };

    if !data.tokens.has_next() {
        return parse_point_error(
            &data.tokens,
            &UNEXPECTED_END,
            Some("global declaration".into()),
        );
    }

    if let Some(prototype) = try_function_parse(
        data,
        return_type.clone(),
        name.clone(),
        comptime,
        linkage,
        symbol_naming,
        specifiers.attributes,
    )? {
        return parse_fn_merge(data, prototype, inherited_external);
    }

    if comptime {
        return parse_point_error(
            &data.tokens,
            &EXPECTED_SYNTAX,
            ("comptime function parameters".into(), None),
        );
    }

    match next_kind!(data.tokens)? {
        TokenKind::Assignment(_) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, punctuator!(Semicolon), "';'");
            
            data.add_stmt(HIRStmt::GlobalVariableDefinition {
                visibility: data.visibility,
                variable: HIRGlobalVariable::Standard {
                    name: name.clone(),
                    ty: return_type.clone(),
                    is_mutable: true,
                    linkage,
                    symbol_name_scheme: symbol_naming,
                    initializer: Some(initial_value.clone()),
                },
            });
        }

        punctuator!(Semicolon) => {
            add_global_variable(
                data,
                name,
                return_type.clone(),
                linkage,
                symbol_naming,
                inherited_external,
                None,
            );
        }

        operator!(Comma) => {
            add_global_variable(
                data,
                name,
                return_type.clone(),
                linkage,
                symbol_naming,
                inherited_external,
                None,
            );

            loop {
                let (next_name, next_type) = parse_base_mods(data, return_type.clone())?;
                let Some(next_name) = next_name else {
                    return parse_point_error(
                        &data.tokens,
                        &EXPECTED_SYNTAX,
                        (
                            "a variable declaration".into(),
                            Some("after ','".into()),
                        ),
                    );
                };
                let initializer = if try_next!(data.tokens, TokenKind::Assignment(_)) {
                    Some(parse_expr(data)?)
                } else {
                    None
                };

                add_global_variable(
                    data,
                    next_name,
                    next_type,
                    linkage,
                    symbol_naming,
                    inherited_external,
                    initializer,
                );

                match next_kind!(data.tokens)? {
                    TokenKind::Operator(OperatorType::Comma) => {}
                    TokenKind::Punctuator(PunctuatorType::Semicolon) => break,
                    _ => {
                        return parse_point_error(
                            &data.tokens,
                            &EXPECTED_SYNTAX,
                            ("a global separator".into(), None),
                        );
                    }
                }
            }
        }

        _ => {
            return parse_point_error(
                &data.tokens,
                &EXPECTED_SYNTAX,
                (
                    "a global declaration".into(),
                    None,
                ),
            );
        }
    }

    Ok(())
}

fn add_global_variable(
    data: &mut ParserData,
    name: CXIdent,
    ty: HIRType,
    linkage: LinkageMode,
    symbol_naming: HIRSymbolNameScheme,
    inherited_external: bool,
    initializer: Option<HIRExpression>,
) {
    data.add_stmt(HIRStmt::GlobalVariableDefinition {
        visibility: data.visibility,
        variable: HIRGlobalVariable::Standard {
            name,
            ty,
            is_mutable: true,
            linkage: if inherited_external {
                LinkageMode::Extern
            } else {
                linkage
            },
            symbol_name_scheme: symbol_naming,
            initializer,
        },
    });
}

pub(crate) fn parse_block(data: &mut ParserData) -> CXResult<HIRExpression> {
    parse_block_kind(data, HIRBlockKind::Statement)
}

pub(crate) fn parse_expression_block(data: &mut ParserData) -> CXResult<HIRExpression> {
    parse_block_kind(data, HIRBlockKind::Expression)
}

fn parse_block_kind(data: &mut ParserData, kind: HIRBlockKind) -> CXResult<HIRExpression> {
    assert_token_matches!(data.tokens, punctuator!(OpenBrace), "'{'");

    let start_index = data.tokens.index - 1;
    let exprs = parse_block_statements(data)?;

    Ok(data.expr_from(start_index, HIRExprKind::Block { exprs, kind }))
}

fn parse_block_statements(data: &mut ParserData) -> CXResult<Rc<[HIRExpression]>> {
    let mut body = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseBrace)) {
        body.push(parse_stmt(data)?);
    }

    Ok(body.into())
}

fn parse_function_body(data: &mut ParserData) -> CXResult<HIRFunctionBody> {
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

    parse_point_error(
        &data.tokens,
        &EXPECTED_SYNTAX,
        ("a braced or arrow function body".into(), None),
    )
}

pub fn parse_intrinsic(tokens: &mut TokenIter) -> CXResult<CXIdent> {
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

    Ok(CXIdent::new(words.join(" ")))
}

pub fn try_parse_simple_identifier(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().map(|token| &token.kind)? else {
        return None;
    };
    let ident = CXIdent::new(ident.clone());
    tokens.next();
    Some(ident)
}
