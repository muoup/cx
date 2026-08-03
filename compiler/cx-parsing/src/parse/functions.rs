use crate::{
    assert_token_matches, log::parse_point_error, next_kind, parse::try_parse_qualified_name,
    peek_next_kind, try_next,
};
use cx_ast::ast::{
    function::{
        CXComptimeFnPrototype, CXComptimeParameter, CXComptimeValueType, CXFunctionContract,
        CXFunctionKind, CXFunctionPrototype, CXParameter,
    },
    modifiers::{CXLinkageMode, CXSymbolNameScheme},
    template::CXTemplatePrototype,
    types::CXType,
};
use cx_log::CXResult;
use cx_tokens::{
    identifier, keyword, operator, punctuator,
    token::{PunctuatorType, TokenKind},
    TokenRange,
};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::parse::{
    expressions::parse_expr, parser::ParserData, templates::try_parse_template,
    types::parse_initializer,
};

pub struct FunctionDeclaration {
    pub prototype: CXFunctionPrototype,
    pub template_prototype: Option<CXTemplatePrototype>,
}

pub struct ComptimeFunctionDeclaration {
    pub prototype: CXComptimeFnPrototype,
    pub template_prototype: Option<CXTemplatePrototype>,
}

pub fn try_function_parse(
    data: &mut ParserData,
    return_type: CXType,
    name: CXIdent,
    linkage: CXLinkageMode,
    symbol_naming: CXSymbolNameScheme,
) -> CXResult<Option<FunctionDeclaration>> {
    let range_start = data.tokens.index;

    let name = if try_next!(data.tokens, operator!(ScopeRes)) {
        data.tokens.index = range_start - 1;

        try_parse_qualified_name(&mut data.tokens)?.unwrap()
    } else {
        QualifiedName::root(name)
    };

    let template_prototype = try_parse_template(&mut data.tokens)?;

    let kind = if name.namespace.is_root() {
        CXFunctionKind::Standard(name.name)
    } else {
        if name.namespace.segments().len() != 1 {
            return parse_point_error(
                &data.tokens,
                "Associated function declarations must have exactly two segments".to_string(),
            );
        }

        CXFunctionKind::AssociatedFunction {
            namespace: name.namespace.segments()[0].clone(),
            name: name.name,
        }
    };

    if !matches!(peek_next_kind!(data.tokens)?, punctuator!(OpenParen)) {
        data.tokens.index = range_start;
        return Ok(None);
    };

    let args = parse_params(data)?;
    let prototype = CXFunctionPrototype {
        return_type,
        kind,
        params: args.params,
        var_args: args.var_args,
        contract: args.contract,
        linkage,
        symbol_naming,
        range: TokenRange::new(
            range_start,
            data.tokens.index,
            data.file_origin_for_range(range_start, data.tokens.index),
        ),
    };

    Ok(Some(FunctionDeclaration {
        prototype,
        template_prototype,
    }))
}

pub fn parse_comptime_function(data: &mut ParserData) -> CXResult<ComptimeFunctionDeclaration> {
    assert_token_matches!(data.tokens, keyword!(Comptime), "'comptime'");
    let return_type = parse_comptime_initializer(data)?;
    let Some(name) = return_type.name else {
        return parse_point_error(&data.tokens, "Expected comptime function name".to_string());
    };

    let Some(declaration) = try_comptime_function_parse(data, return_type.value_type, name)? else {
        return parse_point_error(
            &data.tokens,
            "Expected comptime function parameter list".to_string(),
        );
    };

    Ok(declaration)
}

fn try_comptime_function_parse(
    data: &mut ParserData,
    return_type: CXComptimeValueType,
    name: CXIdent,
) -> CXResult<Option<ComptimeFunctionDeclaration>> {
    let range_start = data.tokens.index;

    let name = if try_next!(data.tokens, operator!(ScopeRes)) {
        data.tokens.index = range_start - 1;

        try_parse_qualified_name(&mut data.tokens)?.unwrap()
    } else {
        QualifiedName::root(name)
    };

    let template_prototype = try_parse_template(&mut data.tokens)?;

    let kind = if name.namespace.is_root() {
        CXFunctionKind::Standard(name.name)
    } else {
        if name.namespace.segments().len() != 1 {
            return parse_point_error(
                &data.tokens,
                "Associated comptime function declarations must have exactly two segments"
                    .to_string(),
            );
        }

        CXFunctionKind::AssociatedFunction {
            namespace: name.namespace.segments()[0].clone(),
            name: name.name,
        }
    };

    if !matches!(peek_next_kind!(data.tokens)?, punctuator!(OpenParen)) {
        data.tokens.index = range_start;
        return Ok(None);
    };

    let args = parse_comptime_params(data)?;
    let prototype = CXComptimeFnPrototype {
        return_type,
        kind,
        params: args,
        range: TokenRange::new(
            range_start,
            data.tokens.index,
            data.file_origin_for_range(range_start, data.tokens.index),
        ),
    };

    Ok(Some(ComptimeFunctionDeclaration {
        prototype,
        template_prototype,
    }))
}

struct ComptimeValueInitializer {
    name: Option<CXIdent>,
    value_type: CXComptimeValueType,
}

fn parse_comptime_initializer(data: &mut ParserData) -> CXResult<ComptimeValueInitializer> {
    let expr = try_next!(data.tokens, keyword!(Expr));
    let mut params = Vec::new();

    if expr && try_next!(data.tokens, punctuator!(OpenParen)) {
        while !try_next!(data.tokens, punctuator!(CloseParen)) {
            let (name, _type, _) = parse_initializer(data)?;
            if name.is_some() {
                return parse_point_error(
                    &data.tokens,
                    "Staged expression parameter types cannot have names".to_string(),
                );
            }
            params.push(_type);

            if !try_next!(data.tokens, operator!(Comma)) {
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                break;
            }
        }
    }

    let (name, _type, _) = parse_initializer(data)?;

    Ok(ComptimeValueInitializer {
        name,
        value_type: CXComptimeValueType {
            expr,
            params,
            _type,
        },
    })
}

fn parse_comptime_params(data: &mut ParserData) -> CXResult<Vec<CXComptimeParameter>> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

    let mut params = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseParen)) {
        let parsed = parse_comptime_initializer(data)?;

        params.push(CXComptimeParameter {
            name: parsed.name,
            value_type: parsed.value_type,
        });

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            break;
        }
    }

    Ok(params)
}

pub(crate) fn parse_function_contract(data: &mut ParserData) -> CXResult<CXFunctionContract> {
    skip_c_declaration_suffixes(data)?;

    let safe = try_next!(data.tokens, keyword!(Safe));

    let mut contract = CXFunctionContract {
        safe,
        precondition: None,
        postcondition: None,
    };

    if !try_next!(data.tokens, keyword!(Where)) {
        return Ok(contract);
    }

    while let Ok(next) = peek_next_kind!(data.tokens) {
        match next {
            keyword!(Precondition) => {
                if contract.precondition.is_some() {
                    return parse_point_error(
                        &data.tokens,
                        "Precondition already defined in function contract.".to_string(),
                    );
                }

                data.tokens.next();
                assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
                assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                let expr = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

                contract.precondition = Some(expr);
            }
            keyword!(Postcondition) => {
                if contract.postcondition.is_some() {
                    return parse_point_error(
                        &data.tokens,
                        "Postcondition already defined in function contract.".to_string(),
                    );
                }

                data.tokens.next();

                let return_val_name = if try_next!(data.tokens, punctuator!(OpenParen)) {
                    assert_token_matches!(data.tokens, identifier!(ret));
                    let name = CXIdent::new(ret.as_str());

                    assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
                    Some(name)
                } else {
                    None
                };

                assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
                assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                let expr = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

                contract.postcondition = Some((return_val_name, expr));
            }
            _ => break,
        }

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    skip_c_declaration_suffixes(data)?;
    Ok(contract)
}

// FIXME: Remove this hack and support declaration suffixes
fn skip_c_declaration_suffixes(data: &mut ParserData) -> CXResult<()> {
    loop {
        let Some(token) = data.tokens.peek() else {
            return Ok(());
        };

        let TokenKind::Identifier(name) = &token.kind else {
            return Ok(());
        };

        if matches!(name.as_str(), "__asm__" | "__asm" | "asm") {
            data.tokens.next();
            skip_optional_parenthesized_tokens(data)?;
            continue;
        }

        if name.starts_with("__attribute")
            || matches!(
                name.as_str(),
                "__declspec" | "__nonnull" | "__nonnull__" | "__wur"
            )
        {
            data.tokens.next();
            skip_optional_parenthesized_tokens(data)?;
            continue;
        }

        return Ok(());
    }
}

fn skip_optional_parenthesized_tokens(data: &mut ParserData) -> CXResult<()> {
    if !matches!(
        data.tokens.peek().map(|token| &token.kind),
        Some(TokenKind::Punctuator(PunctuatorType::OpenParen))
    ) {
        return Ok(());
    }

    let mut depth = 0usize;
    while data.tokens.has_next() {
        match next_kind!(data.tokens)? {
            punctuator!(OpenParen) => depth += 1,
            punctuator!(CloseParen) => {
                depth -= 1;
                if depth == 0 {
                    return Ok(());
                }
            }
            _ => {}
        }
    }

    parse_point_error(
        &data.tokens,
        "Unclosed parenthesized declaration suffix".to_string(),
    )
}

pub(crate) struct ParseParamsResult {
    pub(crate) params: Vec<CXParameter>,
    pub(crate) var_args: bool,
    pub(crate) contract: CXFunctionContract,
}

pub(crate) fn parse_params(data: &mut ParserData) -> CXResult<ParseParamsResult> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

    let mut params = Vec::new();

    while !try_next!(data.tokens, punctuator!(CloseParen)) {
        if try_next!(data.tokens, punctuator!(Ellipsis)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            let contract = parse_function_contract(data)?;

            return Ok(ParseParamsResult {
                params,
                var_args: true,
                contract,
            });
        }

        let (name, _type, _) = parse_initializer(data)?;
        let name = name;

        params.push(CXParameter { name, _type });

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            break;
        }
    }

    let contract = parse_function_contract(data)?;

    Ok(ParseParamsResult {
        params,
        var_args: false,
        contract,
    })
}
