use crate::{
    assert_token_matches, log::parse_point_error, next_kind, parse::try_parse_qualified_name,
    peek_next_kind, try_next,
};
use cx_hir::ast::{
    function::{HIRFunctionContract, HIRFunctionKind, HIRFunctionPrototype, HIRParameter},
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::{HIRType, HIRTypeKind},
};
use cx_log::catalogue::parse::*;
use cx_log::CXResult;
use cx_namespace::module::QualifiedName;
use cx_tokens::{
    identifier, keyword, operator, punctuator,
    token::{PunctuatorType, TokenKind},
};
use cx_util::identifier::CXIdent;

use crate::parse::{
    expressions::parse_expr,
    parser::ParserData,
    types::{parse_attributes, parse_initializer, DeclarationAttributes},
};

pub fn try_function_parse(
    data: &mut ParserData,
    return_type: HIRType,
    name: CXIdent,
    comptime: bool,
    linkage: LinkageMode,
    symbol_naming: HIRSymbolNameScheme,
    attributes: DeclarationAttributes,
) -> CXResult<Option<HIRFunctionPrototype>> {
    let range_start = data.tokens.index;

    let name = if try_next!(data.tokens, operator!(ScopeRes)) {
        data.tokens.index = range_start - 1;

        try_parse_qualified_name(&mut data.tokens)?.unwrap()
    } else {
        QualifiedName::root(name)
    };

    let kind = if name.namespace.is_root() {
        HIRFunctionKind::Standard(name.name)
    } else {
        if name.namespace.segments().len() != 1 {
            return parse_point_error(&data.tokens, &ASSOCIATED_FUNCTION, comptime);
        }

        HIRFunctionKind::AssociatedFunction {
            namespace: name.namespace.segments()[0].clone(),
            name: name.name,
        }
    };

    if !matches!(peek_next_kind!(data.tokens)?, punctuator!(OpenParen)) {
        data.tokens.index = range_start;
        return Ok(None);
    };

    let mut args = parse_params(data)?;
    if comptime {
        for param in &mut args.params {
            param.comptime = true;
        }
    }

    Ok(Some(HIRFunctionPrototype {
        return_type: attributes
            .merge(args.attributes)
            .apply_to_return_type(return_type),
        kind,
        comptime,
        contract: args.contract,
        linkage,
        symbol_naming,

        params: args.params,
        var_args: args.var_args,

        range: data.token_range(range_start, data.tokens.index),
    }))
}

pub(crate) fn parse_function_contract(
    data: &mut ParserData,
    attributes: &mut DeclarationAttributes,
) -> CXResult<HIRFunctionContract> {
    parse_c_declaration_suffixes(data, attributes)?;

    let safe = try_next!(data.tokens, keyword!(Safe));

    let mut contract = HIRFunctionContract {
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
                    return parse_point_error(&data.tokens, &DUPLICATE_ITEM, ("precondition".into(), "function".into()));
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
                    return parse_point_error(&data.tokens, &DUPLICATE_ITEM, ("postcondition".into(), "function".into()));
                }

                data.tokens.next();

                let return_val_name = if try_next!(data.tokens, punctuator!(OpenParen)) {
                    assert_token_matches!(data.tokens, identifier!(ret), "a return value name");
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

    parse_c_declaration_suffixes(data, attributes)?;
    Ok(contract)
}

// FIXME: Remove this hack and support declaration suffixes
fn parse_c_declaration_suffixes(
    data: &mut ParserData,
    attributes: &mut DeclarationAttributes,
) -> CXResult<()> {
    loop {
        parse_attributes(&mut data.tokens, attributes);

        let Some(token) = data.tokens.peek() else {
            return Ok(());
        };

        let TokenKind::Identifier(name) = &token.kind else {
            return Ok(());
        };

        if matches!(
            name.as_str(),
            "__asm__" | "__asm" | "asm" | "__declspec" | "__nonnull" | "__nonnull__" | "__wur"
        ) {
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

    parse_point_error(&data.tokens, &UNEXPECTED_END, Some("declaration".into()))
}

pub(crate) struct ParseParamsResult {
    pub(crate) params: Vec<HIRParameter>,
    pub(crate) var_args: bool,
    pub(crate) contract: HIRFunctionContract,
    pub(crate) attributes: DeclarationAttributes,
}

pub(crate) fn parse_params(data: &mut ParserData) -> CXResult<ParseParamsResult> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");

    let mut params = Vec::new();
    let mut attributes = DeclarationAttributes::default();

    while !try_next!(data.tokens, punctuator!(CloseParen)) {
        if try_next!(data.tokens, punctuator!(Ellipsis)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            let contract = parse_function_contract(data, &mut attributes)?;

            return Ok(ParseParamsResult {
                params,
                var_args: true,
                contract,
                attributes,
            });
        }

        // A '@type' parameter is a comptime parameter without the keyword
        let comptime = try_next!(data.tokens, keyword!(Comptime));
        let (name, ty, _) = parse_initializer(data)?;
        let comptime = comptime || matches!(ty.kind, HIRTypeKind::Universe);

        params.push(HIRParameter { name, ty, comptime });

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            break;
        }
    }

    let contract = parse_function_contract(data, &mut attributes)?;

    Ok(ParseParamsResult {
        params,
        var_args: false,
        contract,
        attributes,
    })
}
