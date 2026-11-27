use crate::{
    assert_token_matches, log::parse_point_error, next_kind, parse::try_parse_qualified_name,
    peek_next_kind, try_next,
};
use chl_hir::ast::{
    expression::{HIRBinOp, HIRExprKind, HIRExpression},
    function::{HIRFunctionContract, HIRFunctionKind, HIRFunctionPrototype, HIRParameter},
    modifiers::HIRSymbolNameScheme,
    types::{HIRType, HIRTypeKind},
};
use chl_log::{CHLResult, catalogue::parse::*};
use chl_namespace::module::QualifiedName;
use chl_tokens::{
    identifier, keyword, operator, punctuator,
    token::{PunctuatorType, TokenKind},
};
use chl_util::identifier::CHLIdent;

use crate::parse::{
    expressions::parse_expr,
    parser::ParserData,
    types::{parse_abstract_type, parse_noreturn, parse_typed_declarator, ParsedSpecifiers, TagScope},
};

pub(crate) mod linkage;


pub fn try_function_parse(
    data: &mut ParserData,
    declaration_start: usize,
    return_type: HIRType,
    name: CHLIdent,
    comptime: bool,
    specifiers: ParsedSpecifiers,
    symbol_naming: HIRSymbolNameScheme,
) -> CHLResult<Option<HIRFunctionPrototype>> {
    let range_start = data.tokens.index;
    let name_start = range_start - 1;

    let name = if try_next!(data.tokens, operator!(ScopeRes)) {
        data.tokens.index = name_start;

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

    let (params, var_args, return_type, contract, noreturn) = data.with_name_scope(|data| {
        let (params, var_args) = parse_params(data, TagScope::Module)?;
        let mut noreturn = specifiers.noreturn;
        let trailing = !data.c_mode && matches!(return_type.kind, HIRTypeKind::Auto);
        if trailing {
            parse_c_decl_modifiers(data, &mut noreturn)?;
        }
        let safe = trailing && try_next!(data.tokens, keyword!(Safe));
        let return_type = if trailing && try_next!(data.tokens, operator!(Arrow)) {
            parse_abstract_type(data, TagScope::Module)?.add_specifier(return_type.specifiers)
        } else {
            return_type
        };
        let contract = parse_function_contract(data, &mut noreturn, safe)?;
        Ok((params, var_args, return_type, contract, noreturn))
    })?;

    Ok(Some(HIRFunctionPrototype {
        return_type,
        noreturn,
        kind,
        comptime,
        contract,
        linkage: specifiers.linkage,
        symbol_naming,

        params,
        var_args,

        range: data.token_range(declaration_start, data.tokens.index),
    }))
}

pub(crate) fn parse_function_contract(
    data: &mut ParserData,
    noreturn: &mut bool,
    safe: bool,
) -> CHLResult<HIRFunctionContract> {
    parse_c_decl_modifiers(data, noreturn)?;

    let safe = safe || try_next!(data.tokens, keyword!(Safe));

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
                data.tokens.next();
                assert_token_matches!(data.tokens, punctuator!(Colon), "':'");
                assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
                let expr = parse_expr(data)?;
                assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");

                contract.precondition = Some(match contract.precondition {
                    Some(previous) => HIRExpression {
                        range: previous.range.cover(&expr.range),
                        kind: HIRExprKind::BinOp {
                            op: HIRBinOp::LAnd,
                            lhs: Box::new(previous),
                            rhs: Box::new(expr),
                        },
                    },
                    None => expr,
                });
            }
            keyword!(Postcondition) => {
                if contract.postcondition.is_some() {
                    return parse_point_error(&data.tokens, &DUPLICATE_ITEM, ("postcondition".into(), "function".into()));
                }

                data.tokens.next();

                let return_val_name = if try_next!(data.tokens, punctuator!(OpenParen)) {
                    assert_token_matches!(data.tokens, identifier!(ret), "a return value name");
                    let name = CHLIdent::new(ret.as_str());

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

    parse_c_decl_modifiers(data, noreturn)?;
    Ok(contract)
}

fn parse_c_decl_modifiers(
    data: &mut ParserData,
    noreturn: &mut bool,
) -> CHLResult<()> {
    loop {
        *noreturn |= parse_noreturn(&mut data.tokens);

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

fn skip_optional_parenthesized_tokens(data: &mut ParserData) -> CHLResult<()> {
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

pub(crate) fn parse_params(
    data: &mut ParserData,
    scope: TagScope,
) -> CHLResult<(Vec<HIRParameter>, bool)> {
    assert_token_matches!(data.tokens, punctuator!(OpenParen), "'('");
    let mut params = Vec::new();
    let var_args = loop {
        if try_next!(data.tokens, punctuator!(CloseParen)) {
            break false;
        }
        if try_next!(data.tokens, punctuator!(Ellipsis)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            break true;
        }

        let start = data.tokens.index;
        let comptime = try_next!(data.tokens, keyword!(Comptime));
        let (name, ty, _) = parse_typed_declarator(data, scope)?;
        let range = data.token_range(start, data.tokens.index);
        let default = match try_next!(data.tokens, TokenKind::Assignment(None)) {
            true => {
                data.change_comma_mode(false);
                let default = parse_expr(data);
                data.pop_comma_mode();
                Some(default?)
            }
            false => None,
        };

        let parameter = HIRParameter { name, ty, comptime, default, range };
        data.note_parameter(&parameter);
        params.push(parameter);

        if !try_next!(data.tokens, operator!(Comma)) {
            assert_token_matches!(data.tokens, punctuator!(CloseParen), "')'");
            break false;
        }
    };

    Ok((params, var_args))
}
