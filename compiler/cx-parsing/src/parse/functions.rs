use crate::{
    assert_token_matches, next_kind,
    parse::try_parse_qualified_name,
    peek_next_kind, try_next,
};
use cx_ast::ast::{
    function::{CXFunctionContract, CXFunctionKind, CXFunctionPrototype, CXParameter},
    modifiers::CXLinkageMode,
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
    expressions::parse_expr,
    parser::ParserData,
    templates::{convert_template_proto_to_args, try_parse_template},
    types::{parse_initializer, parse_specifier},
};

pub struct FunctionDeclaration {
    pub prototype: CXFunctionPrototype,
    pub template_prototype: Option<CXTemplatePrototype>,
}

pub fn try_function_parse(
    data: &mut ParserData,
    return_type: CXType,
    name: CXIdent,
    linkage: CXLinkageMode,
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
        let (member_namespace, member_name) = name
            .namespace
            .parent_and_name()
            .unwrap_or_else(|| unreachable!());
        let name = name.name;

        CXFunctionKind::MemberFunction {
            member_type: QualifiedName {
                namespace: member_namespace,
                name: member_name,
            },
            name: name
        }
    };

    if !try_next!(data.tokens, punctuator!(OpenParen)) {
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
                    return log_parse_error!(
                        data,
                        "Precondition already defined in function contract."
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
                    return log_parse_error!(
                        data,
                        "Postcondition already defined in function contract."
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

    log_parse_error!(data, "Unclosed parenthesized declaration suffix")
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
