use cx_data_ast::{
    assert_token_matches, peek_next, peek_next_kind,
    preparse::{naive_types::{CXNaiveTemplateInput, CXNaiveTypeKind, PredeclarationType}, templates::CXTemplatePrototype},
    try_next,
};
use cx_data_lexer::{operator, token::TokenKind, TokenIter};
use cx_util::identifier::CXIdent;

use crate::declarations::type_parsing::parse_initializer;

pub(crate) fn try_parse_template(tokens: &mut TokenIter) -> Option<CXTemplatePrototype> {
    if peek_next!(tokens, operator!(Less)) {
        parse_template_prototype(tokens)
    } else {
        None
    }
}

pub(crate) fn parse_template_prototype(tokens: &mut TokenIter) -> Option<CXTemplatePrototype> {
    assert_token_matches!(tokens, operator!(Less));

    let mut type_decls = Vec::new();

    loop {
        assert_token_matches!(tokens, TokenKind::Identifier(template_name));
        let template_name = template_name.clone();
        type_decls.push(template_name);

        if !try_next!(tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, operator!(Greater));

    Some(CXTemplatePrototype { types: type_decls })
}

pub(crate) fn convert_template_proto_to_args(prototype: CXTemplatePrototype) -> CXNaiveTemplateInput {
    let params = prototype
        .types
        .into_iter()
        .map(|name| {
            CXNaiveTypeKind::Identifier {
                name: CXIdent::from(name),
                predeclaration: PredeclarationType::None,
            }.to_type()
        })
        .collect();

    CXNaiveTemplateInput { params }
}

pub(crate) fn parse_template_args(tokens: &mut TokenIter) -> Option<CXNaiveTemplateInput> {
    assert_token_matches!(tokens, operator!(Less));

    let mut input_types = Vec::new();

    loop {
        let Some((None, _type)) = parse_initializer(tokens) else {
            log_preparse_error!(tokens, "Expected type declaration in template arguments!");
        };

        input_types.push(_type);

        if !try_next!(tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, operator!(Greater));

    Some(CXNaiveTemplateInput {
        params: input_types,
    })
}

pub fn parse_intrinsic(tokens: &mut TokenIter) -> Option<CXIdent> {
    let mut ss = String::new();

    while let Some(TokenKind::Intrinsic(ident)) = peek_next_kind!(tokens) {
        ss.push_str(format!("{ident:?}").to_lowercase().as_str());
        tokens.next();
    }

    if ss.is_empty() {
        return None;
    }

    Some(CXIdent::from(ss))
}

pub fn parse_std_ident(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };

    tokens.next();

    Some(CXIdent::from(ident))
}
