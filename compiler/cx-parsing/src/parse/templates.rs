use cx_lexer_data::{identifier, operator, TokenIter};
use cx_parsing_data::data::{
    CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind, CXTemplatePrototype, PredeclarationType,
};
use crate::parse::ParserData;
use cx_parsing_data::{assert_token_matches, peek_kind, try_next};
use cx_util::{CXResult, identifier::CXIdent};

use crate::parse::types::parse_initializer;

pub(crate) fn note_templated_types(
    data: &mut ParserData,
    template_prototype: &CXTemplatePrototype,
) {
    for template_name in &template_prototype.types {
        if data.ast.type_data.is_key_std(template_name) {
            continue;
        }

        let _nil_type: CXNaiveType = CXNaiveTypeKind::Identifier {
            name: CXIdent::from("__undefined_template_type"),
            predeclaration: PredeclarationType::None,
        }
        .to_type();

        data.add_type(template_name.clone(), _nil_type.clone(), None);
    }
}

pub(crate) fn unnote_templated_types(
    data: &mut ParserData,
    template_prototype: &CXTemplatePrototype,
) {
    for template_name in &template_prototype.types {
        let (name, _type) = data.ast.type_data.remove_standard(template_name).unwrap();

        if let CXNaiveTypeKind::Identifier {
            name,
            predeclaration: PredeclarationType::None,
        } = &_type.resource.kind
        {
            if name.as_str() == "__undefined_template_type" {
                continue;
            }
        }

        data.ast.type_data.insert_standard(name, _type);
    }
}

pub(crate) fn try_parse_template(tokens: &mut TokenIter) -> Option<CXTemplatePrototype> {
    if peek_kind!(tokens, operator!(Less)) {
        parse_template_prototype(tokens)
    } else {
        None
    }
}

pub(crate) fn parse_template_prototype(tokens: &mut TokenIter) -> Option<CXTemplatePrototype> {
    assert_token_matches!(tokens, operator!(Less));

    let mut type_decls = Vec::new();

    loop {
        assert_token_matches!(tokens, identifier!(template_name));
        let template_name = template_name.clone();
        type_decls.push(template_name);

        if !try_next!(tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, operator!(Greater));

    Some(CXTemplatePrototype { types: type_decls })
}

pub(crate) fn convert_template_proto_to_args(
    prototype: CXTemplatePrototype,
) -> CXNaiveTemplateInput {
    let params = prototype
        .types
        .into_iter()
        .map(|name| {
            CXNaiveTypeKind::Identifier {
                name: CXIdent::from(name),
                predeclaration: PredeclarationType::None,
            }
            .to_type()
        })
        .collect();

    CXNaiveTemplateInput { params }
}

pub(crate) fn parse_template_args(data: &mut ParserData) -> CXResult<CXNaiveTemplateInput> {
    assert_token_matches!(data.tokens, operator!(Less));

    let mut input_types = Vec::new();

    loop {
        let (None, _type) = parse_initializer(data)? else {
            log_parse_error!(data, "Expected type declaration in template arguments!");
        };

        input_types.push(_type);

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, operator!(Greater));

    Ok(CXNaiveTemplateInput {
        params: input_types,
    })
}
