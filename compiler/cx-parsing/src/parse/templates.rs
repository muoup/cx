use crate::parse::ParserData;
use crate::{assert_token_matches, peek_kind, try_next};
use cx_ast::ast::template::{CXTemplateInput, CXTemplatePrototype};
use cx_ast::ast::types::{CXType, CXTypeKind, PredeclarationType};
use cx_tokens::{identifier, operator, TokenIter};
use cx_util::namespace::NamespacePath;
use cx_util::{identifier::CXIdent, namespace::QualifiedName, CXResult};

use crate::parse::types::parse_initializer;

pub(crate) fn note_templated_types(
    data: &mut ParserData,
    template_prototype: &CXTemplatePrototype,
) -> CXResult<()> {
    for template_name in &template_prototype.types {
        if data.is_type_ident(&QualifiedName {
            namespace: NamespacePath::root(),
            name: template_name.clone(),
        }) {
            continue;
        }

        let _nil_type: CXType = CXTypeKind::Identifier {
            name: QualifiedName::new_raw(CXIdent::new("__undefined_template_type")),
            predeclaration: PredeclarationType::None,
            template_input: None,
        }
        .to_type();

        *data
            .temporary_type_names
            .entry(template_name.clone())
            .or_insert(0) += 1;
    }

    Ok(())
}

pub(crate) fn unnote_templated_types(
    data: &mut ParserData,
    template_prototype: &CXTemplatePrototype,
) {
    for template_name in &template_prototype.types {
        let entry = data.temporary_type_names.get_mut(template_name)
            .expect("CRITICAL: unnote_templated_types() should only be called with template prototypes that were previously noted with note_templated_types()!");

        *entry -= 1;

        if *entry == 0 {
            data.temporary_type_names.remove(template_name);
        }
    }
}

pub(crate) fn try_parse_template(tokens: &mut TokenIter) -> CXResult<Option<CXTemplatePrototype>> {
    if peek_kind!(tokens, operator!(Less)) {
        parse_template_prototype(tokens).map(Some)
    } else {
        Ok(None)
    }
}

pub(crate) fn parse_template_prototype(tokens: &mut TokenIter) -> CXResult<CXTemplatePrototype> {
    assert_token_matches!(tokens, operator!(Less), "'<'");

    let mut type_decls = Vec::new();

    loop {
        assert_token_matches!(tokens, identifier!(template_name));
        let template_name = CXIdent::new(template_name.clone());
        type_decls.push(template_name);

        if !try_next!(tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, operator!(Greater), "'>'");

    Ok(CXTemplatePrototype { types: type_decls })
}

pub(crate) fn convert_template_proto_to_args(prototype: CXTemplatePrototype) -> CXTemplateInput {
    let params = prototype
        .types
        .into_iter()
        .map(|name| {
            CXTypeKind::Identifier {
                name: QualifiedName::new_raw(name),
                predeclaration: PredeclarationType::None,
                template_input: None,
            }
            .to_type()
        })
        .collect();

    CXTemplateInput { params }
}

pub(crate) fn parse_template_args(data: &mut ParserData) -> CXResult<CXTemplateInput> {
    assert_token_matches!(data.tokens, operator!(Less), "'<'");

    let mut inputtype_s = Vec::new();

    loop {
        let (None, _type, _) = parse_initializer(data)? else {
            return log_parse_error!(data, "Expected type declaration in template arguments!");
        };

        inputtype_s.push(_type);

        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, operator!(Greater), "'>'");

    Ok(CXTemplateInput {
        params: inputtype_s,
    })
}
