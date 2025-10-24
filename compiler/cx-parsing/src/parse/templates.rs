use cx_parsing_data::parse::parser::ParserData;
use cx_parsing_data::preparse::naive_types::{
    CXNaiveType, CXNaiveTypeKind, ModuleResource, PredeclarationType,
};
use cx_parsing_data::preparse::templates::CXTemplatePrototype;
use cx_util::identifier::CXIdent;

pub(crate) fn note_templated_types(
    data: &mut ParserData,
    template_prototype: &CXTemplatePrototype,
) {
    for template_name in &template_prototype.types {
        if data
            .ast
            .type_map
            .is_key_std(template_name)
        {
            continue;
        }

        let _nil_type: CXNaiveType = CXNaiveTypeKind::Identifier {
            name: CXIdent::from("__undefined_template_type"),
            predeclaration: PredeclarationType::None,
        }
        .to_type();

        data.ast.type_map.insert_standard(
            template_name.clone(),
            ModuleResource::with_visibility(_nil_type, data.visibility),
        );
    }
}

pub(crate) fn unnote_templated_types(
    data: &mut ParserData,
    template_prototype: &CXTemplatePrototype,
) {
    for template_name in &template_prototype.types {
        let (name, _type) = data
            .ast
            .type_map
            .remove_standard(template_name)
            .unwrap();

        if let CXNaiveTypeKind::Identifier {
            name,
            predeclaration: PredeclarationType::None,
        } = &_type.resource.kind
        {
            if name.as_str() == "__undefined_template_type" {
                continue;
            }
        }

        data.ast.type_map.insert_standard(name, _type);
    }
}
