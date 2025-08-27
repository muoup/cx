use crate::type_mapping::contextualize_type;
use cx_data_ast::preparse::templates::CXTypeTemplate;
use cx_data_typechecker::cx_types::{CXTemplateInput, CXType};
use cx_data_typechecker::TCEnvironment;
use cx_util::mangling::mangle_template;

pub(crate) fn instantiate_type_template(env: &mut TCEnvironment, template: &CXTypeTemplate, input: &CXTemplateInput) -> Option<CXType> {
    let mangled_name = mangle_template(template.name.as_str(), &input.args);

    if let Some(type_) = env.get_type(&mangled_name) {
        return Some(type_.clone());
    }

    let mut new_map = env.type_map.clone();

    for (ident, arg_type) in template.prototype.types.iter().zip(input.args.iter()) {
        new_map.insert_standard(ident.clone(), arg_type.clone());
    }

    let instantiated = contextualize_type(env, &template.shell)?;

    env.type_map.insert_standard(mangled_name.clone(), instantiated.clone());
    Some(instantiated)
}