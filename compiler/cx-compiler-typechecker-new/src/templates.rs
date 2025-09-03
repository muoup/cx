use crate::type_mapping::{contextualize_fn_prototype, contextualize_type};
use cx_data_ast::preparse::templates::{CXFunctionTemplate, CXTypeTemplate};
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_util::mangling::mangle_template;
use crate::environment::{TCEnvironment, TCTemplateRequest};

pub(crate) fn instantiate_type_template(env: &mut TCEnvironment, template: &CXTypeTemplate, input: &CXTemplateInput) -> Option<CXType> {
    let mangled_name = mangle_template(template.name.as_str(), &input.args);

    if let Some(type_) = env.get_type(&mangled_name) {
        return Some(type_.clone());
    }

    let mut new_map = env.type_data.clone();

    for (ident, arg_type) in template.prototype.types.iter().zip(input.args.iter()) {
        new_map.insert_standard(ident.clone(), arg_type.clone());
    }

    let instantiated = contextualize_type(env, &template.shell)?;

    env.type_data.insert_standard(mangled_name.clone(), instantiated.clone());
    Some(instantiated)
}

pub(crate) fn instantiate_function_template<'a>(env: &'a mut TCEnvironment, name: &str, input: &CXTemplateInput) -> Option<&'a CXFunctionPrototype> {
    let cache = env.fn_data.get_template(name)?;
    let template = cache.template.resource.clone();
    let mangled_name = mangle_template(template.name.as_str(), &input.args);

    if !env.fn_data.standard.contains_key(&mangled_name) {
        let mut new_map = env.type_data.clone();

        for (ident, arg_type) in template.prototype.types.iter().zip(input.args.iter()) {
            new_map.insert_standard(ident.clone(), arg_type.clone());
        }

        let module_origin = cache.template.external_module.clone();
        let instantiated = contextualize_fn_prototype(env, &template.shell.clone())?;

        env.fn_data.insert_standard(mangled_name.clone(), instantiated);
        env.requests.push(TCTemplateRequest {
            module_origin: module_origin.clone(),
            name: name.to_string(),
            input: input.clone(),
        });
    }

    env.get_func(&mangled_name)
}