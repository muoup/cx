use crate::environment::{TCEnvironment, TCTemplateRequest};
use crate::type_mapping::{contextualize_fn_prototype, contextualize_type};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_util::mangling::{mangle_destructor, mangle_template};

pub(crate) fn instantiate_type_template(env: &mut TCEnvironment, name: &str, input: &CXTemplateInput) -> Option<CXType> {
    let mangled_name = mangle_template(name, &input.args);

    if let Some(type_) = env.get_type(&mangled_name) {
        return Some(type_.clone());
    }

    let template = &env.type_data.get_template(name)?.template.resource;
    let shell = template.shell.clone();
    let mut new_map = env.type_data.clone();

    for (ident, arg_type) in template.prototype.types.iter().zip(input.args.iter()) {
        new_map.insert_standard(ident.clone(), arg_type.clone());
    }

    std::mem::swap(&mut env.type_data, &mut new_map);
    let mut instantiated = contextualize_type(env, &shell)?;
    instantiated.map_name(|name| mangle_template(name, &input.args));
    std::mem::swap(&mut env.type_data, &mut new_map);

    env.type_data.insert_standard(mangled_name.clone(), instantiated.clone());

    let destructor_name = mangle_destructor(name);

    if let Some(_) = env.fn_data.get_template(&destructor_name) {
        instantiate_function_template(env, &destructor_name, input);
    }

    Some(instantiated)
}

pub(crate) fn instantiate_function_template<'a>(env: &'a mut TCEnvironment, name: &str, input: &CXTemplateInput) -> Option<&'a CXFunctionPrototype> {
    let mangled_name = mangle_template(name, &input.args);

    if env.fn_data.standard.contains_key(&mangled_name) {
        return env.get_func(&mangled_name);
    }

    let cache = env.fn_data.get_template(name)?;
    let template = cache.template.resource.clone();

    let mut new_map = env.type_data.clone();

    for (ident, arg_type) in template.prototype.types.iter().zip(input.args.iter()) {
        new_map.insert_standard(ident.clone(), arg_type.clone());
    }

    std::mem::swap(&mut env.type_data, &mut new_map);

    let module_origin = cache.template.external_module.clone();
    let mut instantiated = contextualize_fn_prototype(env, &template.shell.clone())?;
    instantiated.name = CXIdent::from(mangled_name.as_str());

    env.fn_data.insert_standard(mangled_name.clone(), instantiated);
    env.requests.push(TCTemplateRequest {
        module_origin: module_origin.clone(),
        name: name.to_string(),
        input: input.clone(),
    });

    std::mem::swap(&mut env.type_data, &mut new_map);

    env.get_func(&mangled_name)
}