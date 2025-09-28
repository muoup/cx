use cx_data_ast::preparse::templates::CXTemplatePrototype;
use crate::environment::{TCEnvironment, TCTemplateRequest};
use crate::type_mapping::{contextualize_fn_prototype, contextualize_type};
use cx_util::identifier::CXIdent;
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_util::mangling::{mangle_destructor, mangle_template};

pub(crate) type Overwrites = Vec<(String, CXType)>;

pub(crate) fn add_templated_types(env: &mut TCEnvironment, prototype: &CXTemplatePrototype, input: &CXTemplateInput) -> Overwrites {
    let mut overwrites = Vec::new();

    for (ident, arg_type) in prototype.types.iter().zip(input.args.iter()) {
        if let Some(existing) = env.realized_types.insert(ident.clone(), arg_type.clone()) {
            overwrites.push((ident.clone(), existing));
        }
    }

    overwrites
}

pub(crate) fn restore_template_overwrites(env: &mut TCEnvironment, overwrites: Overwrites) {
    for (ident, arg_type) in overwrites.into_iter() {
        env.realized_types.insert(ident, arg_type);
    }
}

pub(crate) fn instantiate_type_template(env: &mut TCEnvironment, name: &str, input: &CXTemplateInput) -> Option<CXType> {
    let mangled_name = mangle_template(name, &input.args);

    if let Some(type_) = env.get_type(&mangled_name) {
        return Some(type_.clone());
    }

    let template = env.base_data.type_data.get_template(name)?
        .template.resource.clone();
    let shell = template.shell.clone();

    let overwrites = add_templated_types(env, &template.prototype, input);

    let mut instantiated = contextualize_type(env, &shell)?;
    instantiated.map_name(|name| mangle_template(name, &input.args));

    env.realized_types.insert(mangled_name.clone(), instantiated.clone());

    let destructor_name = mangle_destructor(name);

    if env.base_data.fn_data.get_template(&destructor_name).is_some() {
        instantiate_function_template(env, &destructor_name, input);
    }

    restore_template_overwrites(env, overwrites);

    Some(instantiated)
}

pub(crate) fn instantiate_function_template(env: &mut TCEnvironment, name: &str, input: &CXTemplateInput) -> Option<CXFunctionPrototype> {
    let mangled_name = mangle_template(name, &input.args);

    if env.base_data.fn_data.standard.contains_key(&mangled_name) {
        return env.get_func(&mangled_name);
    }

    let cache = env.base_data.fn_data.get_template(name)?;
    let module_origin = cache.template.external_module.clone();
    let template = cache.template.resource.clone();

    let overwrites = add_templated_types(env, &template.prototype, input);

    let mut instantiated = contextualize_fn_prototype(env, &template.shell.clone())?;
    instantiated.name = CXIdent::from(mangled_name.as_str());

    env.realized_fns.insert(mangled_name.clone(), instantiated);
    env.requests.push(TCTemplateRequest {
        module_origin: module_origin.clone(),
        name: name.to_string(),
        input: input.clone(),
    });

    restore_template_overwrites(env, overwrites);

    env.get_func(&mangled_name)
}