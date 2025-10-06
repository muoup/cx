use crate::environment::{TCEnvironment, TCTemplateRequest};
use crate::expr_checking::move_semantics::acknowledge_declared_type;
use crate::type_completion::type_mapping::{contextualize_fn_prototype, contextualize_type};
use cx_parsing_data::preparse::templates::CXTemplatePrototype;
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_typechecker_data::function_map::CXFunctionKind;

pub(crate) type Overwrites = Vec<(String, CXType)>;

pub(crate) fn add_templated_types(
    env: &mut TCEnvironment,
    prototype: &CXTemplatePrototype,
    input: &CXTemplateInput,
) -> Overwrites {
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

pub fn mangle_template_name(name: &str, input: &CXTemplateInput) -> String {
    let mut mangled_name = String::from("_t");

    for arg in &input.args {
        mangled_name.push_str(&arg.mangle());
    }

    mangled_name.push('_');
    mangled_name.push_str(name);

    mangled_name
}

pub(crate) fn instantiate_type_template(
    env: &mut TCEnvironment,
    name: &str,
    input: &CXTemplateInput,
) -> Option<CXType> {
    let mangled_name = mangle_template_name(name, input);

    if let Some(type_) = env.get_type(&mangled_name) {
        return Some(type_.clone());
    }

    let template = env
        .base_data
        .type_data
        .get_template(name)?
        .template
        .resource
        .clone();
    let shell = template.shell.clone();
    
    let overwrites = add_templated_types(env, &template.prototype, input);

    let mut instantiated = contextualize_type(env, &shell)
        .expect("Failed to contextualize templated type");
    instantiated.map_name(|name| mangle_template_name(name, &input));

    env.realized_types
        .insert(mangled_name.clone(), instantiated.clone());
    restore_template_overwrites(env, overwrites);

    acknowledge_declared_type(env, &instantiated);
    
    let instantiated_ident = instantiated.get_identifier()?;
    let destructor_ident = CXFunctionKind::Destructor { base_type: instantiated_ident.clone() };
    
    if env.base_data.fn_map.get_template(&destructor_ident.clone().into()).is_some() {
        instantiate_function_template(env, &destructor_ident, input)?;
    }
    
    Some(instantiated)
}

pub(crate) fn instantiate_function_template(
    env: &mut TCEnvironment,
    name: &CXFunctionKind,
    input: &CXTemplateInput,
) -> Option<CXFunctionPrototype> {
    let cache = env.base_data.fn_map.get_template(&name.clone().into())?;
    let resource = &cache.resource;
    
    let module_origin = &cache.external_module;
    let template_prototype = &resource.prototype;
    let shell = &resource.shell;

    let overwrites = add_templated_types(env, template_prototype, input);
    
    let mut instantiated = contextualize_fn_prototype(env, shell)?;
    let base_name = instantiated.name.clone();
    
    instantiated.apply_template_mangling();
     
    if let Some(generated) = env.get_func(&instantiated.name) {
        return Some(generated);
    }
    
    env.realized_fns.insert(instantiated.name.clone(), instantiated.clone());
    env.requests.push(TCTemplateRequest {
        module_origin: module_origin.clone(),
        name: base_name.standardized(),
        input: input.clone(),
    });

    restore_template_overwrites(env, overwrites);

    Some(instantiated)
}