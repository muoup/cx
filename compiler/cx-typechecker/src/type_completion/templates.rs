use crate::environment::{TCEnvironment, TCTemplateRequest};
use crate::type_checking::move_semantics::acknowledge_declared_type;
use crate::type_completion::prototypes::{contextualize_fn_prototype};
use crate::type_completion::types::_complete_type;
use cx_parsing_data::preparse::naive_types::{CXNaiveTemplateInput, ModuleResource};
use cx_parsing_data::preparse::templates::CXTemplatePrototype;
use cx_parsing_data::preparse::CXNaiveTypeMap;
use cx_pipeline_data::db::ModuleData;
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_typechecker_data::CXTypeMap;
use cx_util::identifier::CXIdent;
use cx_util::log_error;

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

pub(crate) fn instantiate_type_temp(
    module_data: &ModuleData,
    acc_fns: &mut CXTypeMap,
    acc_types: &mut CXTypeMap,
    base_type_map: &CXNaiveTypeMap,
    input: &CXNaiveTemplateInput,
    name: &str,
) -> Option<CXType> {
    let template_name = mangle_template_name(name, &input);
    
    if let Some(template) = acc_types.get(template_name.as_str()) {
        return Some(template.clone());
    }

    let Some(template) = base_type_map.get_template(&name.to_owned()) else {
        log_error!(
            "Template not found: {name}<{}>",
            input
                .args
                .iter()
                .map(|param| format!("{param}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
    };

    let shell = &template.resource.shell;
    let mut map_clone = base_type_map.clone();

    for (name, naive_type) in template
        .resource
        .prototype
        .types
        .iter()
        .zip(input.params.iter())
    {
        map_clone
            .insert_standard(name.clone(), ModuleResource::standard(naive_type.clone()));
    }

    let mut cx_type = _complete_type(module_data, acc_types, &map_clone, None, shell)?;
    cx_type.set_name(CXIdent::from(template_name.as_str()));
    acc_types.insert(template_name, cx_type.clone());
       
    let instantiated_ident = cx_type.get_identifier().unwrap();
    let destructor_ident = CXFunctionKind::Destructor { base_type: instantiated_ident.clone() };
       
    if env.base_data.fn_map.get_template(&destructor_ident.clone().into()).is_some() {
        instantiate_function_template(env, &destructor_ident, input)?;
    }
    
    Some(cx_type)
}

pub(crate) fn instantiate_function_template(
    env: &mut TCEnvironment,
    name: &CXFunctionKind,
    input: &CXTemplateInput,
) -> Option<CXFunctionPrototype> {
    let cache = env.base_data.fn_data.get_template(&name.clone().into())?;
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