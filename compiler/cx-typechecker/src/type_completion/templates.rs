use crate::environment::function_query::query_destructor;
use crate::environment::name_mangling::mangle_templated_fn_name;
use crate::environment::{MIRFunctionGenRequest, TypeEnvironment};
use crate::type_completion::complete_prototype_no_insert;
use crate::type_completion::types::{_complete_template_input, _complete_type};
use cx_parsing_data::data::{
    CXFunctionTemplate, CXNaiveTemplateInput, CXTemplatePrototype, ModuleResource,
};
use cx_typechecker_data::mir::program::MIRBaseMappings;
use cx_typechecker_data::mir::types::{CXTemplateInput, MIRFunctionPrototype, MIRType};
use cx_util::identifier::CXIdent;
use cx_util::{CXResult, log_error};

pub(crate) type Overwrites = Vec<(String, MIRType)>;

pub(crate) fn add_templated_types(
    env: &mut TypeEnvironment,
    args: &CXTemplatePrototype,
    input: &CXTemplateInput,
) -> Overwrites {
    args.types.iter().zip(input.args.iter())
        .filter_map(|(ident, arg_type)|
            env.realized_types.insert(ident.clone(), arg_type.clone())
                .map(|existing| (ident.clone(), existing))
        )
        .collect()
}

pub(crate) fn restore_template_overwrites(env: &mut TypeEnvironment, overwrites: Overwrites) {
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
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    input: &CXNaiveTemplateInput,
    name: &str,
) -> CXResult<MIRType> {
    let completed_input = _complete_template_input(env, base_data, None, input)?;
    let template_name = mangle_template_name(name, &completed_input);
    
    if let Some(template) = env.get_realized_type(template_name.as_str()) {
        return Ok(template.clone());
    }

    let Some(template) = base_data.type_data.get_template(&name.to_owned()) else {
        log_error!(
            "Template not found: {name}<{}>",
            completed_input
                .args
                .iter()
                .map(|param| format!("{param}"))
                .collect::<Vec<_>>()
                .join(", ")
        );
    };
    
    let shell = &template.resource.shell;

    let overwrites = add_templated_types(env, &template.resource.prototype, &completed_input);
    let mut cx_type = _complete_type(env, base_data, shell)?;
    restore_template_overwrites(env, overwrites);
    
    cx_type.add_template_info(
        CXIdent::new(template_name.as_str()),
        completed_input.clone(),
    );
    
    env.add_type(base_data, template_name, cx_type.clone());
    query_destructor(env, base_data, &cx_type);
    Ok(cx_type)
}

pub(crate) fn complete_function_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
) -> CXResult<MIRFunctionPrototype> {
    let resource = &template.resource;
    let shell = &resource.shell;

    let mut completed = complete_prototype_no_insert(env, base_data, None, shell)?;
    completed.name = mangle_templated_fn_name(
        env,
        base_data,
        &template.resource.shell.kind,
        &completed.return_type,
        &completed
            .params
            .iter()
            .map(|param| param._type.clone())
            .collect::<Vec<_>>(),
    )?
    .into();

    Ok(completed)
}

pub(crate) fn instantiate_function_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    input: &CXTemplateInput,
) -> CXResult<MIRFunctionPrototype> {
    let resource = &template.resource;
    let module_origin = &template.external_module;
    let template_prototype = &resource.prototype;
    
    let overwrites = add_templated_types(env, template_prototype, input);
    let instantiated = complete_function_template(env, base_data, template)?;
    
    if let Some(generated) = env.get_realized_func(instantiated.name.as_str()) {
        return Ok(generated);
    }
    
    env.realized_fns
        .insert(instantiated.name.to_string(), instantiated.clone());
    env.requests.push(MIRFunctionGenRequest::Template {
        module_origin: module_origin.clone(),
        kind: template.resource.shell.kind.clone(),
        input: input.clone(),
    });
    
    restore_template_overwrites(env, overwrites);
    Ok(instantiated)
}
