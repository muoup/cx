use crate::environment::function_query::deduce_function;
use crate::environment::name_mangling::mangle_templated_fn_name;
use crate::environment::{MIRFunctionGenRequest, TypeEnvironment};
use cx_ast::ast::CXExpr;
use cx_ast::data::CXFunctionKey;
use crate::type_completion::complete_prototype_no_insert;
use crate::type_completion::types::{_complete_template_input, _complete_type};
use cx_ast::data::{
    CXFunctionTemplate, CXTemplateInput, CXTemplatePrototype, ModuleResource,
};
use cx_mir::mir::program::MIRBaseMappings;
use cx_mir::mir::types::{MIRTemplateInput, MIRFunctionPrototype, MIRType};
use cx_util::identifier::CXIdent;
use cx_util::{CXResult, log_error};

pub(crate) type Overwrites = Vec<(String, MIRType)>;

pub(crate) fn add_templated_types(
    env: &mut TypeEnvironment,
    args: &CXTemplatePrototype,
    input: &MIRTemplateInput,
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

pub fn mangle_template_name(name: &str, input: &MIRTemplateInput) -> String {
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
    input: &CXTemplateInput,
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
    let cx_type = _complete_type(env, base_data, shell);
    restore_template_overwrites(env, overwrites);

    let mut cx_type = cx_type?;
    cx_type.add_template_info(
        CXIdent::new(template_name.as_str()),
        completed_input.clone(),
    );

    env.add_type(template_name.clone(), cx_type.clone());

    // Realize the destructor for this templated type using the raw template input
    let Some(base_name) = cx_type.get_base_identifier() else {
        return Ok(cx_type);
    };
    let destructor_key = CXFunctionKey::Destructor {
        type_base_name: base_name.clone(),
    };
    // Use the raw input (not completed) for deduce_function
    let _ = deduce_function(env, base_data, &CXExpr::default(), &destructor_key, Some(input));

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

    // Complete the input
    let completed_input = _complete_template_input(env, base_data, module_origin.as_ref(), input)?;
    let overwrites = add_templated_types(env, template_prototype, &completed_input);
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
