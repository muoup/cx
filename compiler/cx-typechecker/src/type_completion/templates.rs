use crate::environment::{TCEnvironment, TCTemplateRequest};
use crate::type_completion::prototypes::complete_fn_prototype;
use crate::type_completion::types::{_complete_template_input, _complete_type};
use cx_parsing_data::preparse::{FunctionTypeIdent, NaiveFnKind};
use cx_parsing_data::preparse::naive_types::CXNaiveTemplateInput;
use cx_parsing_data::preparse::templates::CXTemplatePrototype;
use cx_typechecker_data::ast::TCBaseMappings;
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_util::identifier::CXIdent;
use cx_util::log_error;

pub(crate) type Overwrites = Vec<(String, CXType)>;

pub(crate) fn add_templated_types(
    env: &mut TCEnvironment,
    args: &CXTemplatePrototype,
    input: &CXTemplateInput,
) -> Overwrites {
    let mut overwrites = Vec::new();

    for (ident, arg_type) in args.types.iter().zip(input.args.iter()) {
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
    base_data: &TCBaseMappings,
    input: &CXNaiveTemplateInput,
    name: &str,
) -> Option<CXType> {
    let completed_input = _complete_template_input(env, base_data, None, input).unwrap();

    let template_name = mangle_template_name(name, &completed_input);

    if let Some(template) = env.get_realized_type(template_name.as_str()) {
        return Some(template.clone());
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
    let mut cx_type = _complete_type(env, base_data, None, shell)?;
    restore_template_overwrites(env, overwrites);

    cx_type.set_name(CXIdent::from(template_name.as_str()));
    env.add_type(template_name, cx_type.clone());

    let destructor_ident = NaiveFnKind::Destructor(FunctionTypeIdent::Templated(
        CXIdent::from(name),
        input.clone(),
    ));

    if env
        .base_data
        .fn_data
        .get_template(&(&destructor_ident).into())
        .is_some()
    {
        instantiate_function_template(env, base_data, &destructor_ident, &completed_input)?;
    }

    Some(cx_type)
}

pub(crate) fn instantiate_function_template(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    name: &NaiveFnKind,
    input: &CXTemplateInput,
) -> Option<CXFunctionPrototype> {
    let cache = env.base_data.fn_data.get_template(&name.into())?;
    let resource = &cache.resource;

    let module_origin = &cache.external_module;
    let template_prototype = &resource.prototype;
    let shell = &resource.shell;

    let overwrites = add_templated_types(env, template_prototype, input);

    let mut instantiated = complete_fn_prototype(env, base_data, shell)?;
    instantiated.apply_template_mangling();

    if let Some(generated) = env.get_realized_func(&instantiated.name) {
        return Some(generated);
    }

    env.realized_fns
        .insert(instantiated.name.clone(), instantiated.clone());
    env.requests.push(TCTemplateRequest {
        module_origin: module_origin.clone(),
        name: name.clone(),
        input: input.clone(),
    });

    restore_template_overwrites(env, overwrites);

    Some(instantiated)
}
