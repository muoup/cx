use crate::environment::name_mangling::mangle_templated_fn_name;
use crate::environment::{MIRFunctionGenRequest, TypeEnvironment};
use crate::type_completion::complete_prototype_no_insert;
use crate::type_completion::types::base_data_from_module;
use crate::type_completion::types::{_complete_template_input, _complete_type};
use cx_ast::ast::CXExpr;
use cx_ast::data::{
    CXFunctionTemplate, CXStructAttributes, CXTemplateInput, CXTemplatePrototype, CXTypeKind,
    ModuleResource,
};
use cx_mir::mir::program::MIRBaseMappings;
use cx_mir::mir::data::{MIRFunctionPrototype, MIRMoveAttributes, MIRTemplateInput, MIRType};
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

pub(crate) type Overwrites = Vec<(String, MIRType)>;

pub(crate) fn add_templated_types(
    env: &mut TypeEnvironment,
    args: &CXTemplatePrototype,
    input: &MIRTemplateInput,
) -> CXResult<Overwrites> {
    if args.types.len() != input.args.len() {
        return CXError::create_result(format!(
            "Template argument count mismatch: expected {}, got {}",
            args.types.len(),
            input.args.len()
        ));
    }

    Ok(args
        .types
        .iter()
        .zip(input.args.iter())
        .filter_map(|(ident, arg_type)| {
            env.realized_types
                .insert(ident.clone(), arg_type.clone())
                .map(|existing| (ident.clone(), existing))
        })
        .collect())
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
    let (_, base_data) = base_data_from_module(env, base_data, None);
    let completed_input =
        _complete_template_input(env, base_data, None, &CXExpr::default(), input)?;
    let template_name = mangle_template_name(name, &completed_input);

    if let Some(template) = env.get_realized_type(template_name.as_str()) {
        return Ok(template.clone());
    }

    let Some(template) = base_data.type_data.get_template(&name.to_owned()) else {
        return CXError::create_result(format!("Unknown template type: {}", name));
    };

    let shell = &template.resource.shell;

    let overwrites = add_templated_types(env, &template.resource.prototype, &completed_input)?;
    let aggregate_base_name = match &shell.kind {
        CXTypeKind::Structured { name, .. } | CXTypeKind::Union { name, .. } => {
            name.as_ref().map(|name| name.as_string())
        }
        CXTypeKind::TaggedUnion { name, .. } => Some(name.as_string()),
        _ => None,
    };
    let mut previous_named_type = None;
    let mut previous_named_id = None;

    if let Some(base_name) = aggregate_base_name.as_ref() {
        let template_type_id = env.get_or_create_named_type_id(template_name.as_str());
        previous_named_id = env
            .named_type_ids
            .insert(base_name.clone(), template_type_id);

        let provisional = match &shell.kind {
            CXTypeKind::Structured { attributes, .. } => {
                let attrs = resolve_template_attributes(env, attributes);
                MIRType::named_struct(
                    CXIdent::new(base_name.clone()),
                    template_type_id,
                    None,
                    attrs,
                )
            }
            CXTypeKind::Union { .. } => {
                MIRType::named_union(CXIdent::new(base_name.clone()), template_type_id)
            }
            CXTypeKind::TaggedUnion { attributes, .. } => {
                let attrs = resolve_template_attributes(env, attributes);
                MIRType::named_tagged_union(
                    CXIdent::new(base_name.clone()),
                    template_type_id,
                    None,
                    attrs,
                )
            }
            _ => unreachable!("Templated named type must be an aggregate"),
        };

        previous_named_type = env.realized_types.insert(base_name.clone(), provisional);
    }

    let cx_type = _complete_type(env, base_data, &CXExpr::default(), shell);
    if let Some(base_name) = aggregate_base_name.as_ref() {
        match previous_named_type {
            Some(previous) => {
                env.realized_types.insert(base_name.clone(), previous);
            }
            None => {
                env.realized_types.remove(base_name);
            }
        }

        match previous_named_id {
            Some(previous) => {
                env.named_type_ids.insert(base_name.clone(), previous);
            }
            None => {
                env.named_type_ids.remove(base_name);
            }
        }
    }
    restore_template_overwrites(env, overwrites);

    let mut cx_type = cx_type?;
    cx_type.add_template_info(
        CXIdent::new(template_name.as_str()),
        completed_input.clone(),
    );
    cx_type.set_name(CXIdent::new(template_name.clone()));

    if let Some(type_id) = cx_type.named_type_id() {
        let template_info = cx_type
            .get_template_data()
            .map(|info| Box::new(info.clone()));
        let renamed = CXIdent::new(template_name.clone());
        cx_type.rewrite_named_type_metadata(type_id, &renamed, &template_info);
        env.update_named_type_metadata(type_id, renamed, template_info);
    }

    env.add_type(template_name, cx_type.clone());

    Ok(cx_type)
}

fn resolve_template_attributes(
    env: &TypeEnvironment,
    attributes: &CXStructAttributes,
) -> MIRMoveAttributes {
    let mut nocopy = attributes.nocopy || attributes.nodrop;
    let mut nodrop = attributes.nodrop;

    if let Some(param_name) = &attributes.copy_traits {
        if let Some(resolved_type) = env.get_realized_type(param_name) {
            if let Some(src_attrs) = resolved_type.struct_attributes() {
                nocopy = nocopy || src_attrs.nocopy;
                nodrop = nodrop || src_attrs.nodrop;
            }
        }
    }

    MIRMoveAttributes { nocopy, nodrop }
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
    let completed_input = _complete_template_input(
        env,
        base_data,
        module_origin.as_ref(),
        &CXExpr::default(),
        input,
    )?;
    let overwrites = add_templated_types(env, template_prototype, &completed_input)?;
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
