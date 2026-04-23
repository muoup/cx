use std::collections::HashMap;

use crate::environment::functions::completion::{complete_prototype_no_insert, complete_type};
use crate::environment::functions::mangling::mangle_templated_fn_name;
use crate::environment::symbols::completion::{
    _complete_template_input, base_data_from_module, int_complete_type,
};
use crate::environment::{MIRFunctionGenRequest, TypeEnvironment};
use cx_ast::ast::CXExpr;
use cx_ast::data::{
    CXFunctionKind, CXFunctionPrototype, CXFunctionTemplate, CXStructAttributes, CXTemplateInput,
    CXTemplatePrototype, CXType, CXTypeKind, ModuleResource,
};
use cx_mir::mir::data::{
    MIRFunctionPrototype, MIRFunctionSignature, MIRMoveAttributes, MIRTemplateInput, MIRType,
    MIRTypeKind,
};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

pub(crate) type Overwrites = crate::environment::symbols::TemplateBindingFrame;
type TemplateBindings = HashMap<String, MIRType>;

pub(crate) fn add_templated_types(
    env: &mut TypeEnvironment,
    args: &CXTemplatePrototype,
    input: &MIRTemplateInput,
) -> CXResult<Overwrites> {
    env.bind_template_types(&args.types, &input.args)
        .map_err(CXError::create_boxed)
}

pub(crate) fn restore_template_overwrites(env: &mut TypeEnvironment, overwrites: Overwrites) {
    env.restore_template_types(overwrites);
}

pub fn mangle_template_name(env: &TypeEnvironment, name: &str, input: &MIRTemplateInput) -> String {
    let mut mangled_name = String::from("_t");

    for arg in &input.args {
        mangled_name.push_str(&env.symbols.context.mangle(arg));
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
    let base_data_ref = base_data_from_module(env, base_data, None);
    let base_data = base_data_ref.as_ref();
    let completed_input =
        _complete_template_input(env, base_data, None, &CXExpr::default(), input)?;
    let template_name = mangle_template_name(env, name, &completed_input);

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
            .symbols
            .named_type_ids
            .insert(base_name.clone(), template_type_id);

        let provisional = match &shell.kind {
            CXTypeKind::Structured { attributes, .. } => {
                let attrs = resolve_template_attributes(env, attributes);
                MIRType {
                    visibility: cx_ast::ast::VisibilityMode::Private,
                    specifiers: 0,
                    move_attributes: attrs,
                    strong_identifier: Some(CXIdent::new(base_name.clone())),
                    template_info: None,
                    kind: cx_mir::mir::data::MIRTypeKind::Structured { fields: vec![] },
                }
            }
            CXTypeKind::Union { .. } => MIRType {
                visibility: cx_ast::ast::VisibilityMode::Private,
                specifiers: 0,
                move_attributes: MIRMoveAttributes::default(),
                strong_identifier: Some(CXIdent::new(base_name.clone())),
                template_info: None,
                kind: cx_mir::mir::data::MIRTypeKind::Union { variants: vec![] },
            },
            CXTypeKind::TaggedUnion { attributes, .. } => {
                let attrs = resolve_template_attributes(env, attributes);
                MIRType {
                    visibility: cx_ast::ast::VisibilityMode::Private,
                    specifiers: 0,
                    move_attributes: attrs,
                    strong_identifier: Some(CXIdent::new(base_name.clone())),
                    template_info: None,
                    kind: cx_mir::mir::data::MIRTypeKind::TaggedUnion { variants: vec![] },
                }
            }
            _ => unreachable!("Templated named type must be an aggregate"),
        };

        env.symbols
            .context
            .register_identifier(CXIdent::new(base_name.clone()), template_type_id);
        previous_named_type = env
            .symbols
            .realized_types
            .insert(base_name.clone(), provisional);
    }

    let cx_type = int_complete_type(env, base_data, &CXExpr::default(), shell);
    if let Some(base_name) = aggregate_base_name.as_ref() {
        match previous_named_type {
            Some(previous) => {
                env.symbols
                    .realized_types
                    .insert(base_name.clone(), previous);
            }
            None => {
                env.symbols.realized_types.remove(base_name);
            }
        }

        match previous_named_id {
            Some(previous) => {
                env.symbols
                    .named_type_ids
                    .insert(base_name.clone(), previous);
            }
            None => {
                env.symbols.named_type_ids.remove(base_name);
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

    if let Some(type_id) = cx_type
        .get_name()
        .and_then(|name| env.get_named_type_id(name.as_str()))
    {
        let template_info = cx_type
            .get_template_data()
            .map(|info| Box::new(info.clone()));
        let renamed = CXIdent::new(template_name.clone());
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

pub(crate) fn deduce_function_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    owner_type: Option<&MIRType>,
    arg_types: &[MIRType],
) -> CXResult<MIRFunctionPrototype> {
    let completed_input =
        deduce_function_template_input(env, base_data, template, owner_type, arg_types)?;
    instantiate_function_template_with_input(env, base_data, template, &completed_input)
}

pub(crate) fn deduce_function_template_input(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    owner_type: Option<&MIRType>,
    arg_types: &[MIRType],
) -> CXResult<MIRTemplateInput> {
    let resource = &template.resource;
    let shell = &resource.shell;
    let template_prototype = &resource.prototype;
    let external_module = template.external_module.as_ref();
    let mut bindings = TemplateBindings::new();

    match &shell.kind {
        CXFunctionKind::Standard(_) => {}
        CXFunctionKind::MemberFunction { member_type, .. }
        | CXFunctionKind::StaticMemberFunction { member_type, .. } => {
            let Some(owner_type) = owner_type else {
                return CXError::create_result("Template deduction requires a concrete owner type");
            };

            deduce_from_cx_type(
                env,
                base_data,
                external_module,
                template_prototype,
                &mut bindings,
                &member_type.as_type(),
                owner_type,
            )?;
        }
    }

    if arg_types.len() != shell.params.len() && !shell.var_args {
        return CXError::create_result(format!(
            "Function template expects {} arguments, found {}",
            shell.params.len(),
            arg_types.len()
        ));
    }

    if arg_types.len() < shell.params.len() {
        return CXError::create_result(format!(
            "Function template expects at least {} arguments, found {}",
            shell.params.len(),
            arg_types.len()
        ));
    }

    for (param, actual_type) in shell.params.iter().zip(arg_types.iter()) {
        deduce_from_cx_type(
            env,
            base_data,
            external_module,
            template_prototype,
            &mut bindings,
            &param._type,
            actual_type,
        )?;
    }

    let args = template_prototype
        .types
        .iter()
        .map(|name| {
            bindings.remove(name).ok_or_else(|| {
                CXError::create_boxed(format!(
                    "Could not deduce template argument '{}' for function {}",
                    name, shell.kind
                ))
            })
        })
        .collect::<CXResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args })
}

fn deduce_from_cx_type(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    template_prototype: &CXTemplatePrototype,
    bindings: &mut TemplateBindings,
    formal: &CXType,
    actual: &MIRType,
) -> CXResult<()> {
    if let MIRTypeKind::MemoryReference { inner_type } = &actual.kind
        && !matches!(formal.kind, CXTypeKind::MemoryReference { .. })
    {
        let inner_type = env
            .symbols
            .context
            .get(*inner_type)
            .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0))
            .clone();

        if env.symbols.is_copyable(&inner_type) {
            return deduce_from_cx_type(
                env,
                base_data,
                external_module,
                template_prototype,
                bindings,
                formal,
                &inner_type,
            );
        }
    }

    match &formal.kind {
        CXTypeKind::Identifier { name, .. }
            if template_prototype
                .types
                .iter()
                .any(|param| param == name.as_str()) =>
        {
            bind_template_argument(env, bindings, name.as_str(), actual)
        }

        CXTypeKind::TemplatedIdentifier { name, input } => {
            let Some(template_info) = actual.get_template_data() else {
                return CXError::create_result(format!(
                    "Expected realized template type '{}' while deducing, found {}",
                    name, actual
                ));
            };

            if template_info.base_name != *name {
                return CXError::create_result(format!(
                    "Expected template type '{}', found '{}'",
                    name, template_info.base_name
                ));
            }

            if input.params.len() != template_info.template_input.args.len() {
                return CXError::create_result(format!(
                    "Template arity mismatch for '{}': expected {}, found {}",
                    name,
                    input.params.len(),
                    template_info.template_input.args.len()
                ));
            }

            for (formal_arg, actual_arg) in input
                .params
                .iter()
                .zip(template_info.template_input.args.iter())
            {
                deduce_from_cx_type(
                    env,
                    base_data,
                    external_module,
                    template_prototype,
                    bindings,
                    formal_arg,
                    actual_arg,
                )?;
            }

            Ok(())
        }

        CXTypeKind::ExplicitSizedArray(inner, size) => {
            let MIRTypeKind::Array {
                length: actual_size,
                inner_type,
            } = &actual.kind
            else {
                return concrete_type_mismatch(formal, actual);
            };

            if size != actual_size {
                return CXError::create_result(format!(
                    "Array size mismatch during template deduction: expected {}, found {}",
                    size, actual_size
                ));
            }

            let actual_inner = env
                .symbols
                .context
                .get(*inner_type)
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0))
                .clone();
            deduce_from_cx_type(
                env,
                base_data,
                external_module,
                template_prototype,
                bindings,
                inner,
                &actual_inner,
            )
        }

        CXTypeKind::ImplicitSizedArray(inner) => match &actual.kind {
            MIRTypeKind::PointerTo { inner_type } | MIRTypeKind::Array { inner_type, .. } => {
                let actual_inner = env
                    .symbols
                    .context
                    .get(*inner_type)
                    .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0))
                    .clone();
                deduce_from_cx_type(
                    env,
                    base_data,
                    external_module,
                    template_prototype,
                    bindings,
                    inner,
                    &actual_inner,
                )
            }
            _ => concrete_type_mismatch(formal, actual),
        },

        CXTypeKind::MemoryReference { inner_type } => {
            let MIRTypeKind::MemoryReference {
                inner_type: actual_inner,
            } = &actual.kind
            else {
                return concrete_type_mismatch(formal, actual);
            };

            let actual_inner = env
                .symbols
                .context
                .get(*actual_inner)
                .unwrap_or_else(|| panic!("Unknown type id {}", actual_inner.0))
                .clone();
            deduce_from_cx_type(
                env,
                base_data,
                external_module,
                template_prototype,
                bindings,
                inner_type,
                &actual_inner,
            )
        }

        CXTypeKind::PointerTo { inner_type, .. } => match &actual.kind {
            MIRTypeKind::PointerTo {
                inner_type: actual_inner,
            }
            | MIRTypeKind::MemoryReference {
                inner_type: actual_inner,
            } => {
                let actual_inner = env
                    .symbols
                    .context
                    .get(*actual_inner)
                    .unwrap_or_else(|| panic!("Unknown type id {}", actual_inner.0))
                    .clone();
                deduce_from_cx_type(
                    env,
                    base_data,
                    external_module,
                    template_prototype,
                    bindings,
                    inner_type,
                    &actual_inner,
                )
            }
            MIRTypeKind::Array {
                inner_type: actual_inner,
                ..
            } => {
                let actual_inner = env
                    .symbols
                    .context
                    .get(*actual_inner)
                    .unwrap_or_else(|| panic!("Unknown type id {}", actual_inner.0))
                    .clone();
                deduce_from_cx_type(
                    env,
                    base_data,
                    external_module,
                    template_prototype,
                    bindings,
                    inner_type,
                    &actual_inner,
                )
            }
            MIRTypeKind::Function { .. }
                if matches!(inner_type.kind, CXTypeKind::FunctionPointer { .. }) =>
            {
                deduce_from_cx_type(
                    env,
                    base_data,
                    external_module,
                    template_prototype,
                    bindings,
                    inner_type,
                    actual,
                )
            }
            _ => concrete_type_mismatch(formal, actual),
        },

        CXTypeKind::FunctionPointer { prototype } => {
            let MIRTypeKind::Function {
                signature: actual_signature,
            } = &actual.kind
            else {
                return concrete_type_mismatch(formal, actual);
            };

            deduce_from_function_signature(
                env,
                base_data,
                external_module,
                template_prototype,
                bindings,
                prototype,
                actual_signature,
            )
        }

        _ => {
            let completed_formal =
                complete_type(env, base_data, external_module, &CXExpr::default(), formal)?;
            if env.type_eq(&completed_formal, actual) {
                Ok(())
            } else {
                concrete_type_mismatch(formal, actual)
            }
        }
    }
}

fn deduce_from_function_signature(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    external_module: Option<&String>,
    template_prototype: &CXTemplatePrototype,
    bindings: &mut TemplateBindings,
    formal: &CXFunctionPrototype,
    actual: &MIRFunctionSignature,
) -> CXResult<()> {
    if formal.var_args != actual.var_args {
        return CXError::create_result(format!(
            "Function pointer varargs mismatch during template deduction: expected {}, found {}",
            formal.var_args, actual.var_args
        ));
    }

    if formal.params.len() != actual.params.len() {
        return CXError::create_result(format!(
            "Function pointer arity mismatch during template deduction: expected {}, found {}",
            formal.params.len(),
            actual.params.len()
        ));
    }

    deduce_from_cx_type(
        env,
        base_data,
        external_module,
        template_prototype,
        bindings,
        &formal.return_type,
        &actual.return_type,
    )?;

    for (formal_param, actual_param) in formal.params.iter().zip(actual.params.iter()) {
        deduce_from_cx_type(
            env,
            base_data,
            external_module,
            template_prototype,
            bindings,
            &formal_param._type,
            &actual_param._type,
        )?;
    }

    Ok(())
}

fn bind_template_argument(
    env: &mut TypeEnvironment,
    bindings: &mut TemplateBindings,
    name: &str,
    actual: &MIRType,
) -> CXResult<()> {
    if let Some(existing) = bindings.get(name) {
        if env.type_eq(existing, actual) {
            return Ok(());
        }

        return CXError::create_result(format!(
            "Conflicting deductions for template argument '{}': {} vs {}",
            name, existing, actual
        ));
    }

    bindings.insert(name.to_string(), actual.clone());
    Ok(())
}

fn concrete_type_mismatch(formal: &CXType, actual: &MIRType) -> CXResult<()> {
    CXError::create_result(format!(
        "Template deduction mismatch: expected {}, found {}",
        formal, actual
    ))
}

pub(crate) fn instantiate_function_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    input: &CXTemplateInput,
) -> CXResult<MIRFunctionPrototype> {
    let completed_input = _complete_template_input(
        env,
        base_data,
        template.external_module.as_ref(),
        &CXExpr::default(),
        input,
    )?;

    instantiate_function_template_with_input(env, base_data, template, &completed_input)
}

fn instantiate_function_template_with_input(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    completed_input: &MIRTemplateInput,
) -> CXResult<MIRFunctionPrototype> {
    let resource = &template.resource;
    let module_origin = &template.external_module;
    let template_prototype = &resource.prototype;
    let overwrites = add_templated_types(env, template_prototype, completed_input)?;

    let instantiated = complete_function_template(env, base_data, template);
    let instantiated = match instantiated {
        Ok(instantiated) => instantiated,
        Err(err) => {
            restore_template_overwrites(env, overwrites);
            return Err(err);
        }
    };

    let result = if let Some(generated) = env.get_realized_func(instantiated.name.as_str()) {
        Ok(generated)
    } else {
        env.items
            .realized_fns
            .insert(instantiated.name.to_string(), instantiated.clone());
        env.request_function_generation(MIRFunctionGenRequest::Template {
            module_origin: module_origin.clone(),
            kind: template.resource.shell.kind.clone(),
            input: completed_input.clone(),
        });
        Ok(instantiated)
    };

    restore_template_overwrites(env, overwrites);
    result
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
