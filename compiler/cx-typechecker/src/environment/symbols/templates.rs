use std::collections::HashMap;

use crate::environment::functions::completion::{complete_prototype_no_insert, complete_type};
use crate::environment::functions::mangling::mangle_templated_fn_name;
use crate::environment::symbols::completion::{
    base_data_from_module, int_complete_type, internal_complete_template_input,
};
use crate::environment::{MIRFunctionGenRequest, TypeEnvironment};
use cx_ast::ast::CXExpression;
use cx_ast::data::{
    CXFunctionKind, CXFunctionPrototype, CXFunctionTemplate, CXStructAttributes, CXTemplateInput,
    CXTemplatePrototype, CXType, CXTypeKind, ModuleResource,
};
use cx_mir::mir::data::{
    MIRFunctionPrototype, MIRFunctionSignature, MIRMoveAttributes, MIRTemplateInput, MIRType,
    MIRTypeKind,
};
use cx_mir::mir::name_mangling::mangle_namespace_symbol;
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::{
    identifier::CXIdent,
    namespace::{NamespacePath, QualifiedName},
};
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
    mangled_name.push_str(&mangle_namespace_symbol(&qualified_name_from_str(name)));

    mangled_name
}

fn qualified_name_from_str(name: &str) -> QualifiedName {
    let path = NamespacePath::from_scoped_path(name);
    path.parent_and_name()
        .map(|(namespace, name)| QualifiedName::new(namespace, name))
        .unwrap_or_else(|| QualifiedName::new_raw(CXIdent::new(name)))
}

pub(crate) fn instantiate_type_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    input: &CXTemplateInput,
    name: &str,
) -> CXResult<MIRType> {
    let Some(template) = base_data.type_data.get_template(&name.to_owned()) else {
        return CXError::create_result(format!("Unknown template type: {}", name));
    };
    let completed_input =
        internal_complete_template_input(env, base_data, None, &CXExpression::default(), input)?;
    let base_data_ref = base_data_from_module(env, base_data, template.external_module.as_ref());
    let base_data = base_data_ref.as_ref();
    let shell = &template.resource.shell;
    let aggregate_base_name =
        templated_aggregate_base_name(shell).unwrap_or_else(|| terminal_name(name).as_string());
    let strong_name = QualifiedName::new(
        base_data.namespace.clone(),
        CXIdent::new(aggregate_base_name.clone()),
    );
    let template_name = mangle_template_name(env, &strong_name.as_flat_name(), &completed_input);

    if let Some(template) = env.get_realized_type(template_name.as_str()) {
        return Ok(template.clone());
    }

    let overwrites = add_templated_types(env, &template.resource.prototype, &completed_input)?;
    let mut previous_named_type = None;
    let mut previous_named_id = None;

    if templated_aggregate_base_name(shell).is_some() {
        let base_name = &aggregate_base_name;
        let template_type_id = env.get_or_create_named_type_id(template_name.as_str());
        previous_named_id = env
            .symbols
            .named_type_ids
            .insert(base_name.clone(), template_type_id);

        let provisional = templated_aggregate_provisional(env, shell, base_name);
        env.symbols
            .context
            .register_identifier(CXIdent::new(base_name.clone()), template_type_id);
        previous_named_type = env
            .symbols
            .realized_types
            .insert(base_name.clone(), provisional);
    }

    let cx_type = int_complete_type(env, base_data, &CXExpression::default(), shell);

    if templated_aggregate_base_name(shell).is_some() {
        let base_name = &aggregate_base_name;
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

    if templated_aggregate_base_name(shell).is_some() {
        let template_info = Some(Box::new(cx_mir::mir::data::TemplateInfo {
            base_name: CXIdent::new(aggregate_base_name.clone()),
            template_input: completed_input.clone(),
        }));
        let type_id = env.get_or_create_named_type_id(template_name.as_str());
        cx_type.strong_identifier = Some(strong_name.clone());
        cx_type.debug_name = Some(CXIdent::new(aggregate_base_name));
        cx_type.template_info = template_info.clone();
        env.finish_type_definition(type_id, cx_type.clone());
        env.update_named_type_metadata(type_id, strong_name, template_info);
    }

    env.add_type(template_name, cx_type.clone());

    Ok(cx_type)
}

fn terminal_name(name: &str) -> CXIdent {
    NamespacePath::from_scoped_path(name)
        .parent_and_name()
        .map(|(_, name)| name)
        .unwrap_or_else(|| CXIdent::new(name))
}

fn templated_aggregate_base_name(shell: &CXType) -> Option<String> {
    match &shell.kind {
        CXTypeKind::Structured { name, .. } | CXTypeKind::Union { name, .. } => {
            name.as_ref().map(|name| name.as_string())
        }
        CXTypeKind::TaggedUnion { name, .. } => Some(name.as_string()),
        _ => None,
    }
}

fn templated_aggregate_provisional(
    env: &TypeEnvironment,
    shell: &CXType,
    base_name: &str,
) -> MIRType {
    let name = CXIdent::new(base_name);

    match &shell.kind {
        CXTypeKind::Structured { attributes, .. } => MIRType {
            visibility: cx_ast::ast::VisibilityMode::Private,
            specifiers: shell.specifiers,
            move_attributes: resolve_template_attributes(env, attributes),
            strong_identifier: Some(QualifiedName::new_raw(name.clone())),
            debug_name: Some(name),
            template_info: None,
            kind: MIRTypeKind::Structured { fields: vec![] },
        },
        CXTypeKind::Union { .. } => MIRType {
            visibility: cx_ast::ast::VisibilityMode::Private,
            specifiers: shell.specifiers,
            move_attributes: MIRMoveAttributes::default(),
            strong_identifier: Some(QualifiedName::new_raw(name.clone())),
            debug_name: Some(name),
            template_info: None,
            kind: MIRTypeKind::Union { variants: vec![] },
        },
        CXTypeKind::TaggedUnion { attributes, .. } => MIRType {
            visibility: cx_ast::ast::VisibilityMode::Private,
            specifiers: shell.specifiers,
            move_attributes: resolve_template_attributes(env, attributes),
            strong_identifier: Some(QualifiedName::new_raw(name.clone())),
            debug_name: Some(name),
            template_info: None,
            kind: MIRTypeKind::TaggedUnion { variants: vec![] },
        },
        _ => unreachable!("Templated named type must be an aggregate"),
    }
}

fn resolve_template_attributes(
    env: &TypeEnvironment,
    attributes: &CXStructAttributes,
) -> MIRMoveAttributes {
    let mut nocopy = attributes.nocopy || attributes.nodrop;
    let mut nodrop = attributes.nodrop;

    if let Some(param_name) = &attributes.copy_traits
        && let Some(resolved_type) = env.get_realized_type(param_name)
        && let Some(src_attrs) = resolved_type.struct_attributes()
    {
        nocopy = nocopy || src_attrs.nocopy;
        nodrop = nodrop || src_attrs.nodrop;
    }

    MIRMoveAttributes { nocopy, nodrop }
}

pub(crate) fn deduce_function_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    arg_types: &[MIRType],
) -> CXResult<MIRFunctionPrototype> {
    let completed_input = deduce_function_template_input(env, base_data, template, arg_types)?;
    instantiate_function_template_with_input(env, base_data, template, &completed_input)
}

pub(crate) fn deduce_function_template_input(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    arg_types: &[MIRType],
) -> CXResult<MIRTemplateInput> {
    let resource = &template.resource;
    let shell = &resource.shell;
    let template_prototype = &resource.prototype;
    let external_module = template.external_module.as_ref();
    let mut bindings = TemplateBindings::new();

    let param_arg_types = match &shell.kind {
        CXFunctionKind::Standard(_) | CXFunctionKind::StaticMemberFunction { .. } => arg_types,
        CXFunctionKind::MemberFunction { member_type, .. } => {
            let Some((receiver_type, param_arg_types)) = arg_types.split_first() else {
                return CXError::create_result(
                    "Member function template deduction requires a receiver argument",
                );
            };

            deduce_from_cx_type(
                env,
                base_data,
                external_module,
                template_prototype,
                &mut bindings,
                &member_type.as_type(),
                receiver_type,
            )?;

            param_arg_types
        }
    };

    if param_arg_types.len() != shell.params.len() && !shell.var_args {
        return CXError::create_result(format!(
            "Function template expects {} arguments, found {}",
            shell.params.len(),
            param_arg_types.len()
        ));
    }

    if param_arg_types.len() < shell.params.len() {
        return CXError::create_result(format!(
            "Function template expects at least {} arguments, found {}",
            shell.params.len(),
            param_arg_types.len()
        ));
    }

    for (param, actual_type) in shell.params.iter().zip(param_arg_types.iter()) {
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
    if let MIRTypeKind::MemoryReference { inner_type, .. } = &actual.kind
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
                .any(|param| param == name.name.as_str()) =>
        {
            bind_template_argument(env, bindings, name.name.as_str(), actual)
        }

        CXTypeKind::TemplatedIdentifier { name, input } => {
            let Some(template_info) = actual.get_template_data() else {
                return CXError::create_result(format!(
                    "Expected realized template type '{}' while deducing, found {}",
                    name,
                    actual.display_with(&env.symbols.context)
                ));
            };

            if template_info.base_name != name.name {
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

        CXTypeKind::MemoryReference { inner_type } => {
            let MIRTypeKind::MemoryReference {
                inner_type: actual_inner,
                ..
            } = &actual.kind
            else {
                return concrete_type_mismatch(env, formal, actual);
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

        CXTypeKind::ExplicitSizedArray(inner_type, _)
        | CXTypeKind::ImplicitSizedArray(inner_type)
        | CXTypeKind::PointerTo { inner_type, .. } => match &actual.kind {
            MIRTypeKind::PointerTo {
                inner_type: actual_inner,
            }
            | MIRTypeKind::MemoryReference {
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
            _ => concrete_type_mismatch(env, formal, actual),
        },

        CXTypeKind::FunctionPointer { prototype } => {
            let MIRTypeKind::Function {
                signature: actual_signature,
            } = &actual.kind
            else {
                return concrete_type_mismatch(env, formal, actual);
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
            let completed_formal = complete_type(
                env,
                base_data,
                external_module,
                &CXExpression::default(),
                formal,
            )?;
            if env.type_eq(&completed_formal, actual) {
                Ok(())
            } else {
                concrete_type_mismatch(env, formal, actual)
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
            name,
            existing.display_with(&env.symbols.context),
            actual.display_with(&env.symbols.context)
        ));
    }

    bindings.insert(name.to_string(), actual.clone());
    Ok(())
}

fn concrete_type_mismatch(
    env: &TypeEnvironment,
    formal: &CXType,
    actual: &MIRType,
) -> CXResult<()> {
    CXError::create_result(format!(
        "Template deduction mismatch: expected {}, found {}",
        formal,
        actual.display_with(&env.symbols.context)
    ))
}

pub(crate) fn instantiate_function_template(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    template: &ModuleResource<CXFunctionTemplate>,
    input: &CXTemplateInput,
) -> CXResult<MIRFunctionPrototype> {
    let completed_input = internal_complete_template_input(
        env,
        base_data,
        template.external_module.as_ref(),
        &CXExpression::default(),
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
    let module_origin = template
        .external_module
        .clone()
        .or_else(|| env.external_template_origin().cloned());
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
            module_origin,
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
