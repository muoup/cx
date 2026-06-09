use std::collections::HashMap;

use cx_ast::ast::{
    function::{CXFunctionKind, CXFunctionPrototype},
    template::{CXTemplateInput, CXTemplatePrototype},
    types::{CXType, CXTypeKind},
};
use cx_ast::symbols::CXSymbolKind;
use cx_mir::{
    EnvironmentNamespace,
    mir::data::{MIRFunctionSignature, MIRTemplateInput, MIRType, MIRTypeKind},
    symbol::MIRSymbol,
    type_context::MIRTypeContext,
};
use cx_util::{CXError, CXResult, namespace::QualifiedName};

use crate::{
    environment::TypeEnvironment,
    symbol::{
        completion::{complete_template_input, complete_type},
        resolution::apply_template,
    },
};

type TemplateBindings = HashMap<String, MIRType>;

pub(crate) fn complete_templated_callee(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
    arg_types: &[MIRType],
) -> CXResult<MIRSymbol> {
    let Some(symbol) = env.get_symbol(namespace, name)? else {
        return CXError::create_result(format!("Templated function '{}' not found", name));
    };

    if let Some(input) = template_input {
        let completed_input = complete_template_input(env, namespace, input)?;
        return apply_template(env, &symbol, completed_input)?.ok_or_else(|| {
            CXError::create_boxed(format!("Symbol '{}' does not accept template arguments", name))
        });
    }

    deduce_template_symbol(env, namespace, &symbol, arg_types)?.ok_or_else(|| {
        CXError::create_boxed(format!("Symbol '{}' is not a template", name))
    })
}

pub(crate) fn deduce_template_symbol(
    env: &mut TypeEnvironment,
    _namespace: &EnvironmentNamespace,
    symbol: &MIRSymbol,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRSymbol>> {
    let MIRSymbol::Template {
        template_prototype,
        source,
        namespace,
        ..
    } = symbol
    else {
        return Ok(None);
    };

    let completed_input =
        deduce_template_input(env, namespace, template_prototype, source, arg_types)?;
    apply_template(env, symbol, completed_input)
}

fn deduce_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    template_prototype: &CXTemplatePrototype,
    source: &cx_ast::symbols::CXSymbol,
    arg_types: &[MIRType],
) -> CXResult<MIRTemplateInput> {
    let CXSymbolKind::FunctionReference(shell) = &source.kind else {
        return CXError::create_result("Only function template deduction is implemented");
    };

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
                namespace,
                template_prototype,
                &mut bindings,
                &member_type.as_type(),
                receiver_type,
            )?;

            param_arg_types
        }
    };

    if param_arg_types.len() > shell.params.len() && !shell.var_args {
        return CXError::create_result(format!(
            "Function template expects {} arguments, found {}",
            shell.params.len(),
            param_arg_types.len()
        ));
    }

    for (param, actual_type) in shell.params.iter().zip(param_arg_types.iter()) {
        deduce_from_cx_type(
            env,
            namespace,
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
            bindings.remove(name.as_str()).ok_or_else(|| {
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
    namespace: &EnvironmentNamespace,
    template_prototype: &CXTemplatePrototype,
    bindings: &mut TemplateBindings,
    formal: &CXType,
    actual: &MIRType,
) -> CXResult<()> {
    if let MIRTypeKind::MemoryReference { inner_type, .. } = &actual.kind
        && !matches!(formal.kind, CXTypeKind::MemoryReference { .. })
    {
        let inner_type = env.symbols.resolve_type_id(*inner_type).clone();
        if !inner_type.is_nocopy() {
            return deduce_from_cx_type(
                env,
                namespace,
                template_prototype,
                bindings,
                formal,
                &inner_type,
            );
        }
    }

    match &formal.kind {
        CXTypeKind::Identifier {
            name,
            template_input: None,
            ..
        } if template_prototype
            .types
            .iter()
            .any(|param| param.as_str() == name.name.as_str()) =>
        {
            bind_template_argument(env, bindings, name.name.as_str(), actual)
        }

        CXTypeKind::Identifier {
            name,
            template_input: Some(input),
            ..
        } => {
            let Some(template_info) = actual.get_template_data() else {
                return CXError::create_result(format!(
                    "Expected realized template type '{}' while deducing, found {}",
                    name,
                    actual.display_with(&env.symbols)
                ));
            };

            if !template_base_matches(name, template_info.base_name.as_ref()) {
                return CXError::create_result(format!(
                    "Expected template type '{}', found '{}'",
                    name,
                    template_info
                        .base_name
                        .as_ref()
                        .map(|name| name.to_string())
                        .unwrap_or_else(|| "<anonymous>".to_string())
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
                    namespace,
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

            let actual_inner = env.symbols.resolve_type_id(*actual_inner).clone();
            deduce_from_cx_type(
                env,
                namespace,
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
            }
            | MIRTypeKind::Array {
                inner_type: actual_inner,
                ..
            } => {
                let actual_inner = env.symbols.resolve_type_id(*actual_inner).clone();
                deduce_from_cx_type(
                    env,
                    namespace,
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
                    namespace,
                    template_prototype,
                    bindings,
                    inner_type,
                    actual,
                )
            }
            _ => concrete_type_mismatch(env, formal, actual),
        },

        CXTypeKind::FunctionPointer { prototype } => {
            let actual_signature = match &actual.kind {
                MIRTypeKind::Function { signature } => signature.as_ref().clone(),
                MIRTypeKind::PointerTo { inner_type } => {
                    let inner = env.symbols.resolve_type_id(*inner_type);
                    let MIRTypeKind::Function { signature } = &inner.kind else {
                        return concrete_type_mismatch(env, formal, actual);
                    };
                    signature.as_ref().clone()
                }
                _ => return concrete_type_mismatch(env, formal, actual),
            };

            deduce_from_function_signature(
                env,
                namespace,
                template_prototype,
                bindings,
                prototype,
                &actual_signature,
            )
        }

        _ => {
            let completed_formal = complete_type(env, namespace, formal)?;
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
    namespace: &EnvironmentNamespace,
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
        namespace,
        template_prototype,
        bindings,
        &formal.return_type,
        &actual.return_type,
    )?;

    for (formal_param, actual_param) in formal.params.iter().zip(actual.params.iter()) {
        deduce_from_cx_type(
            env,
            namespace,
            template_prototype,
            bindings,
            &formal_param._type,
            &actual_param._type,
        )?;
    }

    Ok(())
}

fn bind_template_argument(
    env: &TypeEnvironment,
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
            existing.display_with(&env.symbols),
            actual.display_with(&env.symbols)
        ));
    }

    bindings.insert(name.to_string(), actual.clone());
    Ok(())
}

fn template_base_matches(formal: &QualifiedName, actual: Option<&QualifiedName>) -> bool {
    actual
        .map(|actual| {
            actual == formal || (formal.namespace.is_root() && actual.name == formal.name)
        })
        .unwrap_or(false)
}

fn concrete_type_mismatch(
    env: &TypeEnvironment,
    formal: &CXType,
    actual: &MIRType,
) -> CXResult<()> {
    CXError::create_result(format!(
        "Template deduction mismatch: expected {}, found {}",
        formal,
        actual.display_with(&env.symbols)
    ))
}
