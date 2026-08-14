use std::collections::HashMap;

use cx_hir::ast::{
    function::{HIRComptimeFnPrototype, HIRFunctionPrototype},
    template::{HIRTemplateInput, HIRTemplatePrototype},
    types::{HIRType, HIRTypeKind},
};
use cx_hir::symbols::HIRSymbolKind;
use cx_log::{
    CXRawResult, CXResult,
    error::{CXMaybeRawErr, CXMaybeRawResult},
};
use cx_thir::{
    EnvironmentNamespace,
    thir::data::{THIRFnSignature, MIRTemplateInput, THIRType, THIRTypeKind},
    symbol::MIRSymbol,
    type_context::THIRTypeContext,
};
use cx_util::namespace::QualifiedName;

use crate::{
    environment::TypeEnvironment,
    symbol::{
        completion::{complete_template_input, complete_type},
        resolution::apply_template,
    },
};

type TemplateBindings = HashMap<String, THIRType>;

pub(crate) fn complete_templated_callee_maybe(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &QualifiedName,
    template_input: Option<&HIRTemplateInput>,
    arg_types: &[THIRType],
    expected_return_type: Option<&THIRType>,
) -> CXMaybeRawResult<MIRSymbol> {
    let Some(symbol) = env
        .get_symbol(namespace, name)
        .map_err(CXMaybeRawErr::from)?
    else {
        return crate::log::internal_type_error(format!("Templated function '{}' not found", name))
            .map_err(CXMaybeRawErr::from);
    };

    if let Some(input) = template_input {
        let completed_input = complete_template_input(env, namespace, input)?;
        return apply_template(env, &symbol, completed_input)?.ok_or_else(|| {
            CXMaybeRawErr::from(crate::log::type_error_msg(format!(
                "Symbol '{}' does not accept template arguments",
                name
            )))
        });
    }

    deduce_template_symbol(env, namespace, &symbol, arg_types, expected_return_type)?.ok_or_else(
        || {
            CXMaybeRawErr::from(crate::log::type_error_msg(format!(
                "Symbol '{}' is not a template",
                name
            )))
        },
    )
}

pub(crate) fn deduce_template_symbol(
    env: &mut TypeEnvironment,
    _namespace: &EnvironmentNamespace,
    symbol: &MIRSymbol,
    arg_types: &[THIRType],
    expected_return_type: Option<&THIRType>,
) -> CXMaybeRawResult<Option<MIRSymbol>> {
    let MIRSymbol::Template {
        template_prototype,
        source,
        namespace,
        ..
    } = symbol
    else {
        return Ok(None);
    };

    let completed_input = deduce_template_input(
        env,
        namespace,
        template_prototype,
        source,
        arg_types,
        expected_return_type,
    )?;
    apply_template(env, symbol, completed_input)
}

fn deduce_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    template_prototype: &HIRTemplatePrototype,
    source: &cx_hir::symbols::HIRSymbol,
    arg_types: &[THIRType],
    expected_return_type: Option<&THIRType>,
) -> CXMaybeRawResult<MIRTemplateInput> {
    let shell = match &source.kind {
        HIRSymbolKind::FunctionReference(shell) => TemplateDeductionShell::Runtime(shell),
        HIRSymbolKind::ComptimeFunction { definition, .. } => {
            TemplateDeductionShell::Comptime(definition)
        }
        HIRSymbolKind::TypeConstructor { union_type, .. } => {
            TemplateDeductionShell::TypeConstructor(union_type)
        }
        _ => {
            return crate::log::internal_type_error(
                "Only function template deduction is implemented",
            )
            .map_err(CXMaybeRawErr::from);
        }
    };

    let mut bindings = TemplateBindings::new();
    if arg_types.len() > shell.params_len() && !shell.var_args() {
        return crate::log::internal_type_error(format!(
            "Function template expects {} arguments, found {}",
            shell.params_len(),
            arg_types.len()
        ))
        .map_err(CXMaybeRawErr::from);
    }

    for (formal_type, actual_type) in shell.formal_types().into_iter().zip(arg_types.iter()) {
        deduce_from_cx_type(
            env,
            namespace,
            template_prototype,
            &mut bindings,
            formal_type,
            actual_type,
        )?;
    }

    if let Some(expected_return_type) = expected_return_type {
        deduce_from_cx_type(
            env,
            namespace,
            template_prototype,
            &mut bindings,
            shell.return_type(),
            expected_return_type,
        )?;
    }

    let args = template_prototype
        .types
        .iter()
        .map(|name| {
            let ty = bindings.remove(name.as_str()).ok_or_else(|| {
                CXMaybeRawErr::from(crate::log::type_error_msg(format!(
                    "Could not deduce template argument '{}' for function {}",
                    name,
                    shell.name()
                )))
            })?;

            Ok(env.symbols.generate_type_id(ty))
        })
        .collect::<CXMaybeRawResult<Vec<_>>>()?;

    Ok(MIRTemplateInput { args })
}

enum TemplateDeductionShell<'a> {
    Runtime(&'a HIRFunctionPrototype),
    Comptime(&'a HIRComptimeFnPrototype),
    TypeConstructor(&'a HIRType),
}

impl<'a> TemplateDeductionShell<'a> {
    fn params_len(&self) -> usize {
        match self {
            Self::Runtime(shell) => shell.params.len(),
            Self::Comptime(shell) => shell.params.len(),
            Self::TypeConstructor(_) => 0,
        }
    }

    fn var_args(&self) -> bool {
        match self {
            Self::Runtime(shell) => shell.var_args,
            Self::Comptime(_) => false,
            Self::TypeConstructor(_) => true,
        }
    }

    fn formal_types(&self) -> Vec<&'a HIRType> {
        match self {
            Self::Runtime(shell) => shell.params.iter().map(|param| &param._type).collect(),
            Self::Comptime(shell) => shell
                .params
                .iter()
                .map(|param| &param.value_type._type)
                .collect(),
            Self::TypeConstructor(_) => Vec::new(),
        }
    }

    fn return_type(&self) -> &'a HIRType {
        match self {
            Self::Runtime(shell) => &shell.return_type,
            Self::Comptime(shell) => &shell.return_type._type,
            Self::TypeConstructor(union_type) => union_type,
        }
    }

    fn name(&self) -> String {
        match self {
            Self::Runtime(shell) => shell.kind.to_string(),
            Self::Comptime(shell) => shell.kind.to_string(),
            Self::TypeConstructor(_) => "type constructor".to_string(),
        }
    }
}

fn deduce_from_cx_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    template_prototype: &HIRTemplatePrototype,
    bindings: &mut TemplateBindings,
    formal: &HIRType,
    actual: &THIRType,
) -> CXResult<()> {
    if let THIRTypeKind::MemoryReference { inner_type, .. } = &actual.kind
        && !matches!(formal.kind, HIRTypeKind::MemoryReference { .. })
    {
        let inner_type = env.symbols.resolve_type_id(*inner_type).clone();
        return deduce_from_cx_type(
            env,
            namespace,
            template_prototype,
            bindings,
            formal,
            &inner_type,
        );
    }

    match &formal.kind {
        HIRTypeKind::Identifier {
            name,
            template_input: None,
            ..
        } if template_prototype
            .types
            .iter()
            .any(|param| param.as_str() == name.name.as_str()) =>
        {
            bind_template_argument(env, bindings, name.name.as_str(), actual)
                .map_err(|err| env.complete_err(err, formal.range()))
        }

        HIRTypeKind::Identifier {
            name,
            template_input: Some(input),
            ..
        } => {
            let Some(template_info) = actual.get_template_data() else {
                return crate::log::internal_type_error(format!(
                    "Expected realized template type '{}' while deducing, found {}",
                    name,
                    actual.display_with(&env.symbols)
                ));
            };

            if !template_base_matches(name, template_info.base_name.as_ref()) {
                return crate::log::internal_type_error(format!(
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
                return crate::log::internal_type_error(format!(
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
                let actual_arg = env.symbols.resolve_type_id(*actual_arg).clone();
                deduce_from_cx_type(
                    env,
                    namespace,
                    template_prototype,
                    bindings,
                    formal_arg,
                    &actual_arg,
                )?;
            }

            Ok(())
        }

        HIRTypeKind::MemoryReference { inner_type } => {
            let THIRTypeKind::MemoryReference {
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

        HIRTypeKind::ExplicitSizedArray(inner_type, _)
        | HIRTypeKind::ImplicitSizedArray(inner_type)
        | HIRTypeKind::PointerTo { inner_type, .. } => match &actual.kind {
            THIRTypeKind::PointerTo {
                inner_type: actual_inner,
            }
            | THIRTypeKind::MemoryReference {
                inner_type: actual_inner,
                ..
            }
            | THIRTypeKind::Array {
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
            THIRTypeKind::Function { .. }
                if matches!(inner_type.kind, HIRTypeKind::FunctionPointer { .. }) =>
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

        HIRTypeKind::FunctionPointer { prototype } => {
            let actual_signature = match &actual.kind {
                THIRTypeKind::Function { signature } => signature.as_ref().clone(),
                THIRTypeKind::PointerTo { inner_type } => {
                    let inner = env.symbols.resolve_type_id(*inner_type);
                    let THIRTypeKind::Function { signature } = &inner.kind else {
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
    template_prototype: &HIRTemplatePrototype,
    bindings: &mut TemplateBindings,
    formal: &HIRFunctionPrototype,
    actual: &THIRFnSignature,
) -> CXResult<()> {
    if formal.var_args != actual.var_args {
        return crate::log::internal_type_error(format!(
            "Function pointer varargs mismatch during template deduction: expected {}, found {}",
            formal.var_args, actual.var_args
        ));
    }

    if formal.params.len() != actual.params.len() {
        return crate::log::internal_type_error(format!(
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
    actual: &THIRType,
) -> CXRawResult<()> {
    if let Some(existing) = bindings.get(name) {
        if env.type_eq(existing, actual) {
            return Ok(());
        }

        return env.log_error_base(format!(
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
    formal: &HIRType,
    actual: &THIRType,
) -> CXResult<()> {
    crate::log::internal_type_error(format!(
        "Template deduction mismatch: expected {}, found {}",
        formal,
        actual.display_with(&env.symbols)
    ))
}
