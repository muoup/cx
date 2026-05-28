use cx_ast::{
    ast::CXExpression,
    data::{CXFunctionKey, CXTemplateInput},
};
use cx_mir::mir::{
    data::{MIRFunctionPrototype, MIRType},
    name_mangling::{base_mangle_member, base_mangle_standard, base_mangle_static_member},
    program::MIRBaseMappings,
};
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName};

use crate::{
    environment::TypeEnvironment,
    environment::symbols::templates::{deduce_function_template, instantiate_function_template},
    log_typecheck_error,
};

enum FunctionResolution<'a> {
    Explicit(Option<&'a CXTemplateInput>),
    Deduced {
        owner_type: Option<&'a MIRType>,
        arg_types: &'a [MIRType],
    },
}

fn query_function_by_key(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    key: &CXFunctionKey,
    resolution: FunctionResolution<'_>,
) -> CXResult<Option<MIRFunctionPrototype>> {
    if let Some(standard) = base_data.fn_data.get_standard(key) {
        return env
            .complete_prototype(
                base_data,
                standard.external_module.as_ref(),
                &standard.resource,
            )
            .map(Some);
    }

    if let Some(template) = base_data.fn_data.get_template(key) {
        let prototype = match resolution {
            FunctionResolution::Explicit(Some(template_input)) => {
                instantiate_function_template(env, base_data, template, template_input)
            }
            FunctionResolution::Explicit(None) => {
                return log_typecheck_error!(
                    env,
                    expr.token_range(),
                    "Templated function '{}' requires explicit template arguments in this context",
                    key,
                );
            }
            FunctionResolution::Deduced {
                owner_type,
                arg_types,
            } => match deduce_function_template(env, base_data, template, owner_type, arg_types) {
                Ok(prototype) => Ok(prototype),
                Err(err) => {
                    log_typecheck_error!(env, expr.token_range(), "{}", err.error_content())
                }
            },
        }?;

        return Ok(Some(prototype));
    }

    Ok(None)
}

pub fn query_member_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    member_type: &MIRType,
    name: &CXIdent,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<Option<MIRFunctionPrototype>> {
    let Some(base_name) = member_type.get_base_identifier() else {
        return Ok(None);
    };

    if template_input.is_none() {
        let mangled_name = base_mangle_member(&env.symbols.context, name.as_str(), member_type);

        if let Some(func_proto) = env.get_realized_func(&mangled_name) {
            return Ok(Some(func_proto));
        }
    }

    let key = CXFunctionKey::MemberFunction {
        type_base_name: QualifiedName::new_raw(base_name.clone()),
        name: name.clone(),
    };

    query_function_by_key(
        env,
        base_data,
        expr,
        &key,
        FunctionResolution::Explicit(template_input),
    )
}

pub fn query_deduced_member_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    member_type: &MIRType,
    name: &CXIdent,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRFunctionPrototype>> {
    let Some(base_name) = member_type.get_base_identifier() else {
        return Ok(None);
    };

    let key = CXFunctionKey::MemberFunction {
        type_base_name: QualifiedName::new_raw(base_name.clone()),
        name: name.clone(),
    };

    query_function_by_key(
        env,
        base_data,
        expr,
        &key,
        FunctionResolution::Deduced {
            owner_type: Some(member_type),
            arg_types,
        },
    )
}

pub fn query_static_member_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    member_type: &MIRType,
    name: &CXIdent,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<Option<MIRFunctionPrototype>> {
    if template_input.is_none() {
        let mangled_name =
            base_mangle_static_member(&env.symbols.context, name.as_str(), member_type);

        if let Some(func_proto) = env.get_realized_func(&mangled_name) {
            return Ok(Some(func_proto));
        }
    }

    let Some(base_name) = member_type.get_base_identifier() else {
        return Ok(None);
    };

    let key = CXFunctionKey::StaticMemberFunction {
        type_base_name: QualifiedName::new_raw(base_name.clone()),
        name: name.clone(),
    };

    query_function_by_key(
        env,
        base_data,
        expr,
        &key,
        FunctionResolution::Explicit(template_input),
    )
}

pub fn query_deduced_static_member_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    member_type: &MIRType,
    name: &CXIdent,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRFunctionPrototype>> {
    let Some(base_name) = member_type.get_base_identifier() else {
        return Ok(None);
    };

    let key = CXFunctionKey::StaticMemberFunction {
        type_base_name: QualifiedName::new_raw(base_name.clone()),
        name: name.clone(),
    };

    query_function_by_key(
        env,
        base_data,
        expr,
        &key,
        FunctionResolution::Deduced {
            owner_type: Some(member_type),
            arg_types,
        },
    )
}

pub fn query_standard_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<Option<MIRFunctionPrototype>> {
    if template_input.is_none() && name.namespace.is_root() {
        let mangled_name = base_mangle_standard(name.name.as_str());

        if let Some(func_proto) = env.get_realized_func(&mangled_name) {
            return Ok(Some(func_proto));
        }
    }

    let key = CXFunctionKey::Standard(name.clone());

    query_function_by_key(
        env,
        base_data,
        expr,
        &key,
        FunctionResolution::Explicit(template_input),
    )
}

pub fn query_deduced_standard_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRFunctionPrototype>> {
    let key = CXFunctionKey::Standard(name.clone());

    query_function_by_key(
        env,
        base_data,
        expr,
        &key,
        FunctionResolution::Deduced {
            owner_type: None,
            arg_types,
        },
    )
}
