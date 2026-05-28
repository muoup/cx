use cx_ast::{
    ast::CXExpression,
    data::{member_function_key, CXTemplateInput},
};
use cx_mir::mir::{
    data::{MIRFunctionPrototype, MIRType},
    name_mangling::base_mangle_standard,
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

fn query_function_with_resolution(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    resolution: FunctionResolution<'_>,
) -> CXResult<Option<MIRFunctionPrototype>> {
    if let Some(standard) = base_data.fn_data.get_standard(name) {
        return env
            .complete_prototype(
                base_data,
                standard.external_module.as_ref(),
                &standard.resource,
            )
            .map(Some);
    }

    if let Some(template) = base_data.fn_data.get_template(name) {
        let prototype = match resolution {
            FunctionResolution::Explicit(Some(template_input)) => {
                instantiate_function_template(env, base_data, template, template_input)
            }
            FunctionResolution::Explicit(None) => {
                return log_typecheck_error!(
                    env,
                    expr.token_range(),
                    "Templated function '{}' requires explicit template arguments in this context",
                    name,
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

pub fn type_member_function_name(member_type: &MIRType, name: &CXIdent) -> Option<QualifiedName> {
    member_type
        .get_base_identifier()
        .map(|base_name| member_function_key(&QualifiedName::new_raw(base_name.clone()), name))
}

pub fn query_function(
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

    query_function_with_resolution(
        env,
        base_data,
        expr,
        name,
        FunctionResolution::Explicit(template_input),
    )
}

pub fn query_deduced_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    owner_type: Option<&MIRType>,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRFunctionPrototype>> {
    query_function_with_resolution(
        env,
        base_data,
        expr,
        name,
        FunctionResolution::Deduced {
            owner_type,
            arg_types,
        },
    )
}
