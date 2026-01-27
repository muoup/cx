use cx_ast::{ast::CXExpr, data::{CXFunctionKey, CXTemplateInput}};
use cx_mir::mir::{
    name_mangling::{base_mangle_deconstructor, base_mangle_destructor, base_mangle_member, base_mangle_standard, base_mangle_static_member}, program::MIRBaseMappings, types::{MIRFunctionPrototype, MIRType}
};
use cx_util::{CXResult, identifier::CXIdent};

use crate::{
    environment::TypeEnvironment, log_typecheck_error,
    type_completion::templates::instantiate_function_template,
};

pub(crate) fn deduce_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    key: &CXFunctionKey,
    templated_input: Option<&CXTemplateInput>,
) -> CXResult<MIRFunctionPrototype> {
    if let Some(standard) = base_data.fn_data.get_standard(key) {
        return env.complete_prototype(base_data, standard.external_module.as_ref(), &standard.resource);
    }
    
    if let Some(template) = base_data.fn_data.get_template(key) {
        let Some(templated_input) = templated_input else {
            return log_typecheck_error!(
                env,
                expr,
                "Template argument deduction not yet implemented",
            );
        };
        
        return instantiate_function_template(env, base_data, template, templated_input);
    }
    
    log_typecheck_error!(
        env,
        expr,
        "Function with key {} not found in base mappings",
        key,
    )
}

pub fn query_member_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    member_type: &MIRType,
    name: &CXIdent,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<MIRFunctionPrototype> {
    let Some(base_name) = member_type.get_base_identifier() else {
        return log_typecheck_error!(
            env,
            expr,
            "Cannot query member function '{}' on non-identifiable type '{}'",
            name,
            member_type,
        );
    };

    if template_input.is_none() {
        let mangled_name = base_mangle_member(name.as_str(), member_type);

        if let Some(func_proto) = env.get_realized_func(&mangled_name) {
            return Ok(func_proto);
        }
    }

    let key = CXFunctionKey::MemberFunction {
        type_base_name: base_name.clone(),
        name: name.clone(),
    };

    deduce_function(env, base_data, expr, &key, template_input)
}

pub fn query_static_member_function(
    env: &mut TypeEnvironment,
    _base_data: &MIRBaseMappings,
    expr: &CXExpr,
    member_type: &MIRType,
    name: &CXIdent,
) -> CXResult<MIRFunctionPrototype> {
    let mangled_name = base_mangle_static_member(name.as_str(), member_type);

    if let Some(func_proto) = env.get_realized_func(&mangled_name) {
        return Ok(func_proto);
    }

    // Note: We can't call deduce_function here because member_type.get_template_data()
    // returns MIRTemplateInput (completed), but deduce_function expects CXTemplateInput (raw).
    // If the function isn't already realized, we return an error.
    log_typecheck_error!(
        env,
        expr,
        "Static member function '{}' not found for type '{}'",
        name,
        member_type,
    )
}

pub fn query_destructor(
    env: &mut TypeEnvironment,
    _base_data: &MIRBaseMappings,
    member_type: &MIRType,
) -> Option<MIRFunctionPrototype> {
    let mangled_name = base_mangle_destructor(member_type);

    if let Some(func_proto) = env.get_realized_func(&mangled_name) {
        return Some(func_proto);
    }

    // Note: We can't call deduce_function here because member_type.get_template_data()
    // returns MIRTemplateInput (completed), but deduce_function expects CXTemplateInput (raw).
    // If the destructor isn't already realized, we return None.
    None
}

pub fn query_deconstructor(
    env: &mut TypeEnvironment,
    member_type: &MIRType,
) -> Option<MIRFunctionPrototype> {
    let mangled_name = base_mangle_deconstructor(member_type);
    env.get_realized_func(&mangled_name)
}

pub fn query_standard_function(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    name: &CXIdent,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<MIRFunctionPrototype> {
    if template_input.is_none() {
        let mangled_name = base_mangle_standard(name.as_str());

        if let Some(func_proto) = env.get_realized_func(&mangled_name) {
            return Ok(func_proto);
        }
    }

    let key = CXFunctionKey::Standard(name.clone());

    deduce_function(env, base_data, expr, &key, template_input)
}
