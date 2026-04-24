use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        globals::global_expr, result::TypecheckResult, value::locals::ensure_binding_available,
    },
};
use cx_ast::{ast::CXExpression, data::CXTemplateInput};
use cx_mir::mir::{
    data::{MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    name_mangling::base_mangle_standard,
    program::MIRBaseMappings,
};
use cx_util::{CXResult, identifier::CXIdent};

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &CXIdent,
) -> CXResult<TypecheckResult> {
    if let Some(symbol_val) = env.function.symbol_value(name.as_str()) {
        let symbol_val = symbol_val.clone();
        ensure_binding_available(env, expr, name)?;
        Ok(TypecheckResult::from(symbol_val))
    } else if let Some(function_type) = env
        .get_realized_func(&base_mangle_standard(name.as_str()))
        .map(Ok)
        .or_else(|| {
            let key = cx_ast::data::CXFunctionKey::Standard(name.clone());
            if base_data.fn_data.is_key_any(&key) {
                Some(env.get_standard_function(base_data, expr, name, None))
            } else {
                None
            }
        })
        .transpose()?
    {
        Ok(TypecheckResult::from(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::FunctionReference {
                name: function_type.name.clone(),
            },
            _type: MIRType::from(MIRTypeKind::Function {
                signature: Box::new(function_type.signature()),
            }),
        }))
    } else if env.function.in_safe_context()
        && base_data.global_variables.contains_key(name.as_str())
    {
        log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Safe functions may not access global variables"
        )
    } else if let Ok(global) = global_expr(env, base_data, name.as_str()) {
        Ok(TypecheckResult::from(global))
    } else {
        log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' not found",
            name
        )
    }
}

pub(crate) fn typecheck_templated_identifier(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &CXIdent,
    template_input: &CXTemplateInput,
) -> CXResult<TypecheckResult> {
    let function = env.get_standard_function(base_data, expr, name, Some(template_input))?;

    Ok(TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::FunctionReference {
            name: function.name.clone(),
        },
        _type: MIRType::from(MIRTypeKind::Function {
            signature: Box::new(function.signature()),
        }),
    }))
}
