use crate::{
    environment::{
        TypeEnvironment,
        symbols::{ResolvedValueSymbol, SymbolValueOrigin},
    },
    log_typecheck_error,
    type_checking::result::{TypecheckResult, TypecheckedBinding},
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
    if let Some(symbol) = env.symbols.resolve_value_symbol(name.as_str()) {
        resolved_symbol_to_typecheck_result(env, expr, name, symbol)
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
        env.symbols
            .insert_function_symbol(name.clone(), function_type.clone());
        Ok(TypecheckResult::from(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::FunctionReference {
                name: function_type.name.clone(),
            },
            _type: MIRType::from(MIRTypeKind::Function {
                signature: Box::new(function_type.signature()),
            }),
        }))
    } else {
        log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' not found",
            name
        )
    }
}

fn resolved_symbol_to_typecheck_result(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    name: &CXIdent,
    symbol: ResolvedValueSymbol,
) -> CXResult<TypecheckResult> {
    match symbol {
        ResolvedValueSymbol::PureValue(value) => Ok(TypecheckResult::from(value.as_value())),
        ResolvedValueSymbol::Value { value, origin } => {
            if origin == Some(SymbolValueOrigin::Global) && env.function.in_safe_context() {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Safe functions may not access global variables"
                );
            }

            let result = TypecheckResult::from(value);
            if matches!(
                origin,
                Some(SymbolValueOrigin::Local | SymbolValueOrigin::Contract)
            ) {
                Ok(result.with_binding(TypecheckedBinding::local(name.clone())))
            } else {
                Ok(result)
            }
        }
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
