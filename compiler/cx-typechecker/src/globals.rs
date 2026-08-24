use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::modifiers::{HIR_CONST, HIRSymbolNameScheme};
use cx_hir::ast::types::{HIRType, HIRTypeKind};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::THIRType;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_thir::type_context::THIRTypeContext;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;
use cx_util::namespace::QualifiedName;

use crate::environment::TypeEnvironment;
use crate::symbol::completion::{
    complete_type, completed_symbol_name, ensure_valid_type_component,
};
use crate::type_checking::typechecker::typecheck_expr;

pub(crate) fn lower_global(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: CXIdent,
    hir_type: &HIRType,
    linkage: LinkageMode,
    name_scheme: HIRSymbolNameScheme,
    initializer: Option<&HIRExpression>,
) -> CXResult<()> {
    let is_inferred_array = matches!(hir_type.kind, HIRTypeKind::ImplicitSizedArray(_));
    let mut _type = complete_type(env, &namespace, hir_type)?;
    ensure_valid_type_component(env, hir_type.range(), &_type, "a global variable", true)?;

    let symbol_name = completed_symbol_name(
        env,
        QualifiedName::new(namespace.clone(), name.clone()),
        name_scheme,
    );
    let previous_type = env
        .items
        .generated_global(&symbol_name)
        .map(|global| global._type.clone());

    if let Some(previous_type) = &previous_type
        && env.type_eq(previous_type, &_type)
    {
        _type = previous_type.clone();
    }

    let (global_type, comptime_init) = initializer
        .as_ref()
        .map(|init| {
            let expression = typecheck_expr(env, &namespace, init, Some(&_type))
                .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))?;
            let (global_type, expression) = match &expression.kind {
                THIRExpressionKind::TypeConversion {
                    conversion: THIRCoercion::ReinterpretBits,
                    operand,
                } if is_inferred_array
                    && matches!(operand.kind, THIRExpressionKind::ArrayInitializer { .. }) =>
                {
                    (operand._type.clone(), operand.as_ref().clone())
                }
                THIRExpressionKind::ArrayInitializer { .. } if is_inferred_array => {
                    (expression._type.clone(), expression)
                }
                THIRExpressionKind::TypeConversion {
                    conversion: THIRCoercion::ReinterpretBits,
                    operand,
                } if matches!(operand.kind, THIRExpressionKind::GlobalVariable { .. }) => {
                    (_type.clone(), expression)
                }
                _ => (_type.clone(), expression),
            };
            Ok((global_type, Some(expression)))
        })
        .transpose()?
        .unwrap_or_else(|| (_type.clone(), None));
    let global_type = if let Some(previous_type) = previous_type {
        if env.type_eq(&previous_type, &global_type) {
            previous_type
        } else if is_inferred_array
            && incomplete_array_declaration_compatible(env, &previous_type, &global_type)
        {
            global_type
        } else {
            return env.log_error(
                hir_type.range(),
                format!(
                    "Attempting to redeclare global '{}' with a different type.",
                    name
                ),
            );
        }
    } else {
        global_type
    };

    if !env.type_eq(&_type, &global_type) {
        let global_value_type = env.symbols.mem_ref_to(global_type.clone());
        env.symbols.insert_value(
            QualifiedName::new(namespace.clone(), name.clone()),
            THIRExpression {
                token_range: cx_tokens::TokenRange::internal(),
                kind: THIRExpressionKind::GlobalVariable {
                    symbol: CXIdent::new(symbol_name.clone()),
                },
                _type: global_value_type,
            },
        );
    }

    let global = THIRGlobalVariable {
        name: CXIdent::new(symbol_name),
        _type: global_type,

        is_mutable: _type.get_specifier(HIR_CONST),
        initializer: comptime_init,

        linkage,
    };

    env.items.push_generated_global(global, true);
    Ok(())
}

fn incomplete_array_declaration_compatible(
    env: &TypeEnvironment,
    declaration: &THIRType,
    definition: &THIRType,
) -> bool {
    let Some(declaration_inner) = env.symbols.ptr_inner(declaration) else {
        return false;
    };
    let Some(definition_inner) = env.symbols.array_inner(definition) else {
        return false;
    };
    env.type_eq(declaration_inner, definition_inner)
}
