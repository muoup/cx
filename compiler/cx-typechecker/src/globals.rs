use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::modifiers::{HIR_CONST, HIRSymbolNameScheme};
use cx_hir::ast::types::HIRType;
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::global::THIRGlobalVariable;
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
    let _type = complete_type(env, &namespace, hir_type)?;
    ensure_valid_type_component(env, hir_type.range(), &_type, "a global variable", true)?;

    let symbol_name = completed_symbol_name(
        env,
        QualifiedName::new(namespace.clone(), name.clone()),
        name_scheme,
    );

    let (global_type, comptime_init) = initializer
        .as_ref()
        .map(|init| {
            let expression = typecheck_expr(env, &namespace, init, Some(&_type))
                .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))?;
            let (global_type, expression) = match &expression.kind {
                THIRExpressionKind::TypeConversion {
                    conversion: THIRCoercion::ReinterpretBits,
                    operand,
                } if matches!(operand.kind, THIRExpressionKind::ArrayInitializer { .. }) => {
                    (operand._type.clone(), operand.as_ref().clone())
                }
                THIRExpressionKind::ArrayInitializer { .. } => {
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

    env.items.push_generated_global(global);
    Ok(())
}
