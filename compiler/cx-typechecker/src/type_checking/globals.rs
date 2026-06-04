use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        constexpr::constexpr_evaluate, typechecker::typecheck_expr,
        value::ensure_valid_allocation_type,
    },
    typecheck_error,
};
use cx_ast::ast::{expression::CXExprKind, global_var::CXGlobalVariable};
use cx_mir::mir::{
    data::MIRIntegerType,
    expression::{MIRExpression, MIRExpressionKind, MIRPureExpression, SymbolValueOrigin},
    program::{EnvironmentNamespace, MIRGlobalVarKind, MIRGlobalVariable},
};
use cx_mir::registry::MIRSymbolRegistry;
use cx_mir::symbol::completion::complete_type;
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName};

pub(crate) fn typecheck_global(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ident: &str,
    global: &CXGlobalVariable,
) -> CXResult<()> {
    match &global {
        CXGlobalVariable::EnumDefinition { variants } => {
            let mut previous = None;

            for variant in variants {
                let value = if let Some(expr) = variant.value.as_ref() {
                    typecheck_expr(env, namespace, expr, None)
                        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                        .and_then(|v| constexpr_evaluate(env, v))
                        .and_then(|v| {
                            v.get_integer().ok_or_else(|| {
                                typecheck_error!(
                                    env,
                                    Some(expr.token_range()),
                                    "Enum variant initializers must be integer literals"
                                )
                            })
                        })?
                } else {
                    previous.map(|value| value + 1).unwrap_or(0)
                };

                previous = Some(value);
                env.symbols.insert_pure_value(
                    QualifiedName::new_raw(variant.name.clone()),
                    enum_literal(value),
                );
            }

            Ok(())
        }

        CXGlobalVariable::Standard {
            _type,
            initializer,
            linkage,
            is_mutable,
        } => {
            let _type = complete_type(&mut env.symbols, namespace, _type)?;
            ensure_valid_allocation_type(env, None, "a global variable", &_type)?;
            let _initializer = match initializer.as_ref() {
                Some(init_expr) => {
                    let CXExprKind::IntLiteral { val, .. } = &init_expr.kind else {
                        return log_typecheck_error!(
                            env,
                            Some(init_expr.token_range()),
                            "CX currently only supports integer initializers for global variables"
                        );
                    };

                    Some(*val)
                }

                None => None,
            };

            env.items.realized_globals.insert(
                ident.to_string(),
                MIRGlobalVariable {
                    kind: MIRGlobalVarKind::Variable {
                        name: CXIdent::new(ident.to_string()),
                        initializer: _initializer,
                        _type,
                    },
                    is_mutable: *is_mutable,
                    linkage: *linkage,
                },
            );

            let expr = tcglobal_expr(
                env.items.realized_globals.get(ident).unwrap(),
                &mut env.symbols,
            )?;
            env.symbols
                .insert_value(QualifiedName::new_raw(CXIdent::new(ident)), expr);
            Ok(())
        }
    }
}

fn enum_literal(value: i64) -> MIRPureExpression {
    MIRPureExpression::IntegerLiteral(value, MIRIntegerType::from_bytes(8).unwrap(), true)
}

fn tcglobal_expr(
    global: &MIRGlobalVariable,
    definitions: &mut MIRSymbolRegistry,
) -> CXResult<MIRExpression> {
    match &global.kind {
        MIRGlobalVarKind::Variable { name, _type, .. } => Ok(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::Variable {
                name: name.clone(),
                location: SymbolValueOrigin::Global,
            },
            _type: definitions.mem_ref_to(_type.clone()),
        }),

        MIRGlobalVarKind::StringLiteral { .. } => {
            unreachable!("String literals cannot be referenced via an identifier")
        }
    }
}
